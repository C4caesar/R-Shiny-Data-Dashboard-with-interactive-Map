library(shiny)          # web app framework 
library(shinyjs)        # improve user experience with JavaScript
library(shinydashboard) # dashboard layout for shiny
library(shinythemes)    # themes for shiny

library(sqldf)          # run SQL query on a Data frame
library(summarytools)   # data summary
library(scales)         # plot formatting scale
library(tidyverse)      # data manipulation
library(cluster)        # clustering algorithms
library(factoextra)     # clustering algorithms & visualization
library(Rtsne)
library(ggplot2)
library(dplyr)
library(magrittr)
library(ggrepel)

library(htmltools)
library(shiny)
library(leaflet)

library(jsonlite)

library(rgdal)
library(sf)
library(RColorBrewer)

cur_timezone <- Sys.timezone(location = TRUE)



shape <- readOGR("building.shp")
print(shape$building)


studentdata <- read.csv("focusGroupStudent/df_allStudent.csv", header = TRUE)
studentdata$fp <- NULL
studentdata$Datetime = as.POSIXct(studentdata$Time, format = "%m/%d/%Y %H:%M:%S")

# create unique building data.frame
bb <- unique(studentdata$building)
all_building <- data.frame(bb)
all_building <- all_building[order(all_building$bb),]
all_building <- data.frame(all_building)
names(all_building) <- c('filtered_data$building')



ui <- fluidPage(
  sliderInput(inputId = "slider_datetime", 
              label = "Date & time:", 
              #min = min(studentdata$Datetime), max = max(studentdata$Datetime),
              min = as.POSIXct("2019-05-01 00:00:00", cur_timezone), max = as.POSIXct("2019-10-31 00:00:00", cur_timezone),
              value= c(as.POSIXct("2019-05-01 00:00:00", cur_timezone),as.POSIXct("2019-06-01 00:00:00", cur_timezone)),
              animate = animationOptions(interval = 1000)
  ),
  p(),
  leafletOutput("mymap", height = "1000px"),
  absolutePanel(top = 10, right = 10,
                
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div"))), selected = "Reds" # predefined selected = Reds 
                )
  )
  
  
)

server <- function(input, output, session) {
  
  #points <- eventReactive(input$recalc, {
  #cbind(rnorm(100) + 50, rnorm(100) + 11)
  #}, ignoreNULL = FALSE)
  
  # Static Map, with tiles, center view point
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate="https://api.hkmapservice.gov.hk/osm/xyz/basemap/WGS84/tile/{z}/{x}/{y}.png?key=584b2fa686f14ba283874318b3b8d6b0")%>%
      addTiles(urlTemplate="https://api.hkmapservice.gov.hk/osm/xyz/label-tc/WGS84/tile/{z}/{x}/{y}.png?key=584b2fa686f14ba283874318b3b8d6b0")%>%
      clearShapes() %>%
      setView(114.179747, 22.304247, zoom = 18)
    
  })
  # reactive variables
  cbb <- reactive({
    # count record in each buildings
    filtered_data <- studentdata %>% filter(Datetime >= input$slider_datetime[1] & Datetime <= input$slider_datetime[2])
    count_building <- dplyr::count(filtered_data, filtered_data$building, sort = TRUE)
    count_building$`filtered_data$building` <- factor(count_building$`filtered_data$building`, levels = count_building$`filtered_data$building`[order(count_building$n, decreasing = TRUE)])
    count_building = cbind(count_building, count_building$`filtered_data$building`)
    count_building <- all_building %>% left_join(count_building)
    # count_nMAC in each buildings
    studentdata222 <- sqldf("select building as 'filtered_data$building', count(distinct(nMAC)) As count_nMAC from filtered_data group by building order by building")
    count_building <- count_building %>% left_join(studentdata222)
    count_building[is.na(count_building)] <- 0
    names(count_building) <- c("filtered_data$building", "n", "dd", "count_nMAC")
    count_building$dd <- NULL
    count_building <- count_building[order(match(count_building$`filtered_data$building`, shape$building)),]
    count_building = cbind(count_building, count_building$`filtered_data$building`)
    
    return(count_building)
  })
  
  # changing content depends on clicked polygon (building) in map
  observeEvent( input$mymap_shape_click, {
    
    print(input$mymap_shape_click$id)
    
  })
  
  observe({
    
    count_max <- max(cbb()$n)
    lenOut <- 11
    if(count_max > 10){
      count_max <- signif(count_max,1)
    }else{
      lenOut <- count_max
    }
    if(count_max < 2){
      lenOut <- 2
      count_max <- 2
    }
    bins <- seq(from = 0, to = count_max,length.out = lenOut) # length.out default = 11
    bins <- c(bins, Inf)
    pal <- colorBin(input$colors, domain = cbb()$n, bins = bins)
    labels <- paste("<p>", cbb()$`filtered_data$building`, "</p>",
                    "<p>", "Name : ", shape$nameOfBuil, "</p>",
                    "<p>", "Count : ", cbb()$n, "</p>",
                    "<p>", "Unique nMAC : ", cbb()$count_nMAC, "</p>",
                    "<p>", "Average record : ", cbb()$n / cbb()$count_nMAC, "</p>",sep="")
    
    
    leafletProxy("mymap", data = cbb()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = shape, color = "grey", weight=2, smoothFactor = 2, 
                  layerId = cbb()$`filtered_data$building`, 
                  fillOpacity = 0.8, fillColor = pal(cbb()$n),
                  highlightOptions = highlightOptions(color = "red", weight = 4,
                                                      bringToFront = TRUE),
                  options = pathOptions(clickable = TRUE),
                  label = lapply(labels, HTML)
      ) %>%
      addLegend(pal = pal, values = cbb()$n, opacity = 0.7, position="topright")
  })
  
  
}

shinyApp(ui, server)