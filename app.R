library(shiny)          # web app framework 
library(shinyjs)        # improve user experience with JavaScript
library(shinydashboard) # dashboard layout for shiny
library(shinythemes)    # themes for shiny
library(shinycssloaders)

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
library(plotly)

library(RColorBrewer)
library(rgdal)          # Read shapefile
library(leaflet)
library(htmltools)

# ====================================
# ===       Global Variables       ===
# ====================================
cur_timezone <- Sys.timezone(location = TRUE)
shape <- readOGR("building.shp")
#print(shape$building)
studentdata <- read.csv("focusGroupStudent/df_allStudent.csv", header = TRUE)
studentdata$fp <- NULL
studentdata$Datetime = as.POSIXct(studentdata$Time, format = "%m/%d/%Y %H:%M:%S")

# create unique building data.frame
bb <- unique(studentdata$building)
all_building <- data.frame(bb)
all_building <- all_building[order(all_building$bb),]
all_building <- data.frame(all_building)
names(all_building) <- c('filtered_data$building')


# ====================================
# ===              UI              ===
# ====================================
ui <- dashboardPage(
  dashboardHeader(
    title = "CrowdViewer"
  ),
  
  dashboardSidebar(collapsed = TRUE, 
                   # set welcome HTML
                   div(htmlOutput("welcome"), style = "padding: 20px"),
                   sidebarMenu(id="sidebar001",
                               #menuItem("Introduction", tabName = "tab_0", icon = icon('lightbulb')), # sample front page
                               menuItem("Clustering", tabName = "tab_1", icon = icon('user-friends')),
                               #menuItem("Crowd View", tabName = "tab_2", icon = icon('school')), # sample 3d map
                               menuItem("Data Dashboard (6 months)", tabName = "tab_3", icon = icon("dashboard")),
                               menuItem("Data Dashboard (1 day)", tabName = "tab_4", icon = icon('calendar-day'))
                   )
  ),  
  
  dashboardBody(
    # define tabItems with separated UI R script
    tabItems(
      #tabItem(tabName = "tab_0", source(file.path("ui", "tab0.R"),  local = TRUE)$value), # sample front page
      tabItem(tabName = "tab_1", source(file.path("ui", "tab1.R"),  local = TRUE)$value), 
      #tabItem(tabName = "tab_2", source(file.path("ui", "tab2.R"),  local = TRUE)$value), # sample 3d map
      tabItem(tabName = "tab_3", source(file.path("ui", "tab3.R"),  local = TRUE)$value),
      tabItem(tabName = "tab_4", source(file.path("ui", "tab4.R"),  local = TRUE)$value)
    ),
    # enable shinyjs
    shinyjs::useShinyjs(),
    # tag header, script, css, etc.
    tags$head(tags$style(".table{margin: 0 auto;}"),
              tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",type="text/javascript"),
              tags$style(HTML("
                .shiny-output-error-validation {
                  color: red;
                }
              "))
              
    )
    
  )
)


# ====================================
# ===            Server            ===
# ====================================

server <- function(input, output, session) {
 
  # loading server function for each tab item
  source(file.path("server", "tab1.R"),  local = TRUE)$value
  source(file.path("server", "tab3.R"),  local = TRUE)$value
  source(file.path("server", "tab4.R"),  local = TRUE)$value
}

# define shinyApp
shinyApp(ui = ui, server = server)