studentdata <- read.csv("focusGroupStudent/df_allStudent.csv", header = TRUE)
studentdata$fp <- NULL
studentdata$Datetime = as.POSIXct(studentdata$Time, format = "%m/%d/%Y %H:%M:%S")

# Dynamically update the time slider
observe({
  updateSliderInput(session, "slider_datetime_1d",
                    min = as.POSIXct(paste(input$date_input, " 00:00:00"), cur_timezone), max = as.POSIXct(paste(input$date_input, " 23:50:00"), cur_timezone),
                    value= c(as.POSIXct(paste(input$date_input, " 00:00:00"), cur_timezone),as.POSIXct(paste(input$date_input, " 23:50:00"), cur_timezone))
  )
})

# reactive variables
filtered_data_reactive1 <- reactive({
  filtered_data <- studentdata %>% filter(Datetime >= input$slider_datetime_1d[1] & Datetime <= input$slider_datetime_1d[2])
  return(filtered_data)
})

# Static Map, with tiles, center view point
output$mymap_1d <- renderLeaflet({
  leaflet() %>%
    addTiles(urlTemplate="https://api.hkmapservice.gov.hk/osm/xyz/basemap/WGS84/tile/{z}/{x}/{y}.png?key=584b2fa686f14ba283874318b3b8d6b0")%>%
    addTiles(urlTemplate="https://api.hkmapservice.gov.hk/osm/xyz/label-tc/WGS84/tile/{z}/{x}/{y}.png?key=584b2fa686f14ba283874318b3b8d6b0")%>%
    clearShapes() %>%
    setView(114.179747, 22.304247, zoom = 18)
})

# for map only
cbb1 <- reactive({
  # count record in each buildings
  filtered_data <- filtered_data_reactive1()
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
q_location11 <- reactive({
  filtered_data <- filtered_data_reactive1()  
  q_location_in <- sqldf(paste("select Location, count(*) As count_nMAC from filtered_data where building = '",input$mymap_1d_shape_click$id ,"' group by location order by count_nMAC desc", sep=""))
  q_location_in$Location <- factor(q_location_in$Location, levels = q_location_in$Location[order(q_location_in$count_nMAC, decreasing = TRUE)])
  
  return(q_location_in)
  
})

q_building11 <- reactive({
  filtered_data <- filtered_data_reactive1()
  q_building_in <- sqldf(paste("select Location, Datetime, count(*) As count_nMAC from filtered_data where building = '",input$mymap_1d_shape_click$id,
                               "' group by Location, Datetime order by location", sep=""))
  # sort the data by q_location count
  q_building_in$Location <- factor(q_building_in$Location, levels = q_location11()$Location)
  
  return(q_building_in)
})
# count_building Reactive
cb1 <- reactive({
  filtered_data <- filtered_data_reactive1()
  
  count_building <- dplyr::count(filtered_data, filtered_data$building, sort = TRUE)
  count_building$`filtered_data$building` <- factor(count_building$`filtered_data$building`, levels = count_building$`filtered_data$building`[order(count_building$n, decreasing = TRUE)])
  count_building = cbind(count_building, count_building$`filtered_data$building`)
  
  return(count_building)
})
# count_Location Reactive
cl1 <- reactive({
  filtered_data <- filtered_data_reactive1()
  count_Location <- dplyr::count(filtered_data, filtered_data$Location, sort = TRUE)
  count_Location$`filtered_data$Location` <- factor(count_Location$`filtered_data$Location`, levels = count_Location$`filtered_data$Location`[order(count_Location$n, decreasing = TRUE)])
  
  return(count_Location)
})

# studentdata2 Reactive, only for line plotly
count_record_time1 <- reactive({
  filtered_data <- filtered_data_reactive1()
  studentdata2 <- sqldf("select Datetime As Datet, count(distinct(nMAC)) As count_nMAC from filtered_data group by Datetime order by Datetime")
  studentdata2$Datet <- as.POSIXct(studentdata2$Datet, origin='1970-01-01')
  
  return(studentdata2)
})

# update plot, sum, table, etc.
observe({
  # First get filtered data
  filtered_data <- filtered_data_reactive1()
  # set box value - start -------------------------------------------------
  output$total_record_info_1d <- renderInfoBox({
    valueBox(
      nrow(filtered_data),
      "Total Record Count",
      icon = icon("table"),
      color = "purple"
    )
  })
  output$total_count_info_1d <- renderInfoBox({
    shiny::validate (
      need(nrow(filtered_data) > 0, "No result found, please select another parameters.")
    )
    valueBox(
      length(unique(filtered_data$nMAC)), 
      "Total Device Count",
      icon = icon("users"),
      color = "orange"
    )
  })
  output$MACcount_per_10min_info_1d <- renderValueBox({
    shiny::validate (
      need(nrow(filtered_data) > 0, "No result found, please select another parameters.")
    )
    valueBox(
      round(length(unique(filtered_data$nMAC)) / (as.numeric(difftime(input$slider_datetime_1d[2], input$slider_datetime_1d[1], units="mins"))/10), digits = 2), 
      "Device/10-mins",
      icon = icon("mobile-alt"),
      color = "green"
    )
  })
  output$count_per_10min_info_1d <- renderValueBox({
    shiny::validate (
      need(nrow(filtered_data) > 0, "No result found, please select another parameters.")
    )
    valueBox(
      round(nrow(filtered_data) / (as.numeric(difftime(input$slider_datetime_1d[2], input$slider_datetime_1d[1], units="mins"))/10), digits = 2), 
      "Record/10-mins",
      icon = icon("clock"),
      color = "red"
    )
  })
  output$popular_building_info_1d <- renderValueBox({
    shiny::validate (
      need(nrow(filtered_data) > 0, "No result found, please select another parameters.")
    )
    valueBox(
      cb1()$`filtered_data$building`[1], 
      "Most Popular Building",
      icon = icon("building"),
      color = "teal"
    )
  })
  output$popular_location_info_1d <- renderValueBox({
    shiny::validate (
      need(nrow(filtered_data) > 0, "No result found, please select another parameters.")
    )
    valueBox(
      cl1()$`filtered_data$Location`[1],
      "Most Popular Location", 
      icon = icon("thumbtack"),
      color = "yellow"
    )
  })
  # set box value -  end  -------------------------------------------------
  
  
  # Display Table
  output$studentTable_1d  <- renderDataTable({
    # Filter the data by datetime
    shiny::validate (
      need(nrow(filtered_data) > 0, "No result found, please select another parameters.")
    )
    filtered_data
  })
  
  
  
  # Display Data summary
  output$summ_1d <- renderPrint({
    shiny::validate (
      need(nrow(filtered_data) > 0, "No result found, please select another parameters.")
    )
    
    dfSummary(filtered_data)
    
  })
  # define sub plot
  output$studentTablePlot1_1d <- renderPlotly({
    shiny::validate (
      need(nrow(filtered_data) > 0, "No result found, please select another parameters.")
    )
    studentdata2 <- count_record_time1()
    plot_ly(studentdata2, x = ~Datet, y = ~count_nMAC, type = 'scatter', mode = 'lines') %>% 
      layout(title = paste("Record Count from", strftime(input$slider_datetime_1d[1],format('%Y-%m-%d %H:%M'), tz=cur_timezone), 'to', strftime(input$slider_datetime_1d[2],format('%Y-%m-%d %H:%M'), tz=cur_timezone)),
             xaxis = list(title = "Datetime"), yaxis = list(title = "Record Count"))
    
    
    
    
  })
  output$studentTablePlot2_1d <- renderPlotly({
    shiny::validate (
      need(nrow(filtered_data) > 0, "No result found, please select another parameters.")
    )
    count_building <- cb1()
    plot_ly(count_building[1:10,], x = ~`filtered_data$building`[1:10], y = ~n[1:10], type = 'bar', color = ~`filtered_data$building`[1:10]) %>%
      layout(title = "Top 10 Frequent Connected Building",
             xaxis = list(title = "Building"), yaxis = list(title = "Record Count"))
  })
  
  output$studentTablePlot3_1d <- renderPlotly({
    shiny::validate (
      need(nrow(filtered_data) > 0, "No result found, please select another parameters.")
    )
    count_Location <- cl1()
    plot_ly(count_Location[1:10,], x = ~`filtered_data$Location`[1:10], y = ~n[1:10], type = 'bar', color = ~`filtered_data$Location`[1:10]) %>%
      layout(title = "Top 10 Frequent Connected Location",
             xaxis = list(title = "Location"), yaxis = list(title = "Record Count"))
  })
  
})
# Map leaflet proxy - Start ---------------------------------------------
# Map Visualization using Leaflet-Proxy
observe({
  
  count_max <- max(cbb1()$n)
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
  pal <- colorBin(input$colors, domain = cbb1()$n, bins = bins)
  labels <- paste("<p><b> Datetime </b>: ", paste( strftime(input$slider_datetime_1d[1],format('%Y-%m-%d %H:%M'), tz=cur_timezone), 'to', strftime(input$slider_datetime_1d[2],format('%Y-%m-%d %H:%M'), tz=cur_timezone)), "</p>",
                  "<p><b> Building </b>: ", cbb1()$`filtered_data$building`, "</p>",
                  "<p><b> Building Name </b>: ", shape$nameOfBuil, "</p>",
                  "<p><b> Count </b>: ", cbb1()$n, "</p>",
                  "<p><b> Unique nMAC </b>: ", cbb1()$count_nMAC, "</p>",
                  "<p><b> Average record per nMAC</b>: ", round(cbb1()$n / cbb1()$count_nMAC,digits=2), "</p>",sep="")
  
  
  leafletProxy("mymap_1d", data = cbb1()) %>%
    clearShapes() %>%
    clearControls() %>%
    addPolygons(data = shape, color = "grey", weight=2, smoothFactor = 2, 
                layerId = cbb1()$`filtered_data$building`, 
                fillOpacity = 0.8, fillColor = pal(cbb1()$n),
                highlightOptions = highlightOptions(color = "red", weight = 4,
                                                    bringToFront = TRUE),
                options = pathOptions(clickable = TRUE),
                label = lapply(labels, HTML),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")
    ) %>%
    addLegend(pal = pal, values = cbb1()$n, opacity = 0.7, position="topright")
})
# Map leaflet proxy -  End  ---------------------------------------------
# Map Plot Function
# Create Reactive Plot Object for plotting graph
plot_buildingloc_count_1d <- reactive({
  q_building1 <- q_building11()
  plot_ly(q_building1, x = ~Datetime, y = ~count_nMAC, type = 'scatter', mode = 'lines', color = ~Location) %>%
    layout(title = "Record Count Over Time",
           xaxis = list(title = "Date"), yaxis = list(title = "Record Count", range = c(0, max(q_building1$count_nMAC)))
    ) %>%
    rangeslider()
})

plot_location_bar_1d <- reactive({
  q_location1 <- q_location11()
  plot_ly(q_location1, x = ~Location, y = ~count_nMAC, type = 'bar', color = ~Location) %>%
    layout(title = paste("Record Count on Each Floor (", input$mymap_1d_shape_click$id, ")"),
           xaxis = list(title = "Location"), yaxis = list(title = "Record Count"))
})

# Map Polygon Click Trigger Event
observeEvent( input$mymap_1d_shape_click, {
  
  
  output$map_plot1_1d <- renderPlotly({
    
    plot_buildingloc_count_1d()
    
  })
  
  output$map_plot2_1d <- renderPlotly({
    
    plot_location_bar_1d()
  })
  
  
})


# Layout ========================================================
# Map Plot Layout
output$maptb_1d <- renderUI({
  fluidPage(
    fluidRow(
      withSpinner(plotlyOutput("map_plot1_1d"), type=3, color.background = "white")
    ),
    fluidRow(
      withSpinner(plotlyOutput("map_plot2_1d"), type=3, color.background = "white")
    )
  )
})
# isolating elements - --------------------------------------------------
# Plots layout
output$studentTablePlots_1d <- renderUI({
  fluidPage(
    fluidRow(
      withSpinner(plotlyOutput("studentTablePlot1_1d"), type=3, color.background = "white")
    ),
    fluidRow(
      withSpinner(plotlyOutput("studentTablePlot2_1d"), type=3, color.background = "white")
    ),
    fluidRow(
      withSpinner(plotlyOutput("studentTablePlot3_1d"), type=3, color.background = "white")
    )
  )
})
# Display tabsetPanel
output$studenttb_1d <- renderUI({
  tabBox(
    width='100%',
    tabPanel("Plot", uiOutput("studentTablePlots_1d")),
    tabPanel("Summary", verbatimTextOutput("summ_1d")),
    tabPanel("Table", dataTableOutput("studentTable_1d"))
  )
})