fluidPage(
  fluidRow(
    titlePanel(h1("Student Data (6 months)",
                  style='padding-left: 15px'))
  ),
  fluidRow(
    # Box - Start --------------------------------------------------------
    valueBoxOutput("total_record_info", width = 2),
    valueBoxOutput("total_count_info", width = 2),
    valueBoxOutput("count_per_date_info", width = 2),
    valueBoxOutput("count_per_10min_info", width = 2),
    valueBoxOutput("popular_building_info", width = 2),
    valueBoxOutput("popular_location_info", width = 2)
    # Box -  End  --------------------------------------------------------
  ),
  fluidRow(
    box(
      width = 4,
      title = "Query",
      status = "primary",
      h4(helpText("Select the parameters below")),
      sliderInput(inputId = "slider_datetime", 
                  label = "Date & time:", 
                  #min = min(studentdata$Datetime), max = max(studentdata$Datetime),
                  min = as.POSIXct("2019-05-01 00:00:00", cur_timezone), max = as.POSIXct("2019-10-31 00:00:00", cur_timezone),
                  value= c(as.POSIXct("2019-05-01 00:00:00", cur_timezone),as.POSIXct("2019-06-01 00:00:00", cur_timezone)),
                  animate=animationOptions(interval = 1300, loop = FALSE, playButton = NULL,
                                           pauseButton = NULL)
      ),
      selectInput("colors", "Color Scheme",
                  rownames(subset(brewer.pal.info, category %in% c("seq", "div"))), selected = "Reds" # predefined selected = Reds 
      ),
      leafletOutput("mymap", height = "650px")
      
    ),
    box(
      width = 4,
      title ="Click-by-building Result",
      status = "success",
      withSpinner(uiOutput('maptb'), type=3, color.background = "white")
      
    ),
    box(
      width = 4,
      title ="Datetime Query Result",
      status = "info", 
      withSpinner(uiOutput('studenttb'), type=3, color.background = "white")
      
    )
    
  )
)