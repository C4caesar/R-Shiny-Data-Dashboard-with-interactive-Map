fluidPage(
  fluidRow(
    titlePanel(h1("Student Data (1 day)",
                  style='padding-left: 15px'))
  ),
  fluidRow(
    # Box - Start --------------------------------------------------------
    valueBoxOutput("total_record_info_1d", width = 2),
    valueBoxOutput("total_count_info_1d", width = 2),
    valueBoxOutput("MACcount_per_10min_info_1d", width = 2),
    valueBoxOutput("count_per_10min_info_1d", width = 2),
    valueBoxOutput("popular_building_info_1d", width = 2),
    valueBoxOutput("popular_location_info_1d", width = 2)
    # Box -  End  --------------------------------------------------------
    
  ),
  fluidRow(
    box(
      width = 4,
      title = "Query",
      status = "primary",
      h4(helpText("Select the parameters below")),
      dateInput(inputId = "date_input", 
                label = "Date:", 
                #min = min(studentdata$Datetime), max = max(studentdata$Datetime),
                min = as.POSIXct("2019-05-01 00:10:00", cur_timezone), max = as.POSIXct("2019-10-31 23:50:00", cur_timezone),
                value= as.POSIXct("2019-09-02 00:00:00", cur_timezone)
      ),
      
      sliderInput(inputId = "slider_datetime_1d", 
                  label = "Time:", 
                  #min = min(studentdata$Datetime), max = max(studentdata$Datetime),
                  step= 600,
                  min = as.POSIXct("2019-09-02 00:00:00", cur_timezone), max = as.POSIXct("2019-09-02 23:50:00", cur_timezone),
                  value= c(as.POSIXct("2019-09-02 00:00:00", cur_timezone),as.POSIXct("2019-09-02 23:50:00", cur_timezone)),
                  animate=animationOptions(interval = 1400, loop = FALSE, playButton = NULL,
                                           pauseButton = NULL)
      ),
      leafletOutput("mymap_1d", height = "650px")
    ),
    box(
      width = 4,
      title ="Click-by-building Result",
      status = "success",
      withSpinner(uiOutput('maptb_1d'), type=3, color.background = "white")
      
    ),
    
    box(
      width = 4,
      title ="Datetime Query Result",
      status = "info",      
      withSpinner(uiOutput('studenttb_1d'), type=3, color.background = "white")
      
    )
    
  
  )
)