fluidPage(
  fluidRow(
    titlePanel(h1("Crowd View",
                  style='padding-left: 15px'))
  ),
  fluidRow(
    tags$iframe(src="http://localhost/fyp/work_allDay.php?mm=1", height=850, width=1500, frameborder = "no")
  )
)
