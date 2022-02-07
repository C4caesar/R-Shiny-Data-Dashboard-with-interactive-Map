fluidPage(
  
  fluidRow(
    titlePanel(h1("K-means Clustering on Different Similarity Measures",
                  style='padding-left: 15px'))
  ),
  
  fluidRow(
    box(
      width = 3,
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      title = "Query",
      h4(helpText("Select the parameters below")),
      
      sliderInput("k_c", "Number of clusters:",
                  min = 2, max = 10,
                  value = 3, step = 1),
      radioButtons('sim', 'Similarity measures', c(Adam='Adam', Cosine='Cosine', Jaccard='Jaccard', Psim='Psim-3', Rarity='Rarity'), selected = 'Adam'),
      
      
    ),
    
    box(
      width = 9,
      title ="Result",
      # show our read file input as a table
      
      fluidRow(
        column(6, plotOutput("tb")),
        column(6, plotOutput("tb2"))
        
      ),
      fluidRow(
        column(6, plotOutput("tb3")),
        column(6, plotOutput("tb4"))
        
      )
    
    )
  )
)
