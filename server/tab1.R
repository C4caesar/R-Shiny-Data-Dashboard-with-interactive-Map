data2 <- read.csv("focusGroupStudent/trueLabel.csv", header = FALSE)
matrixdata2 <- as.matrix(data2)
dd2 <- as.dist(data2)
trueGroup <- kmeans(dd2, centers = 3, nstart = 100)

observe({
  data <- read.csv(paste("focusGroupStudent/", input$sim, ".csv", sep=''), header = FALSE)
  matrixdata <- as.matrix(data)
  dd <- as.dist(data)
  k_result <- kmeans(dd, centers = input$k_c, nstart = 100)
  
  
  
  
  
  tsne <- Rtsne(matrixdata, check_duplicates = FALSE, pca = TRUE,
                perplexity=30, theta=0.0, dims=2)
  rd <- as.data.frame(tsne$Y)
  
  
  # Set output
  output$select_sim <- renderText(paste("Selected Similarity Measure: ",input$sim))
  
  output$tb <- renderPlot({
    fviz_cluster(k_result, data = dd,geom = c("point"),frame.type = "norm", main = "K-means PCA Plot")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$tb2 <- renderPlot({
    k_result$cluster <- trueGroup$cluster
    fviz_cluster(k_result, data = dd,geom = c("point"),frame.type = "norm", main = "True Label PCA Plot")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$tb3 <- renderPlot({
    
    ggplot(rd, aes(x=V1, y=V2, color=k_result$cluster)) +
      geom_point(size=1.75) +
      labs(title = "K-means t-SNE Plot",
           x = "Dim1",
           y = "Dim2") +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_color_gradientn(colours = rainbow(5))
  })
  output$tb4 <- renderPlot({
    
    ggplot(rd, aes(x=V1, y=V2, color=trueGroup$cluster)) +
      geom_point(size=1.75) +
      labs(title = "True Label t-SNE Plot",
           x = "Dim1",
           y = "Dim2") +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_color_gradientn(colours = rainbow(5))
  })
  
  
})


