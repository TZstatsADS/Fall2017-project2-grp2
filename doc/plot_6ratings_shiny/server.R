function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    HS_frame[,c(Aspects[input$aspect][[1]])]
  })
  
  output$plot1 <- renderPlotly({
    #plot_scores(selectedData(),input$school_name,input$aspect)
    if(is.na(selectedData()[selectedData()$School.Name==input$school_name,4])){
      plot_ly(x=selectedData()[,4],type = "histogram")%>%
        layout(title=paste("Have no",input$aspect,"data for this school"))
    }else{
      plot_scores(selectedData(),input$school_name,input$aspect)
      }
    
  })
  
}