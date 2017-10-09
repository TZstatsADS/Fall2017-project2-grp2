library(shiny)
library(shinydashboard)
shinyServer(function(input, output) {
  output$plot=renderPlotly({
    p = plot_ly()
    x=input$school1
    y=input$school2
    
    loc1<-which(df$school==x)[1]
    loc2<-which(df$school==y)[1]
    
    for(i in 1:6) {
      p <- add_trace(
        p, 
        x = c(df$o5[i],0), 
        y = c(df$a5[i],0), 
        evaluate = TRUE,
        line = list(color = "#d3d3d3", dash = "3px"),
        showlegend = FALSE
      )
    }
    
    d<-df[c(1:6,1),]
    dd<-df[c(loc1:(loc1+5),loc1,loc2:(loc2+5),loc2),]
    
    p %>% 
      add_trace(data = dd, x = dd$o, y = dd$a, color = factor(dd$school), 
                mode = "lines+markers",
                hoverinfo = "text",
                text = paste(dd$response, dd$score)) %>% 
      add_trace(data = d, x = d$o5, y = d$a5, 
                text = d$response,
                hoverinfo = "none",
                textposition = "top middle", mode = "lines+text", 
                line = list(color = "#d3d3d3", dash = "3px", shape = "spline"),
                showlegend = FALSE) %>% 
      add_trace(data = d, x = d$o4, y = d$a4, mode = "lines",
                line = list(color = "#d3d3d3", dash = "3px", shape = "spline"), 
                hoverinfo = "none",
                showlegend = FALSE) %>% 
      add_trace(data = d, x = d$o3, y = d$a3, mode = "lines", 
                line = list(color = "#d3d3d3", dash = "3px", shape = "spline"), 
                hoverinfo = "none",
                showlegend = FALSE) %>%
      add_trace(data = d, x = d$o2, y = d$a2, mode = "lines", 
                line = list(color = "#d3d3d3", dash = "3px", shape = "spline"), 
                hoverinfo = "none",
                showlegend = FALSE) %>%
      add_trace(data = d, x = d$o1, y = d$a1, mode = "lines", 
                line = list(color = "#d3d3d3", dash = "3px", shape = "spline"), 
                hoverinfo = "none",
                showlegend = FALSE) %>%
      layout(
        autosize = FALSE,
        hovermode = "closest",     
        autoscale = TRUE,
        width = 450,
        height = 450,
        xaxis = list(range = c(-2,2), showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
        yaxis = list(range = c(-2,2), showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE))
  })
})
