library(shiny)
library(shinydashboard)


shinyServer(function(input, output) {


# Radar plot for both of the schools 
  output$plot=renderPlotly({
    p = plot_ly()
    
    if(input$br_val==1){
      x=input$school1.1
    }
    else if(input$br_val==2){
      x=input$school1.2
    }
    else if(input$br_val==3){
      x=input$school1.3
    }
    else if(input$br_val==4){
      x=input$school1.4
    }
    else if(input$br_val==5){
      x=input$school1.5
    }
    
    
    if(input$br_val2==1){
      y=input$school2.1
    }
    else if(input$br_val2==2){
      y=input$school2.2
    }
    else if(input$br_val2==3){
      y=input$school2.3
    }
    else if(input$br_val2==4){
      y=input$school2.4
    }
    else if(input$br_val2==5){
      y=input$school2.5
    }

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
    
    if(is.na(dd[1:6,3])&is.na(dd[8:13,3])==F){
      p %>% 
        add_trace(data = dd[8:14,], x = dd[8:14,]$o, y = dd[8:14,]$a, color = "red", 
                  mode = "lines+markers",
                  textposition = "bottom middle",
                  hoverinfo = "text",
                  text = paste(dd[8:14,]$response, dd[8:14,]$score)) %>% 
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
    }
    
    else if(is.na(dd[1:6,3])==F&is.na(dd[8:13,3])){
      p %>% 
        add_trace(data = dd, x = dd[1:7,]$o, y = dd[1:7,]$a, color = "red", 
                  mode = "lines+markers",
                  hoverinfo = "text",
                  text = paste(dd[1:7,]$response, dd[1:7,]$score)) %>% 
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
    }
    
    else if(is.na(dd[1:6,3])&is.na(dd[8:13,3])){
      p %>% 
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
    }
    
    else{
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
    }
  })
})
