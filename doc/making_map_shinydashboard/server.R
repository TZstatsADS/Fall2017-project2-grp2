#server
library("dplyr")
dirColors <-c("Approaching Target"="#595490", "Exceeding Target"="#527525", "Meeting Target"="#A93F35",
              "Not Meeting Target"="#BA48AA","N/A"="#eead0e")

function(input, output, session) {
  # Define a reactive expression for the document term matrix
  #zip_range<-c(as.integer(input$Zipcode)-1,as.integer(input$Zipcode),as.integer(input$Zipcode)+1)
  df <- reactive({
    if(input$Zipcode!="All"){
      if(input$Near){
        #data_merge%>%filter(zipcode%in%zip_range,Enrollment>=input$Enroll[1],Enrollment<=input$Enroll[2])
        data_merge%>%filter(zipcode>=as.integer(input$Zipcode)-1,zipcode<=as.integer(input$Zipcode)+1,Enrollment>=input$Enroll[1],Enrollment<=input$Enroll[2])
      }else{
        data_merge%>%filter(zipcode==as.integer(input$Zipcode),Enrollment>=input$Enroll[1],Enrollment<=input$Enroll[2])
      }
    }else
    {data_merge%>%filter(Enrollment>=input$Enroll[1],Enrollment<=input$Enroll[2])}
    
    
  })
  
  marker_opt <- markerOptions(opacity=0.8,riseOnHover=T)
  output$map <- renderLeaflet({
    labels <- sprintf(
      "<strong><font color=\"#00008b\" size=3>%s</font></strong><br/>
      %s<br/> 
      <strong>%g</strong> students<br/>
      ",
      df()$school.name, as.character(df()$addr),df()$Enrollment
      
    ) %>% lapply(htmltools::HTML)
    
    if(nrow(df())<1){
      df_new<-data_merge
      labels <- sprintf(
        "<strong><font color=\"#00008b\" size=3>%s</font></strong><br/>
        %s<br/> 
        <strong>%g</strong> students<br/>
        ",
        df_new$school.name, as.character(df_new$addr),df_new$Enrollment
        
      )
    }else{
      df_new<-df()
    }
    if(input$Zipcode=="All"){
      leaflet(df_new)%>%
        addTiles()%>%
        addCircles(~lon, ~lat,popup=labels,label=~school.name,radius=~Enrollment/10,options=marker_opt,color="green",layerId=~school.name)%>%
        setView(-73.983,40.7639,zoom = 11)
    }else{
      leaflet(df_new)%>%
        addTiles()%>%
        addCircles(~lon, ~lat,popup=labels,label=~school.name,radius=~Enrollment/10,options=marker_opt,color="green",layerId=~school.name)
    }
  })
  
  #change the map color
  observeEvent(input$map_color, {
    leafletProxy("map") %>% addProviderTiles(input$map_color)
  })
  
  
  
  observeEvent(input$School1,{
    
    eventid<-input$School1
    if(eventid!=" "){
      if(!input$label_it){
        leafletProxy("map") %>%clearMarkers()%>%clearShapes()}
      df_target<-data_merge%>%filter(school.name==eventid)
      label_i<-labels_all[data_merge$school.name==input$School1]
      leafletProxy("map") %>%addMarkers(df_target$lon,df_target$lat,popup=label_i)%>%
        setView(df_target$lon,df_target$lat,zoom=13)
      
      showratings(eventid)
      show_races(eventid)
      show_6_m(eventid)
      show_18_m(eventid)
      show_teacher(eventid)
      show_eco(eventid)
      show_disa(eventid)
      show_learner(eventid)
      show_schoolname(eventid)
      show_sat(eventid)}
  })
  
  observe({
    
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showratings(event$id)
      show_races(event$id)
      show_6_m(event$id)
      show_18_m(event$id)
      show_teacher(event$id)
      show_eco(event$id)
      show_disa(event$id)
      show_learner(event$id)
      show_schoolname(event$id)
      show_sat(event$id)
    })
  })
  
  # observe({
  #   
  #   event <- input$map_marker_click
  #   if (is.null(event))
  #     return()
  #   
  #   isolate({
  #     output$hah<-renderText("haha",paste0(event$id,event$lat))
  # showratings(event$id)
  # show_races(event$id)
  # show_6_m(event$id)
  # show_18_m(event$id)
  # show_teacher(event$id)
  # show_eco(event$id)
  # show_disa(event$id)
  # show_learner(event$id)
  # show_schoolname(event$id)
  # show_sat(event$id)
  #   })
  # })
  
  ###Summary Functions
  show_sat<-function(School.name){
    df<-tidysat[tidysat$Name==School.name,]
    output$SAT<-renderPlotly({
      if(all(is.na(df$Score))){
        ax <- list(
          title = "",
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
        plot_ly(df)%>%layout(xaxis = ax,yaxis =ax)
      }else{
      sat_plot(School.name)
      }
    })
    
    output$SAT1<-renderPlotly({
      # sat_plot(School.name)
      df<-tidysat[tidysat$Name==School.name,]
      if(all(is.na(df$Score))){
        ax <- list(
          title = "",
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
        plot_ly(df)%>%layout(xaxis = ax,yaxis =ax)
      }else{
        sat_plot(School.name)
      }
    })
  }
  
  show_schoolname<-function(School.name){
    output$selected_school<-renderUI({
      sprintf(
        "%s   <strong><font color=\"#1a1a1a\" size=6>%s</font>",icon("university","fa-3x"),School.name)%>%lapply(htmltools::HTML)
    })
  }
  
  
  show_teacher<-function(School.name){
    teacher_df<-teacher_expr%>%filter(School.Name==School.name)
    output$Teacher<- renderValueBox({
      valueBox(
        paste0(teacher_df[,2]*100,"%"),"with 3 or More Years of Expr" , icon = icon("users"),
        color = "purple",width=3
      )
    })
    output$Teacher1<- renderValueBox({
      valueBox(
        paste0(teacher_df[,2]*100,"%"),"with 3 or More Years of Exper" , icon = icon("users"),
        color = "purple",width=3
      )
    })
  }
  show_eco<-function(School.name){
    econ_df<-econ_need%>%filter(School.Name==School.name)
    # if(is.na(econ_df[,2])) {
    #   return()
    # }
    output$Econ<- renderValueBox({
      valueBox(
        paste0(econ_df[,2]*100,"%"),"of the School Population" , icon = icon("dollar"),
        color = "green",width=3
      )
    })
    output$Econ1<- renderValueBox({
      valueBox(
        paste0(econ_df[,2]*100,"%"),"of the School Population" , icon = icon("dollar"),
        color = "green",width=3
      )
    })
  }
  
  show_disa<-function(School.name){
    disa_df<-disability%>%filter(School.Name==School.name)
    # if(is.na(disa_df[,2])) {
    #   return()
    # }
    output$disa<- renderValueBox({
      valueBox(
        paste0(disa_df[,2]*100,"%"),"of the School Population" , icon = icon("wheelchair"),
        color = "light-blue",width=3
      )
    })
    output$disa1<- renderValueBox({
      valueBox(
        paste0(disa_df[,2]*100,"%"),"of the School Population" , icon = icon("wheelchair"),
        color = "light-blue",width=3
      )
    })
  }
  
  #valid color for value box:
  #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, 
  #fuchsia, purple, maroon, black.
  
  show_learner<-function(School.name){
    learner_df<-language_learner%>%filter(School.Name==School.name)
    # if(is.na(learner_df[,2])) {
    #   return()
    # }
    output$learner<- renderValueBox({
      valueBox(
        paste0(learner_df[,2]*100,"%"),"of the School Population" , icon = icon("language"),
        color = "maroon",width=3
      )
    })
    output$learner1<- renderValueBox({
      valueBox(
        paste0(learner_df[,2]*100,"%"),"of the School Population" , icon = icon("language"),
        color = "maroon",width=3
      )
    })
  }
  
  show_6_m<-function(School.name){
    df6<-tidysixmonth%>%filter(SchoolName==School.name)
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 3
    )
    output$show_6_m<-renderPlotly({
      
      plot_ly(df6, labels = ~Type, values = ~Number, type = 'pie',textposition = 'inside',
              textinfo = 'label+percent',insidetextfont = list(color = '#FFFFFF'),marker=list(colors=col1)) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),margin=m)
    })
    output$show_6_m1<-renderPlotly({
      
      plot_ly(df6, labels = ~Type, values = ~Number, type = 'pie',textposition = 'inside',
              textinfo = 'label+percent',insidetextfont = list(color = '#FFFFFF'),marker=list(colors=col1)) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),margin=m)
    })
  }
  
  show_18_m<-function(School.name){
    df18<-tidyeighteenmonth%>%filter(SchoolName==School.name)
    if(is.na(df18$Number[1])) {
      return()
    } 
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 3
    )
    output$show_18_m<-renderPlotly({
      
      plot_ly(df18, labels = ~Type, values = ~Number, type = 'pie',textposition = 'inside',
              textinfo = 'label+percent',insidetextfont = list(color = '#FFFFFF'),marker=list(colors=col2)) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),margin=m)
    })
    output$show_18_m1<-renderPlotly({
      
      plot_ly(df18, labels = ~Type, values = ~Number, type = 'pie',textposition = 'inside',
              textinfo = 'label+percent',insidetextfont = list(color = '#FFFFFF'),marker=list(colors=col2)) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),margin=m)
    })
  }
  
  
  show_races<-function(School.name){
    df_r<-tidyrace%>%filter(School.Name==School.name)
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 3
    )
    output$race<-renderPlotly({
      plot_ly(df_r, labels = ~Race, values = ~Number, type = 'pie',textposition = 'inside',
              textinfo = 'label+percent',insidetextfont = list(color = '#FFFFFF'),marker=list(colors=col)) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),margin=m)
    })
    output$race1<-renderPlotly({
      #school_name<-df$School.Name[1]
      plot_ly(df_r, labels = ~Race, values = ~Number, type = 'pie',textposition = 'inside',
              textinfo = 'label+percent',insidetextfont = list(color = '#FFFFFF'),marker=list(colors=col)) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),margin=m)
    })
  }
  #show the ui text
  showratings<-function(School.name){
    target_df<-data_merge%>%filter(school.name==School.name)
    if(is.null(target_df)) {
      return()
    }
    output$ratings<-renderUI({
      sprintf(
        "%s <strong><font size=3 color=\"#5c5c5c\" style=\"Monospace\">Summary</font></strong><br/>
      
      <span style=\"width:0.5em; height:0.5em; background-color:#8b2323; display:inline-block;\"></span>
      <font color=\"#8b2323\">Rigorous Instruction</font>: <br/>%s %s<br/>
      <span style=\"width:0.5em; height:0.5em; background-color:#458b00; display:inline-block;\"></span>
      <font color=\"#458b00\">Collaborative Teaching</font>: <br/>%s %s<br/>
      <span style=\"width:0.5em; height:0.5em; background-color:#cd9b1d; display:inline-block;\"></span>
      <font color=\"#cd9b1d\">Supportive Environment</font>: <br/>%s %s<br/>
      <span style=\"width:0.5em; height:0.5em; background-color:#68228b; display:inline-block;\"></span>
      <font color=\"#68228b\">Effective School Leadership</font>: <br/>%s %s<br/>
      <span style=\"width:0.5em; height:0.5em; background-color:#104e8b; display:inline-block;\"></span>
      <font color=\"#104e8b\">Strong Family Community Tie</font>: <br/>%s %s<br/>
      <span style=\"width:0.5em; height:0.5em; background-color:#698b69; display:inline-block;\"></span>
      <font color=\"#698b69\">Trust</font>: <br/>%s %s",
        icon("file-text-o"),
        target_df$Rigorous.Instruction.Rating,icon_list[target_df$Rigorous.Instruction.Rating][[1]],
        target_df$Collaborative.Teachers.Rating,icon_list[target_df$Collaborative.Teachers.Rating][[1]],
        target_df$Supportive.Environment.Rating,icon_list[target_df$Supportive.Environment.Rating][[1]],
        target_df$Effective.School.Leadership.Rating,icon_list[target_df$Effective.School.Leadership.Rating][[1]],
        target_df$Strong.Family.Community.Ties.Rating,icon_list[target_df$Strong.Family.Community.Ties.Rating][[1]],
        target_df$Trust.Rating,icon_list[target_df$Trust.Rating][[1]]
      ) %>% lapply(htmltools::HTML)
    })
  }
  
  output$explain<-renderUI({
    sprintf(
      "%s   <font color=\"#ffffff\" size=2>%s</font>",icon("dollar"),ECON_EXPLAIN)%>%lapply(htmltools::HTML)
  })
  output$explain1<-renderUI({
    sprintf(
      "%s   <font color=\"#ffffff\" size=2>%s</font>",icon("dollar"),ECON_EXPLAIN)%>%lapply(htmltools::HTML)
  })
  
  
  output$Boro_1<-renderUI({selectInput('Boro1',strong(icon("map-pin"),'Borough'),c(" ",unique(data_merge$borough)),selected = " ")})
  output$School_1<-renderUI({
    if (is.null(input$Boro1) || input$Boro1 == " "){
      selectInput('School1',strong(icon("university"),'Select a School'),c(" ",as.character(data_merge$school.name)),selected = " ")
    }else selectInput("School1", 
                      "Select a School", 
                      c(unique(as.character(data_merge$school.name)[data_merge$borough==input$Boro1])," "),
                      " ")
  })
  
  
  ################3Second Tab for hist plot
  selectedData<-reactive({
    HS_frame[,Aspects[input$aspect][[1]]]
  })
  
  output$hist_School<-renderUI({
    if(is.na(selectedData()[selectedData()$School.Name==input$school_name2,4])){
      sprintf("<strong><font color=\"#1a1a1a\" size=6> %s Sorry! No <font color=\"#8b1a1a\">%s</font> Data For This School",icon("exclamation-triangle"),input$aspect)%>%lapply(htmltools::HTML)
    }else{
      sprintf(
        "<strong><font color=\"#1a1a1a\" size=6>%s %s</font>",icon("bookmark-o"),input$school_name2)%>%lapply(htmltools::HTML)
    }})
  
  output$rating_hist <- renderPlotly({
    #plot_scores(selectedData(),input$school_name,input$aspect)
    if(is.na(selectedData()[selectedData()$School.Name==input$school_name2,4])){
      plot_ly(x=selectedData()[,4],type = "histogram")
    }else{
      plot_scores(selectedData(),input$school_name2,input$aspect)
    }
    
  })
  
  ###make the value boxes
  ranks_data<-reactive({
    cal_values(selectedData(),input$school_name2)
  })
  
  output$Rank <- renderValueBox({
    valueBox(
      ranks_data()[1],"Rank" , icon = icon("list"),
      color = "purple",width=3
    )
  })
  output$Rank_city <- renderValueBox({
    valueBox(
      ranks_data()[2],"City Level" , icon = icon("building-o"),
      color = "blue",width=3
    )
  })
  output$Rank_boro <- renderValueBox({
    valueBox(
      ranks_data()[3],"Borough Level" , icon = icon("bicycle"),
      color = "yellow",width=3
    )
  })
  
  
  
  
  
  
} 
