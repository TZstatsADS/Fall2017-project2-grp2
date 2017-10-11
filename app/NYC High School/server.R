#server
library("dplyr")
dirColors <-c("Approaching Target"="#595490", "Exceeding Target"="#527525", "Meeting Target"="#A93F35",
              "Not Meeting Target"="#BA48AA","N/A"="#eead0e")

function(input, output, session) {
  
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
        setView(-73.983,40.7639,zoom = 12)%>%
        addProviderTiles(input$map_color)
    }else{
      leaflet(df_new)%>%
        addTiles()%>%
        addCircles(~lon, ~lat,popup=labels,label=~school.name,radius=~Enrollment/10,group=~borough,options=marker_opt,color="green",layerId=~school.name)%>%
        addProviderTiles(input$map_color)
    }
  })
  
  #change the map color
  observeEvent(input$map_color, {
    leafletProxy("map") %>% addProviderTiles(input$map_color)
  })
 
  
  ####################### OBSERVED EVENTS ####################### ####################### 
  observeEvent(input$School1,{
    
    eventid<-input$School1
    if(eventid!=" "){
      if(!input$label_it){
        leafletProxy("map") %>%clearMarkers()}
      df_target<-data_merge%>%filter(school.name==eventid)
      label_i<-labels_all[data_merge$school.name==input$School1]
      leafletProxy("map") %>%addMarkers(df_target$lon,df_target$lat,popup=label_i,layerId=df_target$school.name)%>%
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
      show_sat(eventid)
      show_schoolname_tab_details(eventid)
      show_ranks(eventid)
      #show_rating4search(eventid)
    }
    
  })
  
  
  observe({
    event <- input$map_shape_click
    # if (is.null(event))
    #   return()
    if (is.null(event)){
      event$id<-"The High School of Fashion Industries"
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
        show_rating_hist(event$id)
        show_schoolname_tab_details(event$id)
        show_ranks(event$id)
        })
    }else{
      
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
        show_rating_hist(event$id)
        show_schoolname_tab_details(event$id)
        show_ranks(event$id)
        #updateSelectInput(session ,"Boro1",selected=data_merge$borough[data_merge$school.name==event$id])
        #updateSelectInput(session ,"School1",selected=event$id)
        updateSelectInput(session ,"school_name2",selected=event$id)
        
      })}
  })
  
  observe({
    event <- input$map_marker_click
    if (is.null(event))
      return()
    print(event)
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
      show_rating_hist(event$id)
      show_schoolname_tab_details(event$id)
      show_ranks(event$id)
      updateSelectInput(session ,"school_name2",selected=event$id)
    })
  })
  
  
  ############################################## FUNCTIONS FOR 1st TAB ####################### ####################### 
  #show the ui text
  showratings<-function(School.name){
    target_df<-data_merge%>%filter(school.name==School.name)
    if(is.null(target_df)) {
      return()
    }
    output$ratings<-renderUI({
      sprintf(
        "%s <strong><font size=3 color=\"#5c5c5c\" style=\"Monospace\">Summary</font></strong><br/>
        <span style=\"width:10em; height:0.1em; background-color:#3d3d3d; display:inline-block;\"></span><br/>
        <span style=\"width:0.5em; height:0.5em; background-color:#8b2323; display:inline-block;\"></span>
        <font color=\"#8b2323\"><strong>Rigorous Instruction</font></strong>: <br/>%s %s<br/>
        <span style=\"width:0.5em; height:0.5em; background-color:#458b00; display:inline-block;\"></span>
        <font color=\"#458b00\"><strong>Collaborative Teaching</font></strong>: <br/>%s %s<br/>
        <span style=\"width:0.5em; height:0.5em; background-color:#cd9b1d; display:inline-block;\"></span>
        <font color=\"#cd9b1d\"><strong>Supportive Environment</font></strong>: <br/>%s %s<br/>
        <span style=\"width:0.5em; height:0.5em; background-color:#68228b; display:inline-block;\"></span>
        <font color=\"#68228b\"><strong>Effective School Leadership</font></strong>: <br/>%s %s<br/>
        <span style=\"width:0.5em; height:0.5em; background-color:#104e8b; display:inline-block;\"></span>
        <font color=\"#104e8b\"><strong>Strong Family Community Tie</font></strong>: <br/>%s %s<br/>
        <span style=\"width:0.5em; height:0.5em; background-color:#698b69; display:inline-block;\"></span>
        <font color=\"#698b69\"><strong>Trust</font></strong>: <br/>%s %s",
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
      
      plot_ly(df18, labels = ~Type, values = ~Number, type = 'pie',mode='lines',textposition = 'inside',
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
  
  
  output$explain<-renderUI({
    sprintf(
      "%s   <font color=\"#ffffff\" size=2>%s</font>",icon("dollar"),ECON_EXPLAIN)%>%lapply(htmltools::HTML)
  })
  output$explain1<-renderUI({
    sprintf(
      "%s   <font color=\"#ffffff\" size=2>%s</font>",icon("dollar"),ECON_EXPLAIN)%>%lapply(htmltools::HTML)
  })
  
  
  output$Boro_1<-renderUI({selectInput('Boro1',strong(icon("map-pin"),'Select a Borough'),c(" ",unique(data_merge$borough)),selected = " ")})
  output$School_1<-renderUI({
    if (is.null(input$Boro1) || input$Boro1 == " "){
      selectInput('School1',strong(icon("university"),'Select a School'),c(" ",as.character(data_merge$school.name)),selected = " ")
    }else selectInput("School1",
                      strong(icon("university"),'Select a School'),
                      c(unique(as.character(data_merge$school.name)[data_merge$borough==input$Boro1])," "),
                      " ")
  })
  
  #######################  #######################  ####################### 
  ################Third Tab for hist plot
  selectedData<-reactive({
    HS_frame[,Aspects[input$aspect][[1]]]
  })
  
  show_schoolname_tab_details<-function(School.Name){
    output$hist_School<-renderUI({
      if(is.na(selectedData()[selectedData()$School.Name==School.Name,4])){
        sprintf("<strong><font color=\"#1a1a1a\" size=6> %s Sorry! No <font color=\"#8b1a1a\">%s</font> Data For This School",icon("University"),input$aspect)%>%lapply(htmltools::HTML)
      }else{
        sprintf(
          "<strong><font color=\"#1a1a1a\" size=6>%s %s</font>",icon("university"),School.Name)%>%lapply(htmltools::HTML)
      }})}
  
  observeEvent(input$school_name2,{
    eventid<-input$school_name2
    #show_schoolname_tab_details(eventid)
    if(eventid!=" "){
      if(eventid!=" "){
        if(!input$label_it){
          leafletProxy("map") %>%clearMarkers()
          }
        df_target<-data_merge%>%filter(school.name==eventid)
        label_i<-labels_all[data_merge$school.name==input$school_name2]
        leafletProxy("map") %>%addMarkers(df_target$lon,df_target$lat,popup=label_i,layerId=df_target$school.name)%>%
          setView(df_target$lon,df_target$lat,zoom=13)
      }
      show_rating_hist(eventid)
      show_schoolname_tab_details(eventid)
      show_ranks(eventid)
    }
  })
  observeEvent(input$label_it,{if(!input$label_it) leafletProxy("map") %>%clearMarkers()})
  
  observeEvent(input$aspect,{
    eventid<-input$school_name2
    #show_schoolname_tab_details(eventid)
    if(eventid!=" "){
      show_rating_hist(eventid)
      show_schoolname_tab_details(eventid)
      show_ranks(eventid)
    }
  })
  
  show_rating_hist<-function(School.Name){
    output$rating_hist <- renderPlotly({
      #plot_scores(selectedData(),input$school_name,input$aspect)
      if(is.na(selectedData()[selectedData()$School.Name==School.Name,4])){
        plot_ly(x=selectedData()[,4],type = "histogram")
      }else{
        plot_scores(selectedData(),School.Name,input$aspect)
      }
    })
  }
  
  show_ranks<-function(School.Name){
    ranks_data<-cal_values(selectedData(),School.Name)
    output$Rank <- renderValueBox({
      valueBox(
        ranks_data[1],"Rank" , icon = icon("list"),
        color = "purple",width=3
      )
    })
    output$Rank_city <- renderValueBox({
      valueBox(
        ranks_data[2],"on School Survey" , icon = icon("street-view"),
        color = "blue",width=3
      )
    })
    output$Rank_boro <- renderValueBox({
      valueBox(
        ranks_data[3],paste(data_merge$borough[data_merge$school.name==School.Name]," Level") , icon = icon("map-signs"),
        color = "yellow",width=3
      )
    })
    
  }
  #######################  ####################### ####################### 
  ####second part for the HIST tab
  # selectedData1<-reactive({
  #   HS_frame[,Aspects[input$aspect1][[1]]]
  # })
  # show_rating4search<-function(School.Name){
  #   output$rating_hist1 <- renderPlotly({
  #     #plot_scores(selectedData(),input$school_name,input$aspect)
  #     if(is.na(selectedData1()[selectedData1()$School.Name==School.Name,4])){
  #       plot_ly(x=selectedData1()[,4],type = "histogram")
  #     }else{
  #       plot_scores(selectedData1(),School.Name,input$aspect1)
  #     }
  #     
  #   })
  #   ranks_data1<-cal_values(selectedData1(),School.Name)
  #   output$Rank1 <- renderValueBox({
  #     valueBox(
  #       ranks_data1[1],"Rank" , icon = icon("list"),
  #       color = "purple",width=3
  #     )
  #   })
  #   output$Rank_city1 <- renderValueBox({
  #     valueBox(
  #       ranks_data1[2],"City Level" , icon = icon("building-o"),
  #       color = "blue",width=3
  #     )
  #   })
  #   output$Rank_boro1 <- renderValueBox({
  #     valueBox(
  #       ranks_data1[3],"Borough Level" , icon = icon("bicycle"),
  #       color = "yellow",width=3
  #     )
  #   })
  #   
  #   
  # }
  #######################  #######################  #######################  ####################### 
  ####################### Compare Schools ########################3
  output$plot_radar=renderPlotly({
    make_radar(input$radar_school1,input$radar_school2)
  })
  df_select2<-reactive({
    # if(is.na())
    data_merge%>%filter(school.name%in%c(input$radar_school1,input$radar_school2))
  })
  
  marker_opt <- markerOptions(opacity=0.8,riseOnHover=T)
  output$map2 <- renderLeaflet({
    teacher_expr_i<-teacher_expr$Percent.of.teachers.with.3.or.more.years.of.experience[teacher_expr$School.Name%in%df_select2()$school.name]
    learner_i<-language_learner$Percent.English.Language.Learners[language_learner$School.Name%in%df_select2()$school.name]
    disa_i<-disability$Percent.Students.with.Disabilities[disability$School.Name%in%df_select2()$school.name]
    econ_i<-econ_need$Economic.Need.Index[econ_need$School.Name%in%df_select2()$school.name]
    labels_i <- sprintf(
      "<strong><font color=\"#00008b\" size=3>%s</font></strong><br/>
      %s<br/> 
      <strong>%g</strong> students<br/>
      </i> <font color=\"#8b2323\"><i class=\"fa fa-users\"> Teachers with 3 or More Year Expr</i>: <strong>%s %%</strong></font></br>
       <font color=\"#458b0\"><i class=\"fa fa-language\"> English Language Learner</i>: <strong>%s %%</strong></font></br>
      <font color=\"#cd9b1d\"> <i class=\"fa fa-dollar\">  Economic Need Index</i>: <strong>%s %%</strong></font></br>
      <font color=\"#104e8b\"> <i class=\"fa fa-wheelchair\"> Students with Disabilities</i>: <strong>%s %%</strong></font></br>
      ",
      df_select2()$school.name,
      as.character(df_select2()$addr),
      df_select2()$Enrollment,
      #icon("users"),
      round(teacher_expr_i,2)*100,
      #icon("language"),
      round(learner_i,2)*100,
      #icon("dollar"),
      round(econ_i,2)*100,
      #icon("wheelchair"),
      round(disa_i,2)*100
      
    ) %>% lapply(htmltools::HTML)
    
    if(nrow(df_select2())<1){
      #df_new2<-data_merge
      
    }else{
      df_new2<-df_select2()
    }
    # labels <- sprintf(
    #   "<strong><font color=\"#00008b\" size=3>%s</font></strong><br/>
    #   %s<br/> 
    #   <strong>%g</strong> students<br/>
    #   %
    #   ",
    #   df_new2$school.name, as.character(df_new2$addr),df_new2$Enrollment) %>% lapply(htmltools::HTML)
    
    leaflet(data_merge)%>%
      addTiles()%>%
      addMarkers(~lon, ~lat,popup=labels_all_new,label=~school.name,icon=list(iconUrl='icon/school-2.png',iconSize=c(18,18)),group=~borough,options=marker_opt,layerId=~school.name)%>%
      addProviderTiles("OpenStreetMap.HOT")%>%
      addMarkers(df_new2$lon, df_new2$lat,popup=labels_i,label=df_new2$school.name,layerId=c("school1","school2"),options=marker_opt)%>%
      setView(mean(df_new2$lon),mean(df_new2$lat),zoom=12)
    # }
  })
  
  output$Compare2school_SAT<-renderPlotly({
    df<-tidysat%>%
      filter(Name%in%c(input$radar_school1,input$radar_school2))
    m_sat <- list(
      l = 20,
      r = 20,
      b = 30,
      t =0
    )
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    plot_ly()%>%
      add_trace(y =~Score,x=~Sub,data=df%>%filter(Name==input$radar_school1),type = "bar",name=input$radar_school1,marker=list(color = "#20b2aa"))%>%
      add_trace(y =~Score,x=~Sub,data=df%>%filter(Name==input$radar_school2),type = "bar",name=input$radar_school2,marker=list(color="#b0c4de"))%>%
      layout(yaxis = ax, xaxis =list(title=""),height=250,
             barmode = 'group',legend=list(orientation = 'h'),margin=m_sat)
    
    
  })
  
  
  
  observeEvent(input$click_school1,{
    event<-input$map2_marker_click
    print(event)
    #change_school1(event$id)
    updateSelectInput(session, "radar_school1",
                      #label = strong(icon("hand-o-right"),"Select 1st School",style="color:LightSeaGreen"),
                      #choices =as.vector(schoolname) ,
                      selected =event$id)
  })
  observeEvent(input$click_school2,{
    event<-input$map2_marker_click
    print(event)
    #change_school1(event$id)
    updateSelectInput(session, "radar_school2",
                      #label = strong(icon("hand-o-right"),"Select 2nd School",style="color:LightSeaGreen"),
                      #choices =as.vector(schoolname) ,
                      selected =event$id)
  })
  
  #######################  #######################  #######################  ####################### 
  ######################## Rank Tab ##########################
  output$datarankedquant <- renderDataTable(data.ranked.quant,options=list(pageLength = 10))
  output$datarankedmath <- renderDataTable(data.ranked.math,options=list(pageLength = 10))
  output$datarankedenglish <- renderDataTable(data.ranked.english,options=list(pageLength = 10))
  output$datarankedhistory <- renderDataTable(data.ranked.history,options=list(pageLength = 10))
  output$datarankedscience <- renderDataTable(data.ranked.science)
  output$datarankedallsubjects <- renderDataTable(data.ranked.allsubjects,options=list(pageLength = 10))
  output$datarankedRI <- renderDataTable(data.ranked.RI,options=list(pageLength = 10))
  output$datarankedCT <- renderDataTable(data.ranked.CT,options=list(pageLength = 10))
  output$datarankedSE <- renderDataTable(data.ranked.SE,options=list(pageLength = 10))
  output$datarankedESL <- renderDataTable(data.ranked.ESL,options=list(pageLength = 10))
  output$datarankedSFCT <- renderDataTable(data.ranked.SFCT,options=list(pageLength = 10))
  output$datarankedT <- renderDataTable(data.ranked.T,options=list(pageLength = 10))
  output$datarankedallsurvey <- renderDataTable(data.ranked.allsurvey,options=list(pageLength = 10))
  
  
  
  #######################  #######################  #######################  ####################### 
  ##############Stat Analysis ####################### 
  df.list = list(rigorous.df,teachers.df,supportive.df,
                 leadership.df,community.df,trust.df)
  names(df.list)= 1:6
  
  output$lmPlot <- renderPlot({
    load.plot = function(df, x, y, x_colname, y_colname) {
      ggplot(subset(df, !is.na(df)), aes_string(x=x, y=y)) + 
        geom_point(na.rm=TRUE, aes(color = factor(borough)), size=2) +
        geom_smooth(method=lm,na.rm=TRUE)+
        labs(x = x_colname,y=y_colname)
    }
    
    df = df.list[[input$x1]]
    y_string= input$y
    if(input$x1==1){
      x_string = colnames(df[as.numeric(input$x21)+4])
      x_colname = rigorous.col[[x_string]]
    }
    else if(input$x1==2){
      x_string = colnames(df[as.numeric(input$x22)+4])
      x_colname = teachers.col[[x_string]]
    }
    else if(input$x1==3){
      x_string = colnames(df[as.numeric(input$x23)+4])
      x_colname = supportive.col[[x_string]]
    }
    else if(input$x1==4){
      x_string = colnames(df[as.numeric(input$x24)+4])
      x_colname = leadership.col[[x_string]]
    }
    else if(input$x1==5){
      x_string = colnames(df[as.numeric(input$x25)+4])
      x_colname = community.col[[x_string]]
    }
    else if(input$x1==6){
      x_string = colnames(df[as.numeric(input$x26)+4])
      x_colname = trust.col[[x_string]]
    }
    
    set.seed(123)
    load.plot(df,x_string,y_string,x_colname,achievements.col[[y_string]])
    
  })
  output$y_value <-renderInfoBox({
    df = df.list[[input$x1]]
    y_string= input$y
    if(input$x1==1){
      x_string = colnames(df[as.numeric(input$x21)+4])
      x_colname = rigorous.col[[x_string]]
    }
    else if(input$x1==2){
      x_string = colnames(df[as.numeric(input$x22)+4])
      x_colname = teachers.col[[x_string]]
    }
    else if(input$x1==3){
      x_string = colnames(df[as.numeric(input$x23)+4])
      x_colname = supportive.col[[x_string]]
    }
    else if(input$x1==4){
      x_string = colnames(df[as.numeric(input$x24)+4])
      x_colname = leadership.col[[x_string]]
    }
    else if(input$x1==5){
      x_string = colnames(df[as.numeric(input$x25)+4])
      x_colname = community.col[[x_string]]
    }
    else if(input$x1==6){
      x_string = colnames(df[as.numeric(input$x26)+4])
      x_colname = trust.col[[x_string]]
    }
    infoBox(
      "Y Axis", achievements.col[[y_string]],
      color = if (y_string == "Graduate_Pct"){"purple"} else {"blue"},
      icon = if (y_string == "Graduate_Pct"){icon("graduation-cap")} else {icon("university")}
    )
  })
  output$x_value <-renderInfoBox({
    df = df.list[[input$x1]]
    y_string= input$y
    if(input$x1==1){
      x_string = colnames(df[as.numeric(input$x21)+4])
      x_colname = rigorous.col[[x_string]]
      color = "fuchsia"
      icon = icon("book")
    }
    else if(input$x1==2){
      x_string = colnames(df[as.numeric(input$x22)+4])
      x_colname = teachers.col[[x_string]]
      color = "maroon"
      icon = icon("commenting")
    }
    else if(input$x1==3){
      x_string = colnames(df[as.numeric(input$x23)+4])
      x_colname = supportive.col[[x_string]]
      color="orange"
      icon = icon("heart-o")
    }
    else if(input$x1==4){
      x_string = colnames(df[as.numeric(input$x24)+4])
      x_colname = leadership.col[[x_string]]
      color="black"
      icon = icon("star-o")
    }
    else if(input$x1==5){
      x_string = colnames(df[as.numeric(input$x25)+4])
      x_colname = community.col[[x_string]]
      color="teal"
      icon = icon("users")
    }
    else if(input$x1==6){
      x_string = colnames(df[as.numeric(input$x26)+4])
      x_colname = trust.col[[x_string]]
      color="aqua"
      icon = icon("handshake-o")
    }
    
    infoBox(
      "X Axis", x_colname,
      color = color,
      icon = icon
    )
  })
  output$significance <- renderValueBox({ 
    df = df.list[[input$x1]]
    y_string= input$y
    if(input$x1==1){
      x_string = colnames(df[as.numeric(input$x21)+4])
      x_colname = rigorous.col[[x_string]]
    }
    else if(input$x1==2){
      x_string = colnames(df[as.numeric(input$x22)+4])
      x_colname = teachers.col[[x_string]]
    }
    else if(input$x1==3){
      x_string = colnames(df[as.numeric(input$x23)+4])
      x_colname = supportive.col[[x_string]]
    }
    else if(input$x1==4){
      x_string = colnames(df[as.numeric(input$x24)+4])
      x_colname = leadership.col[[x_string]]
    }
    else if(input$x1==5){
      x_string = colnames(df[as.numeric(input$x25)+4])
      x_colname = community.col[[x_string]]
    }
    else if(input$x1==6){
      x_string = colnames(df[as.numeric(input$x26)+4])
      x_colname = trust.col[[x_string]]
    }
    formula <- paste(y_string, "~", x_string)
    fit = lm(formula, data=df.list[[input$x1]], na.action = na.exclude)
    p_val = summary(fit)$coefficient[2,4]
    
    valueBox(
      value = if (p_val<=0.001){"Very significantly correlated"}
      else if(p_val<=0.01){"Significantly correlated"}
      else if(p_val<=0.05){"Somewhat significantly correlated"}
      else {"Not significantly correlated"},
      subtitle = paste("Based on the regression analysis, p value is",format(p_val, scientific=T,digits=3),"for",x_colname),
      color = if (p_val<=0.001){"green"}
      else if(p_val<=0.01){"olive"}
      else if(p_val<=0.05){"yellow"}
      else {"red"},
      icon = if (p_val<=0.001){icon("thumbs-up")}
      else if(p_val<=0.01){icon("thumbs-up")}
      else if(p_val<=0.05){icon("question")}
      else {icon("thumbs-o-down")}
    )}
  )
  
  output$lm_title<-renderUI({
    h3(icon("comments"),"How do the survey results relate to the graduation rate & college enrollment rate?")
    # sprintf(
    #   
    #   "<strong>%s <font color=\"#1a1a1a\" size=4>How do the survey results relate to the graduation rate & college enrollment rate?</font></strong>",icon("comments-o","fa-3x"))%>%lapply(htmltools::HTML)
  })
  
  ############# CALCULATOR #########
  output$values <- renderTable({
    n = c(0,0,0,0,0,0,0,0,0)
    vec = c("Graduation Rate","College Enrollment Rate","Teacher Experience","Rigorous Instruction","Collaborative Teachers","Supportive Environment","Effective School Leadership","Strong Family-Community Ties","Trust")
    inputchoice = c(which(vec == input$choice1),which(vec == input$choice2),which(vec == input$choice3))
    inputvalue = c(as.numeric(input$var1),as.numeric(input$var2),as.numeric(input$var3))
    n[inputchoice] = inputvalue
    new = data.frame(mapply(`*`,vardata,n))
    new2 = data.frame(sum=rowSums(new))
    lst = sort(new2[,1], index.return=TRUE, decreasing=TRUE)
    top5 = lapply(lst, `[`, lst$x %in% head(unique(lst$x),5))$ix
    vecnew = c(1,inputchoice+1)
    var2data[top5,][vecnew]
  })
 
  
} 
