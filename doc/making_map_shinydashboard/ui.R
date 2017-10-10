

header <- dashboardHeader(
  title = "High Schools in NYC"
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Schools Map", tabName = "Schools_Map", icon = icon("map")),
    menuItem("6 RATINGS", icon = icon("th"), tabName = "6ratings",
             badgeLabel = "haha", badgeColor = "green"),
    menuItem("Compare 2 Schools", icon = icon("th"), tabName = "Compare",
             badgeLabel = "haha", badgeColor = "green")
  )
)

body <- dashboardBody(
  #tags$head(includeCSS("styles.css")),
  tabItems(
    
    tabItem(tabName = "Schools_Map",
            fluidRow(
              column(width = 9,
                     box(width = NULL, solidHeader = TRUE,
                         leafletOutput("map", height = 590)
                     )
              )
              ,
              column(width = 3,
                     box(width = NULL, status = "warning",
                         
                         selectInput("map_color", "Map Color Theme", 
                                     choices=c("Original"="OpenStretMap.HOT", "Green & Blue"="Hydda.Full","Black & White"="Stamen.TonerLite")
                                     ,selected = "Original"),
                         hr(),
                         selectInput("Zipcode", "Zipcode:",
                                     choices = c("All",sort(unique(data_merge$zipcode)))
                         ),
                         checkboxInput("Near", "Show Schools in the nearby area",T),
                         
                         sliderInput("Enroll",
                                     "Number of Students:",
                                     min = 1,  max = 5500, value = c(0,5500))
                         
                         ,
                         
                         uiOutput("ratings")
                     )
                     
              )),
            tabsetPanel(id="SummaryTabSet",
                        tabPanel("Main",value="MapSetting",
                                 fluidRow(
                                   box(width=NULL,status = "danger",uiOutput("selected_school"),
                                       #       )
                                       # ),
                                       
                                       fluidRow(hr()),
                                       fluidRow(
                                         column(width = 3,
                                                box(title=strong(icon("user-circle-o"),"Proportion of Races"),width=NULL,status = "warning",solidHeader = T,
                                                    plotlyOutput("race",height = 200)
                                                )
                                         ),
                                         column(width =3,
                                                box(title=strong(icon("graduation-cap"),"Post-Secondary Status-6 Month"),width=NULL,status = "success",solidHeader = T,
                                                    plotlyOutput("show_6_m",height = 200)
                                                )
                                         ),
                                         column(width =3,
                                                
                                                box(title=strong(icon("graduation-cap"),"Post-Secondary Status-18 Month"),width=NULL,status = "success",solidHeader = T,
                                                    plotlyOutput("show_18_m",height = 200))
                                                
                                         ),
                                         column(width =3,
                                                
                                                box(title=strong(icon("bar-chart"),"SAT Score"),width=NULL,status = "danger",solidHeader = T,
                                                    plotlyOutput("SAT",height = 200))
                                         )
                                       ),
                                       fluidRow(hr()),
                                       fluidRow(
                                         column(width = 3,
                                                
                                                box(title=strong(icon("users"),"Teachers Experience"),width=NULL,status = ,solidHeader = F,
                                                    #plotlyOutput("Teacher",height = 200)
                                                    valueBoxOutput('Teacher',width = NULL)
                                                )),
                                         column(width =3,
                                                
                                                box(title=strong(icon("dollar"),"Economic Need Index"),width=NULL,solidHeader = F,
                                                    #plotlyOutput("Teacher",height = 200)
                                                    valueBoxOutput('Econ',width = NULL)
                                                )
                                         ),
                                         column(width =3,
                                                box(title=strong(icon("wheelchair"),"Students with Disabilities"),width=NULL,solidHeader = F,
                                                    #plotlyOutput("Teacher",height = 200)
                                                    valueBoxOutput('disa',width = NULL)
                                                )
                                         ),
                                         column(width =3,
                                                box(title=strong(icon("language"),"English Language Learners"),width=NULL,solidHeader = F,
                                                    #plotlyOutput("Teacher",height = 200)
                                                    valueBoxOutput('learner',width = NULL)
                                                )
                                         )
                                       ),
                                       fluidRow(box(width = 12,uiOutput("explain"),background = "olive"))
                                       
                                       
                                   ))),
                        tabPanel("Search Your School",value="Search",
                                 fluidRow(
                                   box(width = 12,column(width=6,uiOutput("Boro_1")),
                                       column(width=6,uiOutput("School_1")),
                                       column(width=4,checkboxInput("label_it", "Label The Schools On Map",T)))
                                   
                                 ),
                                 fluidRow(
                                   column(width = 3,
                                          box(title=strong(icon("user-circle-o"),"Proportion of Races"),width=NULL,status = "warning",
                                              plotlyOutput("race1",height = 250)
                                              
                                          )
                                   ),
                                   column(width =3,
                                          box(title=strong(icon("graduation-cap"),"Post-Secondary Status-6 Month"),width=NULL,status = "success",
                                              plotlyOutput("show_6_m1",height = 250)
                                          )
                                   ),
                                   column(width =3,
                                          
                                          box(title=strong(icon("graduation-cap"),"Post-Secondary Status-18 Month"),width=NULL,status = "success",
                                              plotlyOutput("show_18_m1",height = 250))
                                          
                                   ),
                                   column(width =3,
                                          
                                          box(title=strong(icon("bar-chart"),"SAT Score"),width=NULL,status = "danger",
                                              plotlyOutput("SAT1",height = 250))
                                          
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(width = 3,
                                          
                                          box(title=strong(icon("users"),"Teachers Experience"),width=NULL,status = ,solidHeader = F,
                                              #plotlyOutput("Teacher",height = 200)
                                              valueBoxOutput('Teacher1',width = NULL)
                                          )),
                                   column(width =3,
                                          
                                          box(title=strong(icon("dollar"),"Economic Need Index"),width=NULL,solidHeader = F,
                                              #plotlyOutput("Teacher",height = 200)
                                              valueBoxOutput('Econ1',width = NULL)
                                          )
                                   ),
                                   column(width =3,
                                          box(title=strong(icon("wheelchair"),"Students with Disabilities"),width=NULL,solidHeader = F,
                                              #plotlyOutput("Teacher",height = 200)
                                              valueBoxOutput('disa1',width = NULL)
                                          )
                                   ),
                                   column(width =3,
                                          box(title=strong(icon("language"),"English Language Learners"),width=NULL,solidHeader = F,
                                              #plotlyOutput("Teacher",height = 200)
                                              valueBoxOutput('learner1',width = NULL)
                                          )
                                   )
                                 ),
                                 fluidRow(box(width = 12,uiOutput("explain1"),background = "olive"))
                        )
            )),
    
    tabItem(tabName = "6ratings",
            #h2("Dashboard tab content")
            
            fluidRow(
              box(width = 6,status = "warning",
                  selectInput('school_name2', 'School',HS_frame$School.Name)),
              box(width=6,status = "warning",
                  selectInput('aspect', 'Item',names(Aspects),
                              selected=names(Aspects)[1]))
            ),
            fluidRow(box(width = 12,status = "danger",uiOutput("hist_School"))),
            fluidRow(
              column(width =8,
                     box(width = NULL,status = "primary",
                         plotlyOutput('rating_hist')))
              ,
              # column(width =NULL,
              column(width =4,    
                     valueBoxOutput('Rank',width = NULL),
                     
                     box(width = NULL,title=sprintf("%s <strong>Positive Responses on Survey",icon("thumbs-up"))%>%lapply(htmltools::HTML),
                         valueBoxOutput('Rank_city',width = NULL),
                         valueBoxOutput('Rank_boro',width = NULL))
              )
              #valueBox(verbatimTextOutput("Rank"), "Rank", icon = icon("credit-card"))
            )
            
    )
  )
)





dashboardPage(
  header,
  sidebar,
  body
)
