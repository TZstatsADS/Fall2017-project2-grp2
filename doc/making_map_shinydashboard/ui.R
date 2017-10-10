
header <- dashboardHeader(
  title = "High Schools in NYC",disable = TRUE
)


sidebar <- dashboardSidebar(collapsed = T,width =200,disable = TRUE,
                            sidebarMenu(
                              menuItem("Schools Map", tabName = "Schools_Map", icon = icon("map")),
                              # menuItem("6 RATINGS", icon = icon("th"), tabName = "6ratings",
                              #          badgeLabel = "haha", badgeColor = "green"),
                              menuItem("Compare 2 Schools", icon = icon("balance-scale"), tabName = "Compare",
                                       badgeLabel = "haha", badgeColor = "green")
                            )
)
s3<-dashboardSidebar(width =500,
                     sidebarMenu(
                       menuItem("Introduction", tabName = "Intro", icon = icon("home"))))
b3<-dashboardBody(tabItems())

body <- dashboardBody(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  # ),
  # 
  # tabItems(
  navbarPage(strong(icon("university"),"NYC High Schools",style="color: Green;"), theme="styles.css",
             # 1.INTRO TAB
             tabPanel(strong(icon("map"), "Schools Map"),
                      tabItem(tabName =strong(icon("map"), "Schools_Map"),
                              fluidRow(
                                column(width=2,
                                       box(id="controls1",width=NULL,
                                                     # draggable = T,top = 90, left = 60, right = "auto", bottom = "auto", 
                                                     # height = 'auto',
                                                     p(" "),
                                                     selectInput("map_color", "Map Color Theme", 
                                                                 choices=c("Original"="OpenStretMap.France", "Green & Blue"="Hydda.Full","Black & White"="Stamen.TonerLite")
                                                                 ,selected = "Original"),
                                                     #selectInput("Boro_map","Borough",choices = unique(data_merge$borough)),
                                                     hr(),
                                                     selectInput("Zipcode", "Zipcode:",
                                                                 choices = c("All",sort(unique(data_merge$zipcode)))
                                                     ),
                                                     checkboxInput("Near", "Show Schools in the nearby area",T),
                                                     hr(),
                                                     sliderInput("Enroll",
                                                                 "Number of Students:",
                                                                 min = 1,  max = 5500, value = c(0,5500))
                                       )   
                                ),
                                
                                
                                column(width = 8,
                                       div(leafletOutput("map", height = 380))
                                )
                                ,
                                column(width = 2,
                                       uiOutput("ratings")
                                )),
                              #hr(),
                              tabsetPanel(id="SummaryTabSet",
                                          tabPanel(p(icon("th"),"Main",style="color: grey;"),value="MapSetting",
                                                   #strong(icon("university"),"hahhhha",size=10),
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
                                                                  box(title=strong(icon("graduation-cap"),"6 Months After Grad"),width=NULL,status = "success",solidHeader = T,
                                                                      plotlyOutput("show_6_m",height = 200)
                                                                  )
                                                           ),
                                                           column(width =3,
                                                                  
                                                                  box(title=strong(icon("graduation-cap"),"18 Months After Grad"),width=NULL,status = "success",solidHeader = T,
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
                                                         fluidRow(box(width = 12,background = "olive",p(icon("dollar"),ECON_EXPLAIN)))
                                                         
                                                         
                                                     ))),
                                          
                                          
                                          ##the 2nd Tab
                                          tabPanel(p(icon("search"),"Search Your School",style="color:grey"),value="Search",
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
                                                            box(title=strong(icon("graduation-cap"),"6 Months After Grad"),width=NULL,status = "success",
                                                                plotlyOutput("show_6_m1",height = 250)
                                                            )
                                                     ),
                                                     column(width =3,
                                                            
                                                            box(title=strong(icon("graduation-cap"),"18 Months After Grad"),width=NULL,status = "success",
                                                                plotlyOutput("show_18_m1",height = 250))
                                                            
                                                     ),
                                                     column(width =3,
                                                            
                                                            box(title=strong(icon("bar-chart"),"SAT Score"),width=NULL,status = "danger",
                                                                plotlyOutput("SAT1",height = 250))
                                                            
                                                     )
                                                   ),
                                                   
                                                   fluidRow(
                                                     column(width = 3,
                                                            
                                                            box(title=strong(icon("users"),"Teachers Experience"),width=NULL,solidHeader = T,
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
                                                   fluidRow(box(width = 12,p(icon("dollar"),ECON_EXPLAIN),background = "olive"))
                                                   
                                          ),
                                          #The 3rd tab
                                          tabPanel(p(icon("info-circle"),"Details for Summary Items",style="color:grey"),value="details for aspects",
                                                   
                                                   fluidRow(
                                                     
                                                     box(width=NULL,
                                                         column(width=6,selectInput('aspect', strong(icon("search"),'Select a Item'),names(Aspects),
                                                                                    selected=names(Aspects)[1]))
                                                         #)
                                                         ,
                                                         #box(width = 6,status = "warning",
                                                         column( width=6,   
                                                                 selectInput('school_name2', strong(icon("university"),'Select a School'),c(HS_frame$School.Name," "),selected = " ")
                                                         ))
                                                     
                                                   ),
                                                   fluidRow(box(width = 12,status = "danger",uiOutput("hist_School"),hr(),
                                                                # )),
                                                                fluidRow(
                                                                  column(width =8,
                                                                         box(width = NULL,
                                                                             plotlyOutput('rating_hist')))
                                                                  ,
                                                                  # column(width =NULL,
                                                                  column(width =4,    
                                                                         valueBoxOutput('Rank',width = NULL),
                                                                         valueBoxOutput('Rank_boro',width = NULL),
                                                                         box(width = NULL,title=strong("Positive Responses",icon("thumbs-up")),
                                                                             valueBoxOutput('Rank_city',width = NULL)
                                                                         )
                                                                  )
                                                                  #valueBox(verbatimTextOutput("Rank"), "Rank", icon = icon("credit-card"))
                                                                )))
                                          )
                                          
                              ))),
             tabPanel(strong(icon("balance-scale"),"Compare 2 Schools"),
                      
                      div(leafletOutput("map2", height = 700)),
                      
                      # tabItem(tabName = "Compare",
                      tags$head(tags$style(
                        HTML('
                             #input_date_control {background-color: rgba(0,0,0,0.2);;}
                             #sel_date {background-color: #FFFFFF;}')
                      )),
                      absolutePanel( id = "controls", class = "panel panel-default", fixed = TRUE
                                     , draggable = T,top = 90, left = 60, right = "auto", bottom = "auto", width = 330,
                                     height = 'auto',
                                     
                                     #h4("Compare 2 Schools"), 
                                     p(""),
                                     selectInput("radar_school1", strong(icon("hand-o-right"),"Select 1st School",style="color:LightSeaGreen"),
                                                 choices = as.vector(schoolname), selected = "MANHATTAN ACADEMY FOR ARTS & LANGUAGE",width = 330),
                                     #h4(icon("hand-o-up")," VS ",icon("hand-o-down")),
                                     # uiOutput("radar_Boro1"),
                                     # uiOutput("radar_school1"),
                                     actionButton("click_school1", "Set the current school 1st School"),
                                     hr(),
                                     # uiOutput("radar_Boro2"),
                                     # uiOutput("radar_school2"),
                                     selectInput("radar_school2", strong(icon("hand-o-right"),"Select 2nd School",style="color:SteelBlue"),
                                                 choices =as.vector(schoolname), selected = "Frances Perkins Academy",width = 330)
                                     ,
                                     actionButton("click_school2", "Set the current school 2nd School"),
                                     hr(),
                                     h4("Avg. SAT Scores of 2 Schools"),
                                     plotlyOutput("Compare2school_SAT",height = 270)
                      )
                      ,
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE
                                    , draggable = T,top = 120, left = "auto", right = 50, bottom = "auto", width = 350,
                                    height = 'auto',
                                    #column(width=6,
                                    h4("Radar Plot for the 6 Quatitative Assessment"),
                                    plotlyOutput("plot_radar",height = 400)
                                    
                                    #)
                      )
                      
             ),
             tabPanel(strong(icon("star"),"Rank"),
                      fluidPage(
                        
                        #titlePanel("NYC High School Rankings"),
                        
                        navlistPanel(widths = c(3, 8),
                                     
                                     tabPanel(strong(icon("home"),"Intro"),
                                              tabItem(tabName="Intro",
                                                      h2("A Ranking of High Schools in the City of New York"),
                                                      tags$hr(),
                                                      h4("This Ranking App was created with the use of data",
                                                         "\ from the latest School Quality Report published by the NYC",
                                                         "\ Department of Education (the 2014-15 Edition)."),
                                                      tags$br(),
                                                      h4(tags$a(href="https://data.cityofnewyork.us/Education/2014-2015-School-Quality-Reports-Results-For-High-/vrfr-9k4d", "The link to the Report can be accessed by clicking on this text.")),
                                                      tags$br()
                                              )
                                     ),
                                     tabPanel(strong(icon("graduation-cap"),"Diploma Completion"),
                                              tabItem(tabName="Diplo",
                                                      h1(tags$b("Diploma Completion")),
                                                      tags$p("These rankings were calculated by averaging the scores of arguably the most important quantitative metrics that determine a school's success:"),
                                                      tags$ul(
                                                        tags$li("The Graduation Rate"),
                                                        tags$li("The rate at which Students enter College/University within the first 6 months of Graduation")
                                                      ),
                                                      tags$br(),
                                                      fluidRow(
                                                        dataTableOutput("datarankedquant")
                                                      )
                                              )
                                     ),
                                  
                                     tabPanel(strong(icon("trophy"),"Academic Performance"),
                                              tabsetPanel(tabPanel("Math",
                                                         tabItem(tabName="Mathyo",
                                                                 h1("Performance in", tags$b("Mathematics")),
                                                                 tags$br(),
                                                                 fluidRow(
                                                                   dataTableOutput("datarankedmath")
                                                                   
                                                                 )
                                                         )),
                                                tabPanel("English",
                                                         tabItem(tabName="Englishyo",
                                                                 h1("Performance in", tags$b("English")),
                                                                 tags$br(),
                                                                 fluidRow(
                                                                   dataTableOutput("datarankedenglish")
                                                                   
                                                                 )
                                                         )
                                                ),
                                                tabPanel("History",
                                                         tabItem(tabName="Historyyo",
                                                                 h1("Performance in", tags$b("History")),
                                                                 tags$br(),
                                                                 fluidRow(
                                                                   dataTableOutput("datarankedhistory")
                                                                   
                                                                 )
                                                         )
                                                ),
                                                tabPanel("Science",
                                                         h1("Performance in", tags$b("Science")),
                                                         tags$br(),
                                                         fluidRow(
                                                           dataTableOutput("datarankedscience")
                                                           
                                                         )
                                                ),
                                                tabPanel("All Subjects",
                                                         h1("Performance in", tags$b("All Subjects")),  
                                                         fluidRow(
                                                           dataTableOutput("datarankedallsubjects")
                                                           
                                                         )
                                                ))
                                     )
                                     ,
                                     #"<font size=3>Header</li>"%>%lapply(htmltools::HTML),
                                     tabPanel(strong(icon("thumbs-up"),"Qualitative Assessment"),
                                                tabsetPanel(
                                                tabPanel("<font color=grey>Rigorous</br>Instruction</font>"%>%lapply(htmltools::HTML),
                                                         h1("Performance in", tags$b("Rigorous Instruction")),
                                                         tags$p("These rankings were calculated by averaging the scores of the following Qualitative Survey questions included in the Report."),
                                                         tags$ul(
                                                           tags$li("% of Students who say they learn a lot from feedback"),
                                                           tags$li("% of Students who know what their teacher wants them to learn from class"),
                                                           tags$li("% of Teachers who say that students build on each others' ideas during class")
                                                         ),
                                                         tags$br(),
                                                         fluidRow(
                                                           dataTableOutput("datarankedRI")
                                                           
                                                         )
                                                         ),
                                                tabPanel("<font color=grey>Collaborative</br>Teachers</font>"%>%lapply(htmltools::HTML),
                                                         h1("Performance in", tags$b("Collaborative Teachers")),
                                                         tags$p("These rankins were calculated by averaging the scores of the following Qualitative Survey questions included in the Report."),
                                                         tags$ul(
                                                           tags$li("% of Teachers who say that they work together to design instructional programs"),
                                                           tags$li("% of Teachers who say that they have opportunities to work productively with colleagues"),
                                                           tags$li("% of Teachers who say that they feel responsible that all students learn")
                                                         ),
                                                         tags$br(),
                                                         fluidRow(
                                                           dataTableOutput("datarankedCT")
                                                           
                                                         )
                                                         ),
                                                tabPanel("<font color=grey>Supportive</br>Environment</font>"%>%lapply(htmltools::HTML),
                                                         h1("Performance in", tags$b("Supportive Environment")),
                                                         tags$p("These rankings were calculated by averaging the scores of the following Qualitative Survey questions included in the Report."),
                                                         tags$ul(
                                                           tags$li("% of Students who say that they feel safe in the hallways, bathrooms, locker room, and cafeteria"),
                                                           tags$li("% of Students who say that teachers notice when they are upset or have emotional difficulty"),
                                                           tags$li("% of Students who say that school supports navigating post-secondary success"),
                                                           tags$li("% of Students with 90 percent or more attendance")
                                                         ),
                                                         tags$br(),
                                                         fluidRow(
                                                           dataTableOutput("datarankedSE")
                                                           
                                                         )
                                                         ),
                                                tabPanel("<font color=grey>Effective School</br>Leadership</font>"%>%lapply(htmltools::HTML),
                                                         h1("Performance in", tags$b("Effective School Leadership")),
                                                         tags$p("These rankings were calculated by averaging the scores of the following Qualitative Survey questions included in the Report."),
                                                         tags$ul(
                                                           tags$li("% of Teachers who say that the Principal communicates a clear vision for the school"),
                                                           tags$li("% of Teachers who say that curriculum and instruction are well coordinated across different grade levels"),
                                                           tags$li("% of Parents who feel that the Principal works to create a sense of community in the school")
                                                         ),
                                                         tags$br(),
                                                         fluidRow(
                                                           dataTableOutput("datarankedESL")
                                                           
                                                         )
                                                         ),
                                                tabPanel("<font color=grey>Strong Family</br>Community Ties</font>"%>%lapply(htmltools::HTML),
                                                         h1("Performance in", tags$b("Strong Family-Community Ties")),
                                                         tags$p("These rankings were calculated by averaging the scores of the following Qualitative Survey questions included in the Report."),
                                                         tags$ul(
                                                           tags$li("% of Parents who say that school staff regularly communicate with them about how the staff can help their children learn"),
                                                           tags$li("% of Parents who feel that teachers try to understand families' problems and concerns"),
                                                           tags$li("% of Teachers who say that teachers at this school work closely with families to meet students' needs")
                                                         ),
                                                         tags$br(),
                                                         fluidRow(
                                                           dataTableOutput("datarankedSFCT")
                                                           
                                                         )
                                                         ),
                                                tabPanel("<font color=grey>Trust</br> </br></font>"%>%lapply(htmltools::HTML),
                                                         h1("Performance in", tags$b("Trust")),
                                                         tags$p("These rankings were calculated by averaging the scores of the following Qualitative Survey questions included in the Report."),
                                                         tags$ul(
                                                           tags$li("% of Teachers who say that they trust the Principal"),
                                                           tags$li("% of Teachers who say that they trust each other"),
                                                           tags$li("% of Parents who say that school staff work hard to build lasting relationships with them"),
                                                           tags$li("% of Students who say that Teachers treat them with respect")
                                                         ),
                                                         tags$br(),
                                                         fluidRow(
                                                           dataTableOutput("datarankedT")
                                                           
                                                         )
                                                         ),
                                                tabPanel("<font color=grey>All</br>Quantative Areas</font>"%>%lapply(htmltools::HTML),
                                                         h1("Performance in", tags$b("All Qualitative Areas")),
                                                         tags$p("These rankings were calculated by averaging the scores of all six assessment areas."),
                                                         tags$ul(
                                                           tags$li("Rigorous Instruction"),
                                                           tags$li("Collaborative Teachers"),
                                                           tags$li("Supportive Environment"),
                                                           tags$li("Effective School Leadership"),
                                                           tags$li("Strong Family-Community Ties"),
                                                           tags$li("Trust")
                                                         ),
                                                         tags$br(),
                                                         fluidRow(
                                                           dataTableOutput("datarankedallsurvey")
                                                           
                                                         )
                                                         )
                                                )
                                              )
                                     
                        ))
             )
             
             
  )
)






dashboardPage(
  header,
  sidebar,
  body
)
