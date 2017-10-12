
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
             tabPanel(strong(icon("bookmark-o"),"Intro"),div(id="bg",
                                                             absolutePanel(width=1200,left=50,right=30,
                                                                       h1("Project: an RShiny app development project"),
                                                                       h4("We will recommend you open this app with Safari", style="color:#ff4040"),
                                                                       h2("Summary"),
                                                                       p(" We developed this app using R Shiny to help parents decide where to send their children to high school in New York City. In this app, users can compare high schools with one another and can, thus, find the best school on the basis of what they value the most, including school location, teacher performance, student achievement, and other factors. Our app makes it easier for users to understand the relative strengths and weaknesses of each school through data visualization (in the form of maps, scatter plots, bar charts, pie charts, and radar charts). We used a specific dataset" ,tags$a(href="https://data.cityofnewyork.us/Education/2014-2015-School-Quality-Reports-Results-For-High-/vrfr-9k4d","2014-2015 School Quality Reports Results for High Schools from data.gov"),". The dataset includes information pertaining to categories like enrollment, diversity, graduation rate, post-secondary enrollment status, SAT scores, ACT scores, regents scores, and school framework survey results for a total of 847 high schools in New York City from 2014 to 2015."),
                                                                       # p("This project explores and visualizes information of high schools in New York City by" ,tags$a(href="https://data.cityofnewyork.us/Education/2014-2015-School-Quality-Reports-Results-For-High-/vrfr-9k4d", "487 high schools data from 2014 to 2015 on NYC Open Data Portal"),". We created a Shiny App to help parents decide the ideal high school for their children in New York."),
                                                                       h2("Content"),
                                                                       h4(strong(icon("map"),"School Map")),
                                                                       p("   -  ", strong(icon("home")," Main"),": Find schools on the map on the basis of zip code and total student number, and see your selected school's detailed information regarding, for example, diversity, post-secondary enrollment status and SAT scores."),
                                                                       p("   -  ", strong(icon("search"),"Find Schools"),": Select a school you are interested in and see its location on the map and its detailed information."),
                                                                       p("   -  ", strong(icon("info-circle"),"Details for Summary Items"),": Select a school you are interested in and see the school's overall framework survey performance compared to other schools' in its borough or in the city as a whole."),
                                                                       # em(),
                                                                       h4(strong(icon("balance-scale")," Compare 2 Schools")),
                                                                       p("Use radar charts to compare any two schools' relative strengths and weaknesses."),
                                                                       # br(),
                                                                       h4(strong(icon("star"),"Rank")),
                                                                       p("Rank the schools on the basis of factors you care about the most and see the top 10 results."),
                                                                       # br(),
                                                                       h4(strong(icon("calculator"),"Calculator")),
                                                                       p("Select the three factors most important to you and then calculate (from 1 to 10) the importance of each factor in order to identify the corresponding top five schools."),
                                                                       # br(),
                                                                       h4(strong(icon("line-chart"),"Statistical Analysis")),
                                                                       p("Look into how a school's framework survey performance is related to the school's graduation rate and college-enrollment rate, select the features you like, and see whether they are significantly correlated."),
                                                                      br()
                                                                      # ,
                                                             )
                                                             
                                                             )),
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
                                       div(leafletOutput("map", height = 380),
                                           absolutePanel(id="controls",top = 330,left=40,draggable = T,height = 40,
                                                         checkboxInput("label_it", "Label The Schools On Map",T))
                                           )
                                )
                                ,
                                absolutePanel(right=20,width = 200,
                                       uiOutput("ratings")
                                       #hr(),
                                       #checkboxInput("label_it", "Label The Schools You Clicked",T)
                                )),
                              #hr(),
                              tabsetPanel(id="SummaryTabSet",
                                          tabPanel(p(icon("home"),"Main",style="color: grey;"),value="MapSetting",
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
                                          tabPanel(p(icon("search"),"Find Schools",style="color:grey"),value="Search",
                                                   fluidRow(
                                                     box(width = 12,column(width=6,
                                                                          uiOutput("Boro_1")
                                                                           ),status = "warning",
                                                         column(width=6,
                                                                uiOutput("School_1")
                                                                )
                                                         # ,
                                                         # column(width=4,checkboxInput("label_it", "Label The Schools On Map",T))
                                                         )
                                                     
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
                                                         column(width=6,selectInput('aspect', strong(icon("search"),'Select a Assessment Item'),names(Aspects),
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
                                     h4(strong("Avg. SAT Scores of 2 Schools",style="color:grey;")),
                                     plotlyOutput("Compare2school_SAT",height = 270)
                      )
                      ,
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE
                                    , draggable = T,top = 120, left = "auto", right = 50, bottom = "auto", width = 350,
                                    height = 'auto',
                                    #column(width=6,
                                    h4(strong("Radar Plot for 6 Qualitative Assessments",style="color:grey;")),
                                    plotlyOutput("plot_radar",height = 400)
                                    
                                    #)
                      )
                      
             ),
             tabPanel(strong(icon("star"),"Rank"),
                      fluidPage(
                        navlistPanel(widths = c(3, 9),
                                     "Ranking of High Schools in NYC",
                                     
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
                                              #absolutePanel(draggbale=T,left=10,bottom = 10,up=10,

                                              #),
                                              
                                              tabsetPanel(tabPanel("<font color=grey>Math</font>"%>%lapply(htmltools::HTML),
                                                                   tabItem(tabName="Mathyo",
                                                                           h1("Performance in", tags$b("Mathematics")),
                                                                           tags$p("The percent score for each school represents the average of each of its student's cumulative performance in the given subject (final exam, homeworks, etc). These scores are interpreted as percentages because the NYC Department of Education mandates percent scores for high school students, as opposed to GPAs on a 4.0 scale."),
                                                                           tags$br(),
                                                                           fluidRow(
                                                                             dataTableOutput("datarankedmath")
                                                                             
                                                                           )
                                                                   )),
                                                          tabPanel("<font color=grey>English</font>"%>%lapply(htmltools::HTML),
                                                                   tabItem(tabName="Englishyo",
                                                                           h1("Performance in", tags$b("English")),
                                                                           tags$p("The percent score for each school represents the average of each of its student's cumulative performance in the given subject (final exam, homeworks, etc). These scores are interpreted as percentages because the NYC Department of Education mandates percent scores for high school students, as opposed to GPAs on a 4.0 scale."),
                                                                           
                                                                           tags$br(),
                                                                           fluidRow(
                                                                             dataTableOutput("datarankedenglish")
                                                                             
                                                                           )
                                                                   )
                                                          ),
                                                          tabPanel("<font color=grey>History</font>"%>%lapply(htmltools::HTML),
                                                                   tabItem(tabName="Historyyo",
                                                                           h1("Performance in", tags$b("History")),
                                                                           tags$p("The percent score for each school represents the average of each of its student's cumulative performance in the given subject (final exam, homeworks, etc). These scores are interpreted as percentages because the NYC Department of Education mandates percent scores for high school students, as opposed to GPAs on a 4.0 scale."),
                                                                           
                                                                           tags$br(),
                                                                           fluidRow(
                                                                             dataTableOutput("datarankedhistory")
                                                                             
                                                                           )
                                                                   )
                                                          ),
                                                          tabPanel("<font color=grey>Science</font>"%>%lapply(htmltools::HTML),
                                                                   h1("Performance in", tags$b("Science")),
                                                                   tags$p("The percent score for each school represents the average of each of its student's cumulative performance in the given subject (final exam, homeworks, etc). These scores are interpreted as percentages because the NYC Department of Education mandates percent scores for high school students, as opposed to GPAs on a 4.0 scale."),
                                                                   
                                                                   tags$br(),
                                                                   fluidRow(
                                                                     dataTableOutput("datarankedscience")
                                                                     
                                                                   )
                                                          ),
                                                          tabPanel("<font color=grey>All Subjects</font>"%>%lapply(htmltools::HTML),
                                                                   h1("Performance in", tags$b("All Subjects")),  
                                                                   tags$p("The percent score for each school represents the average of each of its student's cumulative performance in the given subject (final exam, homeworks, etc). These scores are interpreted as percentages because the NYC Department of Education mandates percent scores for high school students, as opposed to GPAs on a 4.0 scale."),
                                                                   
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
                                                tabPanel("<font color=grey>All Qualitative</br>Areas</font>"%>%lapply(htmltools::HTML),
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
             ),tabPanel(strong(icon("calculator"), "Calculator"),div(id="bg",
                    absolutePanel(width=1200,left=50,right=50,top=60,
                        h2(strong("Select the 3 Factors you care and see the Top Schools"),icon("trophy")),
                        fluidRow(
                          column(3, selectInput("choice1", strong(icon("hand-o-right"),"Select the 1st factor:"),
                                                choices = c("Graduation Rate","College Enrollment Rate","Teacher Experience","Rigorous Instruction","Collaborative Teachers","Supportive Environment","Effective School Leadership","Strong Family-Community Ties","Trust"),
                                                selected = "Graduation Rate"),
                                 sliderInput('var1', 'How important it is:', 5,
                                             min = 0, max = 10)),
                          column(3, selectInput("choice2", strong(icon("hand-o-right"),"Select the 2nd factor:"),
                                                choices = c("Graduation Rate","College Enrollment Rate","Teacher Experience","Rigorous Instruction","Collaborative Teachers","Supportive Environment","Effective School Leadership","Strong Family-Community Ties","Trust"),
                                                selected = "College Enrollment Rate"),
                                 sliderInput('var2', 'How important it is:', 5,
                                             min = 0, max = 10)),
                          column(3, selectInput("choice3", strong(icon("hand-o-right"),"Select the 3rd factor:"),
                                                choices = c("Graduation Rate","College Enrollment Rate","Teacher Experience","Rigorous Instruction","Collaborative Teachers","Supportive Environment","Effective School Leadership","Strong Family-Community Ties","Trust"),
                                                selected = "Teacher Experience"),
                                 sliderInput('var3', 'How important it is:', 5,
                                             min = 0, max = 10))),
                        fluidRow(
                          absolutePanel( h4(icon("trophy"),"Top Schools"), 
                                     tableOutput("values")
                          )
                        )))),
             
             tabPanel(strong(icon("line-chart"), "Statistics"),div(id="bg",
                      absolutePanel(width=1200,left=50,right=50,top=50,                                            
                      # tags$head(tags$style(HTML('.info-box {min-height: 175px;}
                      #        .info-box-icon {height: 175px; line-height: 175px;}
                      #                           '))),
                     
                      fluidRow(
                     
                        column(width =11,status = "warning",
                            h3(strong(icon("comments-o"),"How do the Survey Results relate to the Graduation Rate & College Enrollment Rate?"))
                            
                      )),
                      fluidRow(p(" ")),
                      fluidRow(
                        column(width=4,
                        #box(width=4, height=120,status = "info",left=50,
                            selectInput(inputId = "y", label = "Y axis: Select graduation % or college enrollment %.",
                                        choices = list("Graduation Rate, 4 year" = "Graduate_Pct",
                                                       "Postsecondary Enrollment Rate, 6 months After High School"="Postsecondary_Enrollment_Pct"), 
                                        selected = "Graduate_Pct", multiple=FALSE,width = 400)
                        ),
                        column(width=4,
                        #box(width=4,height=120,status = "danger",
                            selectInput(inputId = "x1", label = "X axis: Select the category of survey questions.",
                                        choices = list("Rigorous Instruction"=1,"Collaborative Teachers"=2,
                                                       "Supportive Environment"=3,"Effective School Leadership"=4,
                                                       "Strong Family-Community Ties"=5,"Trust"=6), 
                                        selected = 1, multiple=FALSE,width = 400)
                        ),
                        column(width=4,
                        #box(width=4,height=120,status = "danger",
                            conditionalPanel(condition = "input.x1 == 1",
                                             selectInput(inputId = "x21", 
                                                         label = "X axis: Select a survey result",
                                                         choices = list("Rigorous Instruction - Overall Score" = 1,
                                                                        "% of students who say that they learn a lot from feedback on their work" =2,
                                                                        "% of students who know what their teacher wants them to learn in class"=3,
                                                                        "% of teachers who say that students build on each others' ideas during class discussions"=4), 
                                                         selected = 1, multiple=FALSE)
                            ),
                            conditionalPanel(condition = "input.x1 == 2",
                                             selectInput(inputId = "x22", 
                                                         label = "X axis: Select a survey question.",
                                                         choices = list("Collaborative Teachers - Overall Score" = 1,
                                                                        "% of teachers who say that they work together to design instructional programs"=2,
                                                                        "% of teachers who say that they have opportunities to work productively with colleagues in their school"=3,
                                                                        "% of teachers who say that they feel responsible that all students learn"=4), 
                                                         selected = 1, multiple=FALSE)
                            ),
                            conditionalPanel(condition = "input.x1 == 3",
                                             selectInput(inputId = "x23", 
                                                         label = "X axis: Select a survey question.",
                                                         choices = list("Supportive Environment - Overall Score" = 1,
                                                                        "% of students who feel safe in the hallways, bathrooms, locker room, and cafeteria"=2,
                                                                        "% of students who say that teachers notice when they are upset or having emotional difficulty"=3,
                                                                        "% of students who say that this school supports students in navigating the post-secondary process"=4), 
                                                         selected = 1, multiple=FALSE)
                            ),
                            conditionalPanel(condition = "input.x1 == 4",
                                             selectInput(inputId = "x24", 
                                                         label = "X axis: Select a survey question.",
                                                         choices = list("Effective School Leadership - Overall Score" = 1,
                                                                        "% of teachers who say that the principal communicates a clear vision for this school"=2,
                                                                        "% of teachers who say that curriculum and instruction are well coordinated across different grade levels"=3,
                                                                        "% of parents who feel that the principal works to create a sense of community in the school"=4), 
                                                         selected = 1, multiple=FALSE)
                            ),
                            conditionalPanel(condition = "input.x1 == 5",
                                             selectInput(inputId = "x25", 
                                                         label = "X axis: Select a survey question.",
                                                         choices = list("Strong Family-Community Ties - Overall Score" = 1,
                                                                        "% of parents who say that school staff regularly communicate with them about how the staff can help their children learn"=2,
                                                                        "% of parents who feel that teachers try to understand families' problems and concerns"=3,
                                                                        "% of teachers who say that teachers at this school work closely with families to meet students' needs"=4), 
                                                         selected = 1, multiple=FALSE)
                            ),
                            conditionalPanel(condition = "input.x1 == 6",
                                             selectInput(inputId = "x26", 
                                                         label = "X axis: Select a survey question.",
                                                         choices = list("Trust - Overall Score" = 1,
                                                                        "% of teachers who say they trust the principal"=2,
                                                                        "% of teachers who say that they trust each other"=3,
                                                                        "% of parents who say that school staff work hard to build trusting relationships with them"=4,
                                                                        "% of students who say that teachers treat them with respect"=5), 
                                                         selected = 1, multiple=FALSE)
                            )
                        )
                      ),
                      fluidRow(
                        column(width=8,
                               box(width = NULL, plotOutput(outputId = "lmPlot",height = 350))
                        ),
                        column(width=4,
                               infoBoxOutput(width = NULL, outputId = "y_value"),
                               infoBoxOutput(width = NULL, outputId = "x_value")
                        )
                      ),
                      fluidRow(
                        valueBoxOutput(outputId= "significance", width=12)
                      ))))
             
             
             
  )
)






dashboardPage(
  header,
  sidebar,
  body
)
