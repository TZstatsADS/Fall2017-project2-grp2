### app.R ###
library(shiny)
library(shinydashboard)
library(DT)

#setwd("C:/Users/enriquethemoist/Dropbox/Columbia Work/4A - one/Applied Data Science/Project 2/hs quality app") 

###################################
######Read in Ranked Datasets######
###################################

#Read them in, then clean columns and their respective names
#Edit paths as necessary

data.ranked.quant <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.quant.csv")
head(data.ranked.quant)
data.ranked.quant$X <- NULL
colnames(data.ranked.quant) <- c("Ranking", "School Name", "Graduation Rate", "Post-secondary Enrollment Rate, 6 Months", "Averaged Score (%)")

data.ranked.math <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.math.csv")
data.ranked.math$X <- NULL
colnames(data.ranked.math) <- c("Ranking", "School Name", "Algebra Score (%)", "Geometry Score (%)", "Averaged Math Score (%)")

data.ranked.english <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.english.csv")
data.ranked.english$X <- NULL
colnames(data.ranked.english) <- c("Ranking", "School Name", "English Score (%)")

data.ranked.history <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.history.csv")
data.ranked.history$X <- NULL
colnames(data.ranked.history) <- c("Ranking", "School Name", "US History Score (%)", "Global History Score (%)", "Averaged History Score (%)")

data.ranked.science <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.science.csv")
data.ranked.science$X <- NULL
colnames(data.ranked.science) <- c("Ranking", "School Name", "Chemistry Score (%)", "Physics Score (%)", "Averaged Science Score (%)")

data.ranked.allsubjects <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.allsubjects.csv")
data.ranked.allsubjects$X <- NULL
colnames(data.ranked.allsubjects) <- c("Ranking", "School Name", "Averaged Score - all subjects (%)")

data.ranked.RI <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.RI.csv")
data.ranked.RI$X <- NULL
colnames(data.ranked.RI) <- c("Ranking", "School Name", "Averaged Score (%)")

data.ranked.CT <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.CT.csv")
data.ranked.CT$X <- NULL
colnames(data.ranked.CT) <- c("Ranking", "School Name", "Averaged Score (%)")

data.ranked.SE <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.SE.csv")
data.ranked.SE$X <- NULL
colnames(data.ranked.SE) <- c("Ranking", "School Name", "Averaged Score (%)")

data.ranked.ESL <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.ESL.csv")
data.ranked.ESL$X <- NULL
colnames(data.ranked.ESL) <- c("Ranking", "School Name", "Averaged Score (%)")

data.ranked.SFCT <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.SFCT.csv")
data.ranked.SFCT$X <- NULL
colnames(data.ranked.SFCT) <- c("Ranking", "School Name", "Averaged Score (%)")

data.ranked.T <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.T.csv")
data.ranked.T$X <- NULL
colnames(data.ranked.T) <- c("Ranking", "School Name", "Averaged Score (%)")

data.ranked.allsurvey <- read.csv("C:/Users/enriquethemoist/Documents/data.ranked.allsurvey.csv")
data.ranked.allsurvey$X <- NULL
colnames(data.ranked.allsurvey) <- c("Ranking", "School Name", "Averaged Score (%)")

##############
######UI######
##############


ui <- dashboardPage(
  skin="black",
  dashboardHeader(
    title ="NYC High School Rankings",
    titleWidth=350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Intro", icon = icon("home")),
      menuItem("Diploma Completion", tabName = "Diplo", icon = icon("graduation-cap")),
      menuItem("Academic Performance", tabName="Academic Perf", icon=icon("trophy"), startExpanded = TRUE,
               menuSubItem("Math", tabName="Mathyo"),
               menuSubItem("English", tabName="Englishyo"),
               menuSubItem("History", tabName="Historyyo"),
               menuSubItem("Science", tabName="Scienceyo"),
               menuSubItem("All Subjects", tabName="AllSubjectsyo")
      ),
      menuItem("Qualitative Assessment ", tabName="QualAss ", icon=icon("thumbs-up"), startExpanded = TRUE, 
               menuSubItem("Rigorous Instruction", tabName="RI"),
               menuSubItem("Collaborative Teachers", tabName="CT"),
               menuSubItem("Supportive Environment", tabName="SE"),
               menuSubItem("Effective School Leadership", tabName="ESL"),
               menuSubItem("Strong Family-Community Ties", tabName="SFCT"),
               menuSubItem("Trust", tabName="T"),
               menuSubItem("All Qualitative Areas", tabName="AllQual")
      )
               
               
      
      )
    ),
    
    
  
  
  dashboardBody(
  tabItems(
    tabItem(tabName="Intro",
              h2("A Ranking of High Schools in the City of New York"),
              tags$hr(),
              h4("This Ranking App was created with the use of data",
              "\ from the latest School Quality Report published by the NYC",
              "\ Department of Education (the 2014-15 Edition)."),
              tags$br(),
              h4(tags$a(href="https://data.cityofnewyork.us/Education/2014-2015-School-Quality-Reports-Results-For-High-/vrfr-9k4d", "The link to the Report can be accessed by clicking on this text.")),
              tags$br()
            ),
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
          ),
    tabItem(tabName="Mathyo",
            h1("Performance in", tags$b("Mathematics")),
            tags$br(),
            fluidRow(
              dataTableOutput("datarankedmath")
              
            )
          ),
    tabItem(tabName="Englishyo",
            h1("Performance in", tags$b("English")),
            tags$br(),
            fluidRow(
              dataTableOutput("datarankedenglish")
              
            )
    ),
    tabItem(tabName="Historyyo",
            h1("Performance in", tags$b("History")),
            tags$br(),
            fluidRow(
              dataTableOutput("datarankedhistory")
              
            )
    ),
    tabItem(tabName="Scienceyo",
            h1("Performance in", tags$b("Science")),
            tags$br(),
            fluidRow(
              dataTableOutput("datarankedscience")
              
            )
    ),
    tabItem(tabName="AllSubjectsyo",
            h1("Performance in", tags$b("All Subjects")),  
            fluidRow(
              dataTableOutput("datarankedallsubjects")
              
            )
    ),
    tabItem(tabName="RI",
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
    tabItem(tabName="CT",
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
    tabItem(tabName="SE",
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
    tabItem(tabName="ESL",
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
    tabItem(tabName="SFCT",
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
    tabItem(tabName="T",
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
    tabItem(tabName="AllQual",
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

)
  



##############
####SERVER####
##############


server <- function(input, output) {
  
  output$datarankedquant <- renderDataTable(data.ranked.quant, rownames=F)
  output$datarankedmath <- renderDataTable(data.ranked.math, rownames =F)
  output$datarankedenglish <- renderDataTable(data.ranked.english, rownames =F)
  output$datarankedhistory <- renderDataTable(data.ranked.history, rownames =F)
  output$datarankedscience <- renderDataTable(data.ranked.science, rownames =F)
  output$datarankedallsubjects <- renderDataTable(data.ranked.allsubjects, rownames =F)
  output$datarankedRI <- renderDataTable(data.ranked.RI, rownames =F)
  output$datarankedCT <- renderDataTable(data.ranked.CT, rownames =F)
  output$datarankedSE <- renderDataTable(data.ranked.SE, rownames =F)
  output$datarankedESL <- renderDataTable(data.ranked.ESL, rownames =F)
  output$datarankedSFCT <- renderDataTable(data.ranked.SFCT, rownames =F)
  output$datarankedT <- renderDataTable(data.ranked.T, rownames =F)
  output$datarankedallsurvey <- renderDataTable(data.ranked.allsurvey, rownames =F)
}

shinyApp(ui = ui, server = server)
  