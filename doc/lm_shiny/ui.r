packages.used=c("shiny","shinydashboard")
packages.needed=setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,repos='http://cran.us.r-project.org')
}
library(shiny)
library(shinydashboard)


#######################
### UI
#######################

header <- dashboardHeader(title="test")

sidebar <- dashboardSidebar()

body <- dashboardBody(
  fluidRow(
    box(width = 12,status = "warning",
        title=h2("How do the survey results relate to the graduation rate & college enrollment rate?"))
  ),
  fluidRow(
    box(width=6, 
        selectInput(inputId = "y", label = "Y axis: Please select graduation rate or college enrollment rate.",
                    choices = list("Graduation Rate, 4 year" = "Graduate_Pct",
                                   "Postsecondary Enrollment Rate, 6 months After High School"="Postsecondary_Enrollment_Pct"), 
                    selected = "Graduate_Pct", multiple=FALSE)
    ),
    box(width=6,
        selectInput(inputId = "x1", label = "X axis: Select the category of survey questions.",
                    choices = list("Rigorous Instruction"=1,"Collaborative Teachers"=2,
                                   "Supportive Environment"=3,"Effective School Leadership"=4,
                                   "Strong Family-Community Ties"=5,"Trust"=6), 
                    selected = 1, multiple=FALSE),
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
    )#box
  ),
  fluidRow(
    column(width=9,
           box(width = NULL, plotOutput(outputId = "lmPlot"))
    ),
    column(width=3,
           infoBoxOutput(width = NULL, outputId = "y_value"),
           infoBoxOutput(width = NULL, outputId = "x_value"),
           infoBoxOutput(width = NULL, outputId = "additional_info")
    )
  ),
  fluidRow(
    valueBoxOutput(outputId= "significance", width=12)
  )
)   


shinyUI(dashboardPage(header, sidebar, body))

