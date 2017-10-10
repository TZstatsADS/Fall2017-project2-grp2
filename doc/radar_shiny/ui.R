library(shiny)
library(shinydashboard)

shinyUI(
  fluidPage(
    fluidRow(
      box(
        width = 6,
        radioButtons("br_val",label = "Search by borough: ",
                     choices = list("Manhattan"=1,"Brooklyn"=2,"Bronx"=3,"Queens"=4,"Staten Island"=5))
        ),
      box(
        width = 6,
        radioButtons("br_val2",label = "Search by borough: ",
                     choices = list("Manhattan"=1,"Brooklyn"=2,"Bronx"=3,"Queens"=4,"Staten Island"=5))
        )
      ),
    fluidRow(
      box(
        width = 6,
        #h1("Make a comparison:"),
        conditionalPanel(condition="input.br_val==1",
                         selectInput("school1.1", label = "Select first high school:",
                                     choices = as.vector(mht), selected = F)
        ),
        conditionalPanel(condition="input.br_val==2",
                         selectInput("school1.2", label = "Select first high school:",
                                     choices = as.vector(brkl), selected = F)               
        ),
        conditionalPanel(condition="input.br_val==3",
                         selectInput("school1.3", label = "Select first high school:",
                                     choices = as.vector(brx), selected = F)
        ),
        conditionalPanel(condition="input.br_val==4",
                         selectInput("school1.4", label = "Select first high school:",
                                     choices = as.vector(qs), selected = F)
        ),
        conditionalPanel(condition="input.br_val==5",
                         selectInput("school1.5", label = "Select first high school:",
                                     choices = as.vector(sti), selected = F)
        )
      ),
      box(width = 6,
        conditionalPanel(condition="input.br_val2==1",
                         selectInput("school2.1", label = "Select second high school:",
                                     choices = as.vector(mht), selected = F)
        ),
        conditionalPanel(condition="input.br_val2==2",
                         selectInput("school2.2", label = "Select second high school:",
                                     choices = as.vector(brkl), selected = F)
        ),
        conditionalPanel(condition="input.br_val2==3",
                         selectInput("school2.3", label = "Select second high school:",
                                     choices = as.vector(brx), selected = F)
        ),
        conditionalPanel(condition="input.br_val2==4",
                         selectInput("school2.4", label = "Select second high school:",
                                     choices = as.vector(qs), selected = F)
        ),
        conditionalPanel(condition="input.br_val2==5",
                         selectInput("school2.5", label = "Select second high school:",
                                     choices = as.vector(sti), selected = F)
        )
      )
    ),
    fluidRow(
      box(width = 4,plotlyOutput("plot"))
    )
  )
)
