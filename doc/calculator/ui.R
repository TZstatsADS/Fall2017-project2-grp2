ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("School Calculator", tabName = "calculator", icon = icon("th")))),
  dashboardBody(tabItem(tabName = "dashboard",
                        h2("Select the most important three factors for you and see the result for your ideal school."),
                        fluidRow(
                          column(4, selectInput("choice1", "Select the first factor:",
                                                choices = c("Graduation Rate","College Enrollment Rate","Teacher Experience","Rigorous Instruction","Collaborative Teachers","Supportive Environment","Effective School Leadership","Strong Family-Community Ties","Trust"),
                                                selected = "Graduation Rate"),
                                 sliderInput('var1', 'How important it is:', 5,
                                             min = 0, max = 10)),
                          column(4, selectInput("choice2", "Select the second factor:",
                                                choices = c("Graduation Rate","College Enrollment Rate","Teacher Experience","Rigorous Instruction","Collaborative Teachers","Supportive Environment","Effective School Leadership","Strong Family-Community Ties","Trust"),
                                                selected = "College Enrollment Rate"),
                                 sliderInput('var2', 'How important it is:', 5,
                                             min = 0, max = 10)),
                          column(4, selectInput("choice3", "Select the third factor:",
                                                choices = c("Graduation Rate","College Enrollment Rate","Teacher Experience","Rigorous Instruction","Collaborative Teachers","Supportive Environment","Effective School Leadership","Strong Family-Community Ties","Trust"),
                                                selected = "Teacher Experience"),
                                 sliderInput('var3', 'How important it is:', 5,
                                             min = 0, max = 10))),
                        fluidRow(box(title = "Result", status = "primary", width = 12, solidHeader = TRUE, 
                                     tableOutput("values")
                        ))))
)


  
