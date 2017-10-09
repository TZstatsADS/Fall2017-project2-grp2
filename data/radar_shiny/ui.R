shinyUI(
  fluidPage(
    sidebarPanel(
      h1("Make a comparison:"),
      selectInput("school1", label = "Select first high school:",
                  choices = as.vector(schoolname), selected = F),
      h2("VS"),
      selectInput("school2", label = "Select second high school:",
                  choices = as.vector(schoolname), selected = F)
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  ))
