library(shiny)
library(shinydashboard)

shinyUI(
  fluidPage(
    theme="styles.css",
    div(
      id="canvas",
      navbarPage(
        strong("Project title",style="color: White;"),
        tabPanel(
          "Intro",
          mainPanel(width=12,
            h1("Project: an RShiny app development project"),
            h2("Summary"),
            p("This project explores and visualizes information of high schools in New York City by 481 high schools data from 2014 to 2015 on NYC Open Data Portal. We created a Shiny App to help parents decide the ideal high school for their children in New York."),
            h2("Content"),
            p(strong("1. School Map")),
            p("   -  ", strong("Tab 1"),": Find schools on the map on the basis of zip code and total student number, and see your selected school's detailed information regarding, for example, diversity, post-secondary enrollment status and SAT scores."),
            p("   -  ", strong("Tab 2"),": Select a school you are interested in and see its location on the map and its detailed information."),
            p("   -  ", strong("Tab 3"),": Select a school you are interested in and see the school's overall framework survey performance compared to other schools' in its borough or in the city as a whole."),
            p(strong("2. Compare 2 Schools")),
            p("Use radar charts to compare any two schools' relative strengths and weaknesses."),
            p(strong("3. Rank")),
            p("Rank the schools on the basis of factors you care about the most and see the top 10 results."),
            p(strong("4. Statistical Analysis")),
            p("Look into how a school's framework survey performance is related to the school's graduation rate and college-enrollment rate, select the features you like, and see whether they are significantly correlated."),
            br()
          )
        )
      )
    )
  )
)