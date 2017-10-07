pageWithSidebar(
  headerPanel(''),
  sidebarPanel(
    selectInput('school_name', 'School',HS_frame$School.Name),
    selectInput('aspect', 'Aspects',names(Aspects),
                selected=names(Aspects)[1])
    
  ),
  mainPanel(
    plotlyOutput('plot1')
  )
)