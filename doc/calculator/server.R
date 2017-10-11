library(shiny)
library(shinydashboard)

setwd("~/Desktop/[ADS]Advanced Data Science/Fall2017-project2-grp2/")
vardata<-read.csv("./data/HS_var.csv")
var2data<-read.csv("./data/HS_var2.csv")
rownames(vardata)<-vardata$School.Name
vardata$School.Name<-c()


server <- function(input, output) {
  output$values <- renderTable({
    n = c(0,0,0,0,0,0,0,0,0)
    vec = c("Graduation Rate","College Enrollment Rate","Teacher Experience","Rigorous Instruction","Collaborative Teachers","Supportive Environment","Effective School Leadership","Strong Family-Community Ties","Trust")
    inputchoice = c(which(vec == input$choice1),which(vec == input$choice2),which(vec == input$choice3))
    inputvalue = c(as.numeric(input$var1),as.numeric(input$var2),as.numeric(input$var3))
    n[inputchoice] = inputvalue
    new = data.frame(mapply(`*`,vardata,n))
    new2 = data.frame(sum=rowSums(new))
    lst = sort(new2[,1], index.return=TRUE, decreasing=TRUE)
    top5 = lapply(lst, `[`, lst$x %in% head(unique(lst$x),5))$ix
    vecnew = c(1,inputchoice+1)
    var2data[top5,][vecnew]
  })
}
