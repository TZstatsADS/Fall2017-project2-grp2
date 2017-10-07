library(leaflet)
loc_data<-read.csv("~/Desktop/[ADS]Advanced Data Science/Fall2017-project2-grp2/data/HS_loc.csv")
HS_summary<-read.csv("~/Desktop/HS_Summary.csv")
# 
# zipcode<-as.character(loc_data$addr)
# f<-function(l){
#   x<-strsplit(l,split = ",")[[1]]
#   x<-x[length(x)-1]
#   x<-strsplit(x,split=" ")[[1]]
#   x<-x[length(x)]
#   return(as.integer(x))
# }
# y<-as.numeric(sapply(zipcode,f))
# loc_data$zipcode<-y

data_merge<-merge(loc_data,HS_summary[,c(2,4:10)],by.x="school.name",by.y="School.Name")

# schoolIcons <- iconList(
#   makeIcon("../data/school1600-1.png", "../data/school1600-1.png", iconWidth = 15, iconHeight = 15)
# )

labels <- sprintf(
  "<strong><font color=\"#00008b\" size=3>%s</font></strong><br/>
  %s<br/> 
  <strong>%g</strong> students<br/>
  <font color=\"#8b2323\">Rigorous Instruction</font>: %s<br/>
  <font color=\"#458b00\">Collaborative Teaching</font>: %s<br/>
  <font color=\"#cd9b1d\">Supportive Environment</font>: %s<br/>
  <font color=\"#68228b\">Effective School Leadership</font>: %s<br/>
  <font color=\"#104e8b\">Strong Family Community Tie</font>: %s<br/>
  <font color=\"#698b69\">Trust</font>: %s",
  data_merge$school.name, as.character(data_merge$addr),data_merge$Enrollment,
  data_merge$Rigorous.Instruction.Rating,
  data_merge$Collaborative.Teachers.Rating,
  data_merge$Supportive.Environment.Rating,
  data_merge$Effective.School.Leadership.Rating,
  data_merge$Strong.Family.Community.Ties.Rating,
  data_merge$Trust.Rating
) %>% lapply(htmltools::HTML)

# content <- as.character(tagList(
#   tags$h4(data_merge$school.name),
#   tags$strong(HTML(sprintf("%s",data_merge$addr
#   ))), tags$br(),
#   sprintf("Rigorous Instruction: %s", data_merge$Rigorous.Instruction.Rating), tags$br(),
#   sprintf("Collaborative Teaching: %s%%",data_merge$Collaborative.Teachers.Rating), tags$br(),
#   sprintf("Supportive Environment: %s", data_merge$Supportive.Environment.Rating),tags$br(),
#   sprintf("Effective School Leadership: %s", data_merge$Effective.School.Leadership.Rating),tags$br(),
#   sprintf("Strong Family Community Tie: %s", data_merge$Strong.Family.Community.Ties.Rating),tags$br(),
#   sprintf("Trust: %s", data_merge$Trust.Rating)
# ))



leaflet(data_merge)%>%
  addTiles()%>%
  addCircles(~lon, ~lat,popup=labels,label=~school.name,radius=~Enrollment/10)
  #addMarkers(icon =schoolIcons)

