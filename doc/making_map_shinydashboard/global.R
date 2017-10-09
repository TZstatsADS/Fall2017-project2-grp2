#global
library(shinydashboard)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(leaflet)
library("shinyBS")
library(dplyr)
library(tibble)
loc_data<-read.csv("~/Desktop/[ADS]Advanced Data Science/Fall2017-project2-grp2/data/HS_loc_with_borough.csv")
HS_summary<-read.csv("~/Desktop/[ADS]Advanced Data Science/Fall2017-project2-grp2/data/HS_Summary.csv")
HS_frame<-read.csv("~/Desktop/[ADS]Advanced Data Science/Fall2017-project2-grp2/data/HS_frame.csv")
data_merge<-merge(loc_data,HS_summary[,c(2,4:10)],by.x="school.name",by.y="School.Name")
tidyrace<-read.csv("~/Desktop/[ADS]Advanced Data Science/Fall2017-project2-grp2/data/tidyrace.csv")
data_merge$borough<-as.character(data_merge$borough)
teacher_expr<-HS_summary[,c("School.Name","Percent.of.teachers.with.3.or.more.years.of.experience")]

ECON_EXPLAIN="Economic Need Index = (Percent Temporary Housing) + (Percent HRA-eligible * 0.5) + (Percent Free Lunch Eligible * 0.5)"
econ_need<-HS_summary[,c("School.Name","Economic.Need.Index")]
disability<-HS_summary[,c("School.Name","Percent.Students.with.Disabilities")]
language_learner<-HS_summary[,c("School.Name","Percent.English.Language.Learners")]

#SAT
tidysat<-read.csv("~/Desktop/[ADS]Advanced Data Science/Fall2017-project2-grp2/data/tidysat.csv")
sat_city<-read.csv("~/Desktop/[ADS]Advanced Data Science/Fall2017-project2-grp2/data/sat_city.csv")

# rownames(SAT_HS)<-as.vector(SAT_HS$School.Name)
# SAT_HS$School.Name<-c()
# colnames(SAT_HS)<-c("Math","Writing","Critical Reading")
icon_u=list(iconUrl='icon/construction.png',iconSize=c(25,25))
##Colors
col<- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
col1<-alpha(brewer.pal(8, "Set1"))
col2<-alpha(brewer.pal(8, "Set2"))
col3<-alpha(brewer.pal(8, "Set3"))
#red, yellow, aqua, blue, light-blue, green, navy, 
#teal, olive, lime, orange, fuchsia, purple, maroon, black.

xx="<strong><font size=3>Post-Secondary Status-after 18 Month"%>%lapply(htmltools::HTML)
#BOX TYPE:primary, success, info, warning, danger.
##popup labels
labels_all <- sprintf(
  "<strong><font color=\"#00008b\" size=3>%s</font></strong><br/>
        %s<br/> 
        <strong>%g</strong> students<br/>
        ",
  data_merge$school.name, as.character(data_merge$addr),data_merge$Enrollment
  
)

tidysixmonth<-read.csv("~/Desktop/[ADS]Advanced Data Science/Fall2017-project2-grp2/data/tidysix.csv")
tidyeighteenmonth<-read.csv("~/Desktop/[ADS]Advanced Data Science/Fall2017-project2-grp2/data/tidyeighteen.csv")


HS_frame$School.Name<-as.character(HS_frame$School.Name)
n=nrow(HS_frame)

Aspects<-list(
  'Rigorous Instruction Rating'=c(1:4,8:10),	
  'Collaborative Teachers Rating'=c(1,2,17:18,20:22),	
  'Supportive Environment Rating'=c(1,2,29:30,32:34),
  'Effective School Leadership Rating'=c(1,2,50:54),
  'Strong Family-Community Ties Rating'=c(1,2,61:65),
  'Trust Rating'=c(1,2,72:76)
)

#the list for the summary icon
icon_list<-list("Approaching Target"=icon("star-half-o"),"Exceeding Target"=icon("trophy"),
                "Meeting Target"=icon("check-square-o"),"N/A"=icon("exclamation-triangle"),
                "Not Meeting Target"=icon("close"))

cal_values<-function(df,School_name){
  colnames(df)[3:7]<-c("rating","ele_score","School_per","City_per","Boro_per")
  df[,4]<-as.numeric(as.vector(df[,4]))
  s_i=df[df$School.Name==School_name,"ele_score"]

  df<-df%>%filter(!is.na(ele_score))

  if(is.na(s_i)){
    rank_i="NA"
  }else{
    rank_i=floor(nrow(df)-rank(df$ele_score))
    rank_i=paste(rank_i[which(df$School.Name==School_name)],"/",nrow(df),sep = "")
  }
  higher_t_city<-(df[df$School.Name==School_name,"School_per"]-df[df$School.Name==School_name,"City_per"])/df[df$School.Name==School_name,"City_per"]
  higher_t_boro<-(df[df$School.Name==School_name,"School_per"]-df[df$School.Name==School_name,"Boro_per"])/df[df$School.Name==School_name,"Boro_per"]
  
  if(length(higher_t_city)<1){
    higher_t_city<-"NA"
  }else{
    higher_t_city<-ifelse(higher_t_city>=0,paste(round(higher_t_city,2)*100,"%"," Higher",sep = ""),paste(round(abs(higher_t_city),2)*100,"%"," Lower",sep = ""))
  }
  
  if(length(higher_t_boro)<1){
    higher_t_boro<-"NA"
  }else{
    higher_t_boro<-ifelse(higher_t_boro>=0,paste(round(higher_t_boro,2)*100,"%"," Higher",sep = ""),paste(round(abs(higher_t_boro),2)*100,"%"," Lower",sep = ""))
  }
  return(c(as.character(rank_i),higher_t_city,higher_t_boro))
}

##plot the scores for the 6 aspects
plot_scores<-function(df,school_name,aspect){
  colnames(df)[3:4]<-c("rating","ele_score")
  df[,4]<-as.numeric(as.vector(df[,4]))
  s_i=df[df$School.Name==school_name,"ele_score"]
  if(is.na(s_i)){
    return(paste("No data for this school"))
  }
  df<-df%>%filter(!is.na(ele_score))
  
  h_i=round(sum(df$ele_score<s_i)/nrow(df),2)
  r_i=df[df$School.Name==school_name,"rating"]
  #m<-mean(df$ele_score)
  rank_i=nrow(df)-rank(df$ele_score)
  rank_i=paste(rank_i[which(df$School.Name==school_name)],"/",nrow(df),sep = "")
  
  col=alpha(brewer.pal(8, "Set2"))[3:8]
  den<-density(df$ele_score,na.rm = T)
  fit<-approxfun(den)
  
  plot_ly(x =~ele_score,data=df) %>% 
    add_histogram(type = "histogram", name = "Histogram",marker=list(color=col[1]) )%>%
    add_trace(x = den$x[den$x<=s_i], y = den$y[den$x<=s_i], type = "scatter",
              mode = "lines", fill = "tozeroy", yaxis = "y2", name = "lower score density",
              line=list(color=col[2],alpha=.1)) %>% 
    add_trace(x = den$x[den$x>=s_i], y = den$y[den$x>=s_i], type = "scatter", 
              mode = "lines", fill = "tozeroy", yaxis = "y2", name = "higher score density",
              line=list(color=col[3],alpha=.1)) %>% 
    add_trace(x=s_i,y=fit(s_i),type = "scatter",mode="markers",
              text=paste("Score :", s_i, "<br>Rating:",r_i,'<br>Higher than:',h_i*100,
                         "%","<br>Rank:",rank_i),
              marker = list(size = 15,shape="rect", color = 'rgba(152, 0, 0, .8)'),
              name=school_name,yaxis="y2")%>%
    layout(title=aspect,yaxis1=list(title="Freq"),xaxis=list(title="Elementary Score"),yaxis2 = list(title="Density",overlaying = "y", side = "right", showgrid = FALSE)
           ,legend = list(x = -0.05, y = -0.5),margin = list(r=50))
  
  
}  
###make the bar plot for sat Scores, comparing with the city level
sat_plot<-function(school.name){
  
  df<-tidysat%>%
    filter(Name==school.name)
  m <- list(
    l = 15,
    r = 0,
    b = 15,
    t = 0
  )
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  if(is.na(df$Score[1])){
    plot_ly(df)%>%layout(title="no data",xaxis = ax,yaxis =ax)
  }
  p=plot_ly(y =~Score,x=~Sub,data=df,type = "bar",name="The School")%>%
    add_trace(y =~Score,x=~Sub,data=sat_city,type = "bar",name="City Level",textposition = 'auto')%>%
    layout(xaxis = list(title = ''),yaxis =ax, 
           barmode = 'group',legend=list(x=0,y=500),margin=m)
  
}
