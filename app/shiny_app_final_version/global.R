# Box Color
### red, yellow, aqua, blue, light-blue, green, navy, 
### teal, olive, lime, orange, fuchsia, purple, maroon, black.
#BOX TYPE:primary, success, info, warning, danger.

library(shinydashboard)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(leaflet)
library("shinyBS")
library(dplyr)
library(tibble)

load("all_shiny_data.RData")
# setwd('~/Desktop/[ADS]Advanced Data Science/Fall2017-project2-grp2/')
loc_data<-read.csv("./data/HS_loc_with_borough.csv")
HS_summary<-read.csv("./data/HS_Summary.csv")
HS_frame<-read.csv("./data/HS_frame.csv")
data_merge<-merge(loc_data,HS_summary[,c(2,4:10)],by.x="school.name",by.y="School.Name")
tidyrace<-read.csv("./data/tidyrace.csv")
data_merge$borough<-as.character(data_merge$borough)
HS_frame$School.Name<-as.character(HS_frame$School.Name)
schoolname<-loc_data$school.name
HS_summary$School.Name<-as.character(HS_summary$School.Name)

########### making the data set for Value Boxes################3
ECON_EXPLAIN="Economic Need Index = (Percent Temporary Housing) + (Percent HRA-eligible * 0.5) + (Percent Free Lunch Eligible * 0.5)"
econ_need<-HS_summary[,c("School.Name","Economic.Need.Index")]
disability<-HS_summary[,c("School.Name","Percent.Students.with.Disabilities")]
language_learner<-HS_summary[,c("School.Name","Percent.English.Language.Learners")]
teacher_expr<-HS_summary[,c("School.Name","Percent.of.teachers.with.3.or.more.years.of.experience")]

########## SAT data ###################
tidysat<-read.csv("./data/tidysat.csv")
sat_city<-read.csv("./data/sat_city.csv")


################ Colors #########################
col<- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
col1<-alpha(brewer.pal(8, "Set1"))
col2<-alpha(brewer.pal(8, "Set2"))
col3<-alpha(brewer.pal(8, "Set3"))


###### Map Popup Labels ########################
labels_all <- sprintf(
  "<strong><font color=\"#00008b\" size=3>%s</font></strong><br/>
        %s<br/> 
        <strong>%g</strong> students<br/>
        ",
  data_merge$school.name, as.character(data_merge$addr),data_merge$Enrollment
  
)

labels_all_new <- sprintf(
  "<strong><font color=\"#00008b\" size=3>%s</font></strong><br/>
  %s<br/> 
  <strong>%g</strong> students<br/>
  </i> <font color=\"#8b2323\"><i class=\"fa fa-users\"> Teachers with 3 or More Year Expr</i>: <strong>%s %%</strong></font></br>
  <font color=\"#458b0\"><i class=\"fa fa-language\"> English Language Learner</i>: <strong>%s %%</strong></font></br>
  <font color=\"#cd9b1d\"> <i class=\"fa fa-dollar\">  Economic Need Index</i>: <strong>%s %%</strong></font></br>
  <font color=\"#104e8b\"> <i class=\"fa fa-wheelchair\"> Students with Disabilities</i>: <strong>%s %%</strong></font></br>
  ",
  data_merge$school.name,
  as.character(data_merge$addr),
  data_merge$Enrollment,
  #icon("users"),
  round(teacher_expr[,2],2)*100,
  #icon("language"),
  round(language_learner[,2],2)*100,
  #icon("dollar"),
  round(econ_need[,2],2)*100,
  #icon("wheelchair"),
  round(disability[,2],2)*100
  
) %>% lapply(htmltools::HTML)

########### After Grad ##############
tidysixmonth<-read.csv("./data/tidysix.csv")
tidyeighteenmonth<-read.csv("./data/tidyeighteen.csv")

################  the list for the summary icon ##################
icon_list<-list("Approaching Target"=icon("star-half-o"),"Exceeding Target"=icon("trophy"),
                "Meeting Target"=icon("check-square-o"),"N/A"=icon("exclamation-triangle"),
                "Not Meeting Target"=icon("close"))


############### calculating the RANK Data  ###############
cal_values<-function(df,School_name){
  df<-merge(df,loc_data,by.x = "School.Name",by.y="school.name")
  colnames(df)[3:7]<-c("rating","ele_score","School_per","City_per","Boro_per")
  df[,4]<-as.numeric(as.vector(df[,4]))
  s_i=df[df$School.Name==School_name,"ele_score"]
  boro_i=df[df$School.Name==School_name,"borough"]
  
  df<-df%>%filter(!is.na(ele_score))
  
  if(is.na(s_i)){
    rank_i="NA"
  }else{
    rank_i=floor(nrow(df)-rank(df$ele_score))
    rank_i=paste(rank_i[which(df$School.Name==School_name)],"/","487",sep = "")
  }
  
  higher_t_city<-df[df$School.Name==School_name,"School_per"]
  higher_t_boro<-(s_i-mean(df$ele_score[df$borough==boro_i]))/mean(df$ele_score[df$borough==boro_i])
  if(length(higher_t_city)<1){
    higher_t_city<-"NA"
  }else{
    higher_t_city<-paste(round(higher_t_city,2)*100,"%",sep = "")
    #higher_t_city<-ifelse(higher_t_city>=0,paste(round(higher_t_city,2)*100,"%"," Higher",sep = ""),paste(round(abs(higher_t_city),2)*100,"%"," Lower",sep = ""))
  }
  
  if(length(higher_t_boro)<1){
    higher_t_boro<-"NA"
  }else{
    higher_t_boro<-ifelse(higher_t_boro>=0,paste(round(higher_t_boro,2)*100,"%"," Higher",sep = ""),paste(round(abs(higher_t_boro),2)*100,"%"," Lower",sep = ""))
  }
  return(c(as.character(rank_i),higher_t_city,higher_t_boro))
}

################### Hist for the 6 Aspects ##################
Aspects<-list(
  'Rigorous Instruction Rating'=c(1:4,8:10),	
  'Collaborative Teachers Rating'=c(1,2,17:18,20:22),	
  'Supportive Environment Rating'=c(1,2,29:30,32:34),
  'Effective School Leadership Rating'=c(1,2,50:54),
  'Strong Family-Community Ties Rating'=c(1,2,61:65),
  'Trust Rating'=c(1,2,72:76)
)
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
    layout(yaxis1=list(title="Freq"),xaxis=list(title="Elementary Score"),yaxis2 = list(title="Density",overlaying = "y", side = "right", showgrid = FALSE)
           ,legend = list(x = -0.05, y = -0.5),margin = list(r=50))
  
  
}  
#############  BAR plot for SAT Scores, comparing with the city level#################
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


########################  Radar plot  ######################3
make_radar<-function(school1,school2){
  HS_frame<-read.csv("./data/HS_frame.csv")
  Rigorous_instruction<-HS_frame[,1:16]#3:16
  Collaborative_Teacher<-HS_frame[,c(1,2,17:28)]
  Supportive_env<-HS_frame[,c(1,2,29:49)]
  Leadership<-HS_frame[,c(1,2,50:60)]
  Fam_Sch_Tie<-HS_frame[,c(1,2,61:71)]
  Trust<-HS_frame[,c(1,2,72:89)]
  #rm(HS_frame)
  
  options(digits=3)
  rigo<-as.numeric(as.vector(Rigorous_instruction[,4]))
  coll<-as.numeric(as.vector(Collaborative_Teacher[,4]))
  supp<-as.numeric(as.vector(Supportive_env[,4]))
  lead<-as.numeric(as.vector(Leadership[,4]))
  fami<-as.numeric(as.vector(Fam_Sch_Tie[,4]))
  trus<-as.numeric(as.vector(Trust[,4]))
  
  score<-c()
  for(i in 1:487){
    each<-cbind(rigo[i],coll[i],supp[i],lead[i],fami[i],trus[i])
    score<-cbind(score,each)
  }
  score<-as.vector(score)
  
  response<-rep(c("Rigorous","Collaborative Teachers","Supportive Environment","Leadership","Family-Community Tie","Trust"),487)
  
  schoolname<-Rigorous_instruction[,2]
  school<-rep(schoolname,each=6)
  school<-as.vector(school)
  
  df<-as.data.frame(cbind(school,response,score))
  df$score<-as.numeric(as.vector(df$score))
  
  df$degree <- seq(0,300,60) # 6 responses, equals 60 degrees per response
  df$o <- df$score/5 * sin(df$degree * pi / 180) # SOH
  df$a <- df$score/5 * cos(df$degree * pi / 180) # CAH
  df$o5 <- 1 * sin(df$degree * pi / 180) # Outer ring x
  df$a5 <- 1 * cos(df$degree * pi / 180) # Outer ring y 
  df$a4 <- 0.8 * cos(df$degree * pi / 180) # 80% ring y
  df$o4 <- 0.8 * sin(df$degree * pi / 180) # 80% ring x
  df$o3 <- 0.6 * sin(df$degree * pi / 180) # 60% ring x
  df$a3 <- 0.6 * cos(df$degree * pi / 180) # 60% ring y
  df$o2 <- 0.4 * sin(df$degree * pi / 180) # 40% ring x
  df$a2 <- 0.4 * cos(df$degree * pi / 180) # 40% ring y
  df$o1 <- 0.2 * sin(df$degree * pi / 180) # 20% ring x
  df$a1 <- 0.2 * cos(df$degree * pi / 180) # 20% ring y
  
  
  p = plot_ly()
  x=school1
  y=school2
  
  loc1<-which(df$school==x)[1]
  loc2<-which(df$school==y)[1]
  
  for(i in 1:6) {
    p <- add_trace(
      p, 
      x = c(df$o5[i],0), 
      y = c(df$a5[i],0), 
      #evaluate = TRUE,
      line = list(color = "#d3d3d3", dash = "3px"),
      showlegend = FALSE
    )
  }
  
  d<-df[c(1:6,1),]
  dd<-df[c(loc1:(loc1+5),loc1,loc2:(loc2+5),loc2),]
  m_radar <- list(
    l = 20,
    r = 40,
    b = 0,
    t = 10
  )
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  p %>% 
    add_trace(data = dd, x = dd$o, y = dd$a, color = factor(dd$school), 
              mode = "lines+markers",
              hoverinfo = "text",
              text = paste(dd$response, dd$score)) %>% 
    add_trace(data = d, x = d$o5, y = d$a5, 
              text = d$response,
              hoverinfo = "none",
              textposition = "top middle", mode = "lines+text", 
              line = list(color = "#d3d3d3", dash = "3px", shape = "spline"),
              showlegend = FALSE) %>% 
    add_trace(data = d, x = d$o4, y = d$a4, mode = "lines",
              line = list(color = "#d3d3d3", dash = "3px", shape = "spline"), 
              hoverinfo = "none",
              showlegend = FALSE) %>% 
    add_trace(data = d, x = d$o3, y = d$a3, mode = "lines", 
              line = list(color = "#d3d3d3", dash = "3px", shape = "spline"), 
              hoverinfo = "none",
              showlegend = FALSE) %>%
    add_trace(data = d, x = d$o2, y = d$a2, mode = "lines", 
              line = list(color = "#d3d3d3", dash = "3px", shape = "spline"), 
              hoverinfo = "none",
              showlegend = FALSE) %>%
    add_trace(data = d, x = d$o1, y = d$a1, mode = "lines", 
              line = list(color = "#d3d3d3", dash = "3px", shape = "spline"), 
              hoverinfo = "none",
              showlegend = FALSE) %>%
    layout(
      # autosize = FALSE,
      # hovermode = "closest",     
      # autoscale = TRUE,
      # width = 350,
      # height = 350,
      xaxis =ax,
      yaxis = ax,
      legend=list(orientation = 'h'),
      # lot_bgcolor='rgb(254, 247, 234)',
      margin=m_radar
      )
  
  
}

trans_theme <- theme(
  panel.grid.minor = element_blank(), 
  panel.grid.major = element_blank(),
  panel.background = element_rect(fill=NA),
  plot.background = element_rect(fill=NA)
)


br.df<-loc_data
br.df<-br.df[,-1:-2]
br.name<-c("Manhattan","Brooklyn","Bronx","Queens","Staten Island")


mht<-br.df[br.df$borough==br.name[1],1]
brkl<-br.df[br.df$borough==br.name[2],1]
brx<-br.df[br.df$borough==br.name[3],1]
qs<-br.df[br.df$borough==br.name[4],1]
sti<-br.df[br.df$borough==br.name[5],1]


############### RANK DATA SETS
data.ranked.quant <- read.csv("./data/data.ranked.quant.csv")
head(data.ranked.quant)
data.ranked.quant$X <- NULL
colnames(data.ranked.quant) <- c("Ranking", "School Name", "Graduation Rate", "Post-secondary Enrollment Rate, 6 Months", "Averaged Score (%)")

data.ranked.math <- read.csv("./data/data.ranked.math.csv")
data.ranked.math$X <- NULL
colnames(data.ranked.math) <- c("Ranking", "School Name", "Algebra Score (%)", "Geometry Score (%)", "Averaged Math Score (%)")

data.ranked.english <- read.csv("./data/data.ranked.english.csv")
data.ranked.english$X <- NULL
colnames(data.ranked.english) <- c("Ranking", "School Name", "English Score (%)")

data.ranked.history <- read.csv("./data/data.ranked.history.csv")
data.ranked.history$X <- NULL
colnames(data.ranked.history) <- c("Ranking", "School Name", "US History Score (%)", "Global History Score (%)", "Averaged History Score (%)")

data.ranked.science <- read.csv("./data/data.ranked.science.csv")
data.ranked.science$X <- NULL
colnames(data.ranked.science) <- c("Ranking", "School Name", "Chemistry Score (%)", "Physics Score (%)", "Averaged Science Score (%)")

data.ranked.allsubjects <- read.csv("./data/data.ranked.allsubjects.csv")
data.ranked.allsubjects$X <- NULL
colnames(data.ranked.allsubjects) <- c("Ranking", "School Name", "Averaged Score - all subjects (%)")

data.ranked.RI <- read.csv("./data/data.ranked.RI.csv")
data.ranked.RI$X <- NULL
colnames(data.ranked.RI) <- c("Ranking", "School Name", "Averaged Score (%)")

data.ranked.CT <- read.csv("./data/data.ranked.CT.csv")
data.ranked.CT$X <- NULL
colnames(data.ranked.CT) <- c("Ranking", "School Name", "Averaged Score (%)")

data.ranked.SE <- read.csv("./data/data.ranked.SE.csv")
data.ranked.SE$X <- NULL
colnames(data.ranked.SE) <- c("Ranking", "School Name", "Averaged Score (%)")

data.ranked.ESL <- read.csv("./data/data.ranked.ESL.csv")
data.ranked.ESL$X <- NULL
colnames(data.ranked.ESL) <- c("Ranking", "School Name", "Averaged Score (%)")

data.ranked.SFCT <- read.csv("./data/data.ranked.SFCT.csv")
data.ranked.SFCT$X <- NULL
colnames(data.ranked.SFCT) <- c("Ranking", "School Name", "Averaged Score (%)")

data.ranked.T <- read.csv("./data/data.ranked.T.csv")
data.ranked.T$X <- NULL
colnames(data.ranked.T) <- c("Ranking", "School Name", "Averaged Score (%)")

data.ranked.allsurvey <- read.csv("./data/data.ranked.allsurvey.csv")
data.ranked.allsurvey$X <- NULL
colnames(data.ranked.allsurvey) <- c("Ranking", "School Name", "Averaged Score (%)")

################# lm Analysis ###############
packages.used=c("shiny","readxl","ggplot2","shinydashboard","readr")
packages.needed=setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,repos='http://cran.us.r-project.org')
}
library(shiny)
library(readxl)
library(readr)
library(ggplot2)
library(shinydashboard)


### Load sheets from the raw data
achievements <- read_excel("./data/2014_2015_HS_SQR_Results_2016_01_07.xlsx", 
                           sheet = "Student Achievement",skip=1)
framework <- read_excel("./data/2014_2015_HS_SQR_Results_2016_01_07.xlsx", 
                        sheet = "Framework",skip=1)

borough.df <- read_csv("./data/HS_loc_with_borough.csv")


### Clean up the data
# Create df per category and rename columns so that it's easy to refer to.
# For column names, C in C1, C2... stands for categorical value. N for numerical value.

rigorous.col = c("School Name", "Rigorous Instruction - Element Score",
                 "Percentage of students who say that they learn a lot from feedback on their work",
                 "Percentage of students who know what their teacher wants them to learn in class",
                 "Percentage of teachers who say that students build on each others' ideas during class discussions")
rigorous.df = framework[rigorous.col]
rigorous.col.short = c("SchoolName","Score","N1","N2","N3")
colnames(rigorous.df) = rigorous.col.short
names(rigorous.col) = rigorous.col.short #rigorous.col[['C1']] to refer to the name


teachers.col = c("School Name","Collaborative Teachers - Element Score",
                 "Percentage of teachers who say that they work together to design instructional programs",
                 "Percentage of teachers who say that they have opportunities to work productively with colleagues in their school",
                 "Percentage of teachers who say that they feel responsible that all students learn")
teachers.df = framework[teachers.col]
teachers.col.short = c("SchoolName","Score","N1","N2","N3")
colnames(teachers.df) = teachers.col.short
names(teachers.col) = teachers.col.short


supportive.col = c("School Name","Supportive Environment - Element Score",
                   "Percentage of students who feel safe in the hallways, bathrooms, locker room, and cafeteria",
                   "Percentage of students who say that teachers notice when they are upset or having emotional difficulty",
                   "Percentage of students who say that this school supports students in navigating the post-secondary process")
supportive.df = framework[supportive.col]
supportive.col.short = c("SchoolName","Score","N1","N2","N3")
colnames(supportive.df) = supportive.col.short
names(supportive.col) = supportive.col.short


leadership.col = c("School Name", "Effective School Leadership - Element Score",
                   "Percentage of teachers who say that the principal communicates a clear vision for this school",
                   "Percentage of teachers who say that curriculum and instruction are well coordinated across different grade levels",
                   "Percentage of parents who feel that the principal works to create a sense of community in the school")
leadership.df = framework[leadership.col]
leadership.col.short = c("SchoolName","Score","N1","N2","N3")
colnames(leadership.df) = leadership.col.short
names(leadership.col) = leadership.col.short


community.col = c("School Name","Strong Family-Community Ties - Element Score",
                  "Percentage of parents who say that school staff regularly communicate with them about how the staff can help their children learn",
                  "Percentage of parents who feel that teachers try to understand families' problems and concerns",
                  "Percentage of teachers who say that teachers at this school work closely with families to meet students' needs")
community.df = framework[community.col]
community.col.short = c("SchoolName","Score","N1","N2","N3")
colnames(community.df) = community.col.short
names(community.col) = community.col.short

trust.col = c("School Name","Trust - Element Score",
              "Percentage of teachers who say they trust the principal",
              "Percentage of teachers who say that they trust each other",
              "Percentage of parents who say that school staff work hard to build trusting relationships with them",
              "Percentage of students who say that teachers treat them with respect")
trust.df = framework[trust.col]
trust.col.short = c("SchoolName","Score","N1","N2","N3","N4")
colnames(trust.df) = trust.col.short
names(trust.col) = trust.col.short

achievements.col = c("School Name","Metric Value - Graduation Rate, 4 year",
                     "Metric Value - Postsecondary Enrollment Rate, 6 months After High School")
achievements = achievements[achievements.col]
achievements = na.omit(achievements)
achievements.col.short = c("SchoolName","Graduate_Pct",
                           "Postsecondary_Enrollment_Pct")
colnames(achievements) = achievements.col.short
names(achievements.col) = achievements.col.short

colnames(borough.df)[3] = "SchoolName"
achievements = merge(x = achievements, y = borough.df[c('SchoolName', 'borough')], 
                     by = "SchoolName", all.x = TRUE)

rigorous.df <- merge(x = achievements, y = rigorous.df, by = "SchoolName", all.x = TRUE)
teachers.df <- merge(x = achievements, y = teachers.df, by = "SchoolName", all.x = TRUE)
supportive.df <- merge(x = achievements, y = supportive.df, by = "SchoolName", all.x = TRUE)
leadership.df <- merge(x = achievements, y = leadership.df, by = "SchoolName", all.x = TRUE)
community.df <- merge(x = achievements, y = community.df, by = "SchoolName", all.x = TRUE)
trust.df <- merge(x = achievements, y = trust.df, by = "SchoolName", all.x = TRUE)


############################################################
##############CALCULATER############
vardata<-read.csv("./data/HS_var.csv")
var2data<-read.csv("./data/HS_var2.csv")
#var2data<-cbind(Rank=rep(NA,487),var2data)
rownames(vardata)<-vardata$School.Name
vardata$School.Name<-c()









