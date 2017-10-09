library(tibble)
library(dplyr)
library(tidyr)
library('RColorBrewer')
col=alpha(brewer.pal(8, "Set2"))[1:3]

##if you want SAT
SAT_HS<-read.csv("../Fall2017-project2-grp2/data/SAT_HS.csv")
SAT_HS<-Del_blank_row(SAT_HS)
SAT_HS<-SAT_HS%>%
  select(School.Name,Average.Score.SAT.Math,Average.Score.SAT.Writing,Average.Score.SAT.Critical.Reading)

rownames(SAT_HS)<-as.vector(SAT_HS$School.Name)
SAT_HS$School.Name<-c()
colnames(SAT_HS)<-c("Math","Writing","Reading")
SAT_HS<-SAT_HS[-which(apply(apply(SAT_HS,1,is.na),2,all)),]
tidysat <- SAT_HS %>% 
  rownames_to_column("Name") %>% 
  gather(key = Sub, value = Score,-Name)
tidysat$Sub<-as.factor(tidysat$Sub)

# ##if you want Regents
# Regents_HS<-Del_blank_row(Regents_HS)
# Regents_HS<-Regents_HS%>%
#   select(School.Name,Average.Score.Regents.Algebra,Average.Score.Regents.Geometry,
#          Average.Score.Regents.Algebra.2.Trig,Average.Score.Regents.English,Average.Score.Regents.US.History,
#          Average.Score.Regents.Global.History,Average.Score.Regents.Chemistry,Average.Score.Regents.Physics,
#          Average.Score.Regents.Earth.Science,Average.Score.Regents.Living.Environment)
# 
# 
# rownames(Regents_HS)<-as.vector(Regents_HS$School.Name)
# SAT_HS$School.Name<-c()
# colnames(SAT_HS)<-c("Math","Writing","Critical Reading")
# 
# Regents_HS<-Regents_HS[-which(apply(apply(Regents_HS,1,is.na),2,all)),]


compare_sat_plot<-function(school1,school2){
  
df<-tidysat%>%
  filter(Name%in%c(school1,school2))

# ggplot(df,aes(x=Sub,y=Score,fill=Name))+
#   geom_bar(stat="identity",position="dodge",width = 0.5)+
#   ggtitle ("Compare the Avg SAT scores of 2 schools")+
#   theme(legend.position="bottom")+
#   labs(x="Subject")

plot_ly(y =~Score,x=~Sub,data=df%>%filter(Name==school1),type = "bar",name=school1)%>%
  add_trace(y =~Score,x=~Sub,data=df%>%filter(Name==school2),name=school2)%>%
  layout(title="Compare the Avg SAT Scores of 2 Schools",yaxis = list(title = 'Score'), xaxis = list(title = 'Subject'),
         barmode = 'group',legend=list(orientation = 'h'))


}
compare_sat_plot(tidysat$Name[1],tidysat$Name[5])
