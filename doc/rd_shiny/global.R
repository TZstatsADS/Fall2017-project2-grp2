library("xlsx")
library("plotly")
library("dplyr")

HS_frame<-read.xlsx("~/Desktop/[ADS]Advanced Data Science/Fall2017-project2-grp2/data/2014_2015_HS_SQR_Results_2016_01_07.xlsx",sheetName = "Framework",header = T)
col<-apply(HS_frame[1,],2,as.character)
HS_frame<-HS_frame[2:1254,]
colnames(HS_frame)<-col
HS_frame<-HS_frame%>%filter(!is.na(`School Name`))
Rigorous_instruction<-HS_frame[,1:16]#3:16
Collaborative_Teacher<-HS_frame[,c(1,2,17:28)]
Supportive_env<-HS_frame[,c(1,2,29:49)]
Leadership<-HS_frame[,c(1,2,50:60)]
Fam_Sch_Tie<-HS_frame[,c(1,2,61:71)]
Trust<-HS_frame[,c(1,2,72:89)]
rm(HS_frame)

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

response<-rep(c("rigorous","collaborative teacher","supportive environment","leadership","family-school tie","trust"),487)

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

