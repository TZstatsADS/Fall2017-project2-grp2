
library("plotly")
library("dplyr")
library('RColorBrewer')

#write.csv(HS_frame,"~/Desktop/HS_frame.csv")
HS_frame<-read.csv("../data/HS_frame.csv",as.is = F)
HS_frame$School.Name<-as.character(HS_frame$School.Name)
# HS_frame<-HS_frame[2:1254,]
# colnames(HS_frame)<-col
# HS_frame<-HS_frame%>%filter(!is.na(`School Name`))

Aspects<-list(
  'Rigorous Instruction Rating'=1:16,	
  'Collaborative Teachers Rating'=c(1,2,17:28),	
  'Supportive Environment Rating'=c(1,2,29:49),
  'Effective School Leadership Rating'=c(1,2,50:60),
  'Strong Family-Community Ties Rating'=c(1,2,61:71),
  'Trust Rating'=c(1,2,72:89)
)

Rigorous_instruction<-HS_frame[,1:16]#3:16
Collaborative_Teacher<-HS_frame[,c(1,2,17:28)]
Supportive_env<-HS_frame[,c(1,2,29:49)]
Leadership<-HS_frame[,c(1,2,50:60)]
Fam_Sch_Tie<-HS_frame[,c(1,2,61:71)]
Trust<-HS_frame[,c(1,2,72:89)]
rm(HS_frame)



plot_scores<-function(df,school_name){
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
    layout(title=school_name,yaxis1=list(title="Freq"),xaxis=list(title="Elementary Score"),yaxis2 = list(title="Density",overlaying = "y", side = "right", showgrid = FALSE)
           ,legend = list(x = -0.05, y = 1.25),margin = list(r=50))
  
  # p2<-plot_ly(x = df$ele_score,showscale=F)%>%
  #   add_boxplot(showlegend=F,showscale=F,boxpoints = "all",name=deparse(substitute(df)))%>%
  #   add_trace(x=s_i,y=deparse(substitute(df)),name=school_name,type="scatter",mode="markers",
  #             marker = list(size = 10, color = 'rgba(152, 0, 0, .8)'),
  #             text=paste("Score :", s_i, "<br>Rating:",r_i,'<br>Higher than:',h_i*100,
  #                       "%","<br>Rank:",rank_i))%>%
  #    layout(title=school_name)
  
}  

plot_scores(Leadership,Leadership$School.Name[30])

