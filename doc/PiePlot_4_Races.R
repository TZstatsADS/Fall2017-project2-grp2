# #library('RColorBrewer')
# #col=alpha(brewer.pal(8, "Set3"))
# HS_summary<-read.xlsx("~/Desktop/2014_2015_HS_SQR_Results_2016_01_07.xlsx",
#                       sheetName = "Summary",header = T)
# col<-apply(HS_summary[1,],2,as.character)
# HS<-HS_summary[2:492,]
# colnames(HS_summary)<-col
# HS_summary<-HS_summary[-1,]
# 
# Race<-HS_summary[,c(1,2,4,32:35)]
# rownames(Race)<-HS_summary$DBN
# Race$DBN<-c()
# Race[,2:6]<-apply(Race[,2:6],2,as.numeric)
# #Race[,3:6]<-t(apply(Race[,3:6],1,function(vec){return(vec/sum(vec))}))
# for(i in 1:491){
#   Race[i,3:6]<-round(Race[i,3:6]*Race[i,2],0)
# }
# Race$Enrollment<-c()
# colnames(Race)[2:5]<-c("Asian","Black","Hispanic","White")
# 
# tidyrace <- Race %>% 
#   rownames_to_column("ID") %>% 
#   gather(key = Race, value = Number,-ID,-`School Name`)
#write.csv(tidyrace,"~/Desktop/tidyrace.csv")
tidyrace<-read.csv("../data/tidyrace.csv")
library(plotly)
col<- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
pieplot4race<-function(school_ID){
  df<-tidyrace%>%filter(ID==school_ID)
  school_name<-df$`School Name`[1]
  plot_ly(df, labels = ~Race, values = ~Number, type = 'pie',textposition = 'inside',
          textinfo = 'label+percent',insidetextfont = list(color = '#FFFFFF'),marker=list(colors=col)) %>%
    layout(title = school_name,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
}

pieplot4race("03M479")
