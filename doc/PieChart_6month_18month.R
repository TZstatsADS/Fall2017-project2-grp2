library(tibble)
library(dplyr)
library(tidyr)
library('RColorBrewer')

tidysixmonth<-read.csv("../data/tidysix.csv")
library(plotly)
#col<- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
pieplotsixmonth<-function(school_ID){
  df<-tidysixmonth%>%filter(ID==school_ID)
  school_name<-df$`SchoolName`[1]
  if(is.na(df$Number[1]) == TRUE) {
    print("no available data")
  } else {
  plot_ly(df, labels = ~Type, values = ~Number, type = 'pie',textposition = 'inside',
          textinfo = 'label+percent',insidetextfont = list(color = '#FFFFFF')) %>%
    layout(title = school_name,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  
}

tidyeighteenmonth<-read.csv("../data/tidyeighteen.csv")
library(plotly)
#col<- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
pieploteighteenmonth<-function(school_ID){
  df<-tidyeighteenmonth%>%filter(ID==school_ID)
  school_name<-df$`SchoolName`[1]
  if(is.na(df$Number[1]) == TRUE) {
    print("no available data")
  } else {
  plot_ly(df, labels = ~Type, values = ~Number, type = 'pie',textposition = 'inside',
          textinfo = 'label+percent',insidetextfont = list(color = '#FFFFFF')) %>%
    layout(title = school_name,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  
}
pieplotsixmonth("30Q286")
pieploteighteenmonth("14K614")


