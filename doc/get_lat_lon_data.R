library(ggmap)
library('readr')
rest_data<-read.csv("../DOHMH_New_York_City_Restaurant_Inspection_Results.csv",as.is = F)
loc_rest<-paste(rest_data$BUILDING,rest_data$STREET,"NYC",sep  = " ")
names(loc_rest)<-1:ncol(rest_data)

#choose the start and end you want, don't set a very long time, you might hit the data limits
start=6001
end=6500
#in case of changing the original data
df<-rest_data[start:end,]
df$lon<-NA
df$lat<-NA
df$addr<-NA


for(i in start:end){
  print(i)
  df[i,c("lon","lat")]<-geocode(loc_rest[i])
  if(is.na(df[i,"lat"])){print("NA")}
}


#because of the query limit, sometimes we will get NAs, just call the function again to fill them
while(sum(is.na(df$lat))>0){
  for(i in which(is.na(df$lat))){
    print(i)
    df[i,c("lon","lat")]<-geocode(loc_rest[i])
    if(is.na(df[i,"lat"])){print("NA")}
  }
}
#after you have done with this, save the df as a csv
write.csv(df[start:end],paste("../df",start,"_",end,".csv",sep = ""))


