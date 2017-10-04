library(ggmap)
library('readr')
loc_index<-read.csv("../loc_index.csv",as.is = F)

#choose the start and end you want, don't set a very long time, you might hit the data limits
#i have already done the first 6000 rows, check the index where the NAs starts
min(which(is.na(loc_index$lat)))

start=5624
end=6000

#in case of changing the original data
df<-loc_index[start:end,]

for(i in start:end){
  print(i)
  df[i,c("lon","lat")]<-geocode(loc_index$loc[i])
  if(is.na(df[i,"lat"])){print("NA")}
}


#because of the query limit, sometimes we will get NAs, just call the function again to fill them
while(sum(is.na(df$lat))>0){
  for(i in which(is.na(df$lat))){
    print(i)
    df[i,c("lon","lat")]<-geocode(loc_index$loc[i])
    if(is.na(df[i,"lat"])){print("NA")}
  }
}
#after you have done with this, save the df as a csv
write.csv(df,paste("../df",start,"_",end,".csv",sep = ""))


