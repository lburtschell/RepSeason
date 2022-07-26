library(lubridate)

#setwd("D:/NextCloud/ReproductiveSeasonality_R")  #au bureau
#setwd("C:/Users/Lugdiwine/ownCloud/ReproductiveSeasonality_R")  #ordi HP
#setwd("C:/Users/lugdiwine.burtschell/Owncloud/ReproductiveSeasonality_R")   #ordi dell

modifyNDVI<-function(raw_NDVI,yearLength,n){ #raw_NDVI is the output of extractNDVI, n = number of repetitions
  #add date columns
  raw_NDVI$DayOfYear<-yday(raw_NDVI$Date) #real days of year
  raw_NDVI$DayOfModifiedYear<-round(raw_NDVI$DayOfYear*yearLength/365) #if yearLength !=365
  raw_NDVI$Days<-raw_NDVI$DayOfModifiedYear+(year(raw_NDVI$Date)-year(min(raw_NDVI$Date)))*yearLength #days since first day in dataframe
  
  #interpol by day
  interpol<-approx(raw_NDVI$Days,raw_NDVI$NDVI,seq(raw_NDVI$Days[1],raw_NDVI$Days[length(raw_NDVI$Days)],by=1))
  NDVI_inter<-interpol$y
  Days_inter<-interpol$x
  DaysOfYear<-Days_inter%%yearLength
  DaysOfYear[DaysOfYear==0]<-yearLength
  
  #looking for the least variable day of year between first and last years
  NDVI_gap<-vector()
  NDVI_first<-NDVI_inter[1:yearLength]
  NDVI_last<-NDVI_inter[(length(NDVI_inter)-yearLength+1):length(NDVI_inter)]
  Days_first<-DaysOfYear[1:yearLength]
  Days_last<-DaysOfYear[(length(DaysOfYear)-yearLength+1):length(DaysOfYear)]
  for (i in 1:yearLength){
    NDVI_gap[i]<-abs(NDVI_first[Days_first==i]-NDVI_last[Days_last==i])
  }
  min_date<-which.min(NDVI_gap)
  all_min_date<-which(DaysOfYear==min_date)
  
  #concatenate n series
  startOfSeries<-all_min_date[1]
  endOfSeries<-all_min_date[length(all_min_date)]-1
  NDVI_n<-c(NDVI_inter[1:endOfSeries],rep(NDVI_inter[startOfSeries:endOfSeries],n),NDVI_inter[startOfSeries:endOfSeries])
  DaysOfYear_n<-c(DaysOfYear[1:endOfSeries],rep(DaysOfYear[startOfSeries:endOfSeries],n),DaysOfYear[startOfSeries:endOfSeries])
  
  #start 1st of january
  January1<-which(DaysOfYear==1)[1]
  df_NDVI<-data.frame("NDVI"=NDVI_n[January1:length(NDVI_n)],"DayOfYear"=DaysOfYear_n[January1:length(DaysOfYear_n)])
  return(df_NDVI)
}
