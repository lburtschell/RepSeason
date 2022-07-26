library(lubridate)
library(circular)
library(ggplot2)
library(ggsignif)

#Function to calculate r_length
r_from_births<-function(birthsVector,yearLength){
 if(length(birthsVector>0)){
   births_days<-birthsVector%%yearLength
   births_days[births_days==0]<-yearLength
   births_degrees<-births_days*2*pi/yearLength
   r_mean=mean.circular(births_degrees)%%(2*pi)
   mean_day<-r_mean*yearLength/(2*pi)
   mean_month<-mean_day*12/yearLength+1
   r_length=rho.circular(births_degrees)
   rayleigh=rayleigh.test(births_degrees)
   return (list("r_length"=r_length,"r_mean"=r_mean, "mean_day"=mean_day,"mean_month"=mean_month,"rayleigh"=rayleigh)) 
 }
  else{
    return (list("r_length"=NA,"r_mean"=NA, "mean_day"=NA,"mean_month"=NA,"rayleigh"=NA)) 
  }

}

#Function to plot a circular graphs
circular_graph<-function(birthsVector_all,titleName,yearLength){
  births_days<-birthsVector_all%%yearLength
  births_days[births_days==0]<-yearLength
  
  births_degrees<-births_days*2*pi/yearLength
  r_mean=mean.circular(births_degrees)%%(2*pi)
  mean_day<-r_mean*yearLength/(2*pi)
  mean_month<-mean_day*12/yearLength+1
  r_length=rho.circular(births_degrees)
  mean_day2<-as_date("2010-01-01")
  mean_day2<-mean_day2+mean_day-1
  mean_day2<-paste(day(mean_day2),month(mean_day2,label = TRUE,locale="English"))
  df_segment<-data.frame("x"=mean_month,"xend"=mean_month,"y"=0,"yend"=r_length,"meanDay"=mean_day2,"strategy"="Simulated births (Optimal strategies)")
  
  
  #merge births by month for plot
  births_months<-floor(births_days*12/yearLength)+1
  births_obs<-vector()
  for (i in 1:12){
    births_obs[i]<-length(births_months[births_months==i])/length(births_months)
  }
  monthsNumb<-seq(1,12,1)
  monthsNumb<-as.factor(monthsNumb)
  months<-c("Jan","Feb","Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov","Dec")
  months<-factor(months, levels=months)
  df_births<-data.frame(monthsNumb,months,births_obs)
  df_births$strategy<-"Simulated births (Optimal strategies)"
  
  df_births_tot<-df_births
  
  #real births
  distances<-c(46.31,39.77,43.86,35.41,30.66,35.85,47.77,49.12,48.03,50.55,46.42,52.73)
  births<-(distances/151.7)^2
  df_births$births_obs<-births
  df_births$strategy<-"Observed births (Amboseli)"
  
  df_births_tot<-rbind(df_births_tot,df_births)
  
  births_months<-vector()
  for (i in 1:12){
    births_months<-c(births_months,rep(i, round(df_births$births_obs[i]*100)))
  }
  
  births_degrees<-births_months*2*pi/12
  r_mean=mean.circular(births_degrees)%%(2*pi)
  mean_day<-r_mean*yearLength/(2*pi)
  mean_month<-r_mean*12/(2*pi)+1
  r_length=rho.circular(births_degrees)
  mean_day2<-as_date("2010-01-01")
  mean_day2<-mean_day2+mean_day-1
  mean_day2<-paste(day(mean_day2),month(mean_day2,label = TRUE,locale="English"))
  
  df_segment<-rbind(df_segment,c(mean_month,mean_month,0,0.15,"meanDay"=mean_day2,"Observed births (Amboseli)"))
  df_segment$xend<-as.numeric(df_segment$xend)
  df_segment$x<-as.numeric(df_segment$x)
  df_segment$yend<-as.numeric(df_segment$yend)
  df_segment$y<-as.numeric(df_segment$y)
  
  note<-paste("r = ",round(r_length,2))
  colwellPalette <- c("#FFFFBF", "#FA8A64", "#B53475", "#4F1373", "#000000")
  bluePalette<-c("#132B43","#326A98","#438AC3","#56B1F7")
  
  myPalette<-bluePalette
  
  df_births_tot$strategy_f<-factor(df_births_tot$strategy,levels=c("Simulated births (Optimal strategies)","Observed births (Amboseli)"))
  df_segment$strategy_f<-factor(df_segment$strategy,levels=c("Simulated births (Optimal strategies)","Non seasonal strategy","Observed births (Amboseli)"))
  
  ggplot(df_births_tot, aes(monthsNumb, births_obs)) + #fill=births_obs
    geom_bar(stat = "identity") +
    #scale_fill_gradient("Proportion of births during month")+ #limits=c(0,1),colors=myPalette
    scale_x_discrete("",labels = df_births$months)+
    scale_y_sqrt("Proportion of births",limits=c(0,1))+
    coord_polar(clip="off") +
    geom_segment(data=df_segment,aes(x=x,xend=xend,y=y,yend=yend),inherit.aes = FALSE,colour = "red", size = 1)+
    geom_text(data=df_segment,aes(x=8.5,y=1,label=paste("r =",round(yend,2))),vjust = 6,inherit.aes = FALSE,colour="red")+
    geom_text(data=df_segment,aes(x=4.5,y=1,label=paste("μ =",meanDay)),vjust = 6,inherit.aes = FALSE,colour="red")+
    
    #ggtitle(titleName)+
    theme_light()+
    theme(legend.position="bottom")+
    facet_grid(~strategy_f)
  
}
