library(ggplot2)
library(ggpubr)
library(reshape2)
library(rstatix)

#Change working directory to folder with your files. This command works if you use RStudio, if you use R directly you will need to use setwd with the file location.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#This script uses the datasets created with Create_heatmap_data.R and stored in a "Heatmaps" repository 

#####Functions used  to plot heatmap###########

plot_heatmap<-function(heatmap_df,title){
  heatmap_df$r_length[heatmap_df$lambda==0]<-NA
  ggplot(data=heatmap_df, aes(x=as.factor(seasonality), y=as.factor(productivity),fill= r_length)) +
    geom_tile()+
    scale_fill_gradient(low="white", high="red",na.value="grey",limits=c(0,1)) +
    coord_equal() + 
    labs(x = "Environmental \n seasonality",y = "Environmental productivity",fill="Birth\nseasonality    \n(r)",title=title)+
    theme(plot.title = element_text(hjust = 0.5))
}

plot_heatmap_lambda<-function(heatmap_df,title){
  heatmap_df$r_length[heatmap_df$lambda==0]<-NA
  ggplot(data=heatmap_df, aes(x=as.factor(seasonality), y=as.factor(productivity),fill= lambda)) +
    geom_tile()+
    scale_fill_distiller(palette = "Blues") + 
    coord_equal() + 
    labs(x = "Environmental \n seasonality",y = "Environmental \n productivity",fill=expression(paste("Fitness (",lambda[ind],")")),title=title)+
    theme(plot.title = element_text(hjust = 0.5))
}

MeanSd <- function(df_HX){
  means<-round(aggregate(df_HX$value,list(df_HX$variable),FUN=mean, na.rm=TRUE)$x,2)
  sds<-round(aggregate(df_HX$value,list(df_HX$variable),FUN=sd, na.rm=TRUE)$x,2)
  return(paste(means,"Â±",sds))
}

############################
###TEST H1 and H2##########
###########################
# #normal H1 and H2
load(file="Heatmaps/heatmap_df_N.RData")
heatmap_df_N<-heatmap_df
pH12a<-plot_heatmap(heatmap_df_N,"") + theme(legend.position = "bottom")
pH12a_lambda<-plot_heatmap_lambda(heatmap_df_N,"u = 1; GR = 5g/day;\n IBI = 1.7 YL; M = 11.61%") #plot_heatmap(heatmap_df_N,"Normal conditions") 


pH1<-ggplot(data = heatmap_df_N,aes(x=seasonality,y=r_length,color=as.factor(productivity),group=as.factor(productivity)))+
  geom_point(size=2)+
  geom_smooth(method=lm,se=FALSE)+
  ylim(0,1)+
  theme(legend.position = "right")+
  labs(x = "Environmental seasonality",y ="Birth seasonality (r)", colour="Environmental\nproductivity",title="")

pH2<-ggplot(data = heatmap_df_N,aes(x=productivity,y=r_length,color=as.factor(seasonality),group=as.factor(seasonality)))+
  geom_point(size=2)+
  geom_smooth(method=lm,se=FALSE)+
  ylim(0,1)+
  theme(legend.position = "right")+
  labs(x = "Environmental productivity",y ="Birth seasonality (r)", colour="Environmental\nseasonality",title="")

# pH12b<-ggplot(heatmap_df_N,aes(x="",y=r_length))+ #x="Normal conditions"
#   geom_violin(draw_quantiles =0.5)+   #
#   stat_summary(fun="mean")+
#   scale_x_discrete(name="",)+
#   scale_y_continuous(name="Birth seasonality (r)",limits = c(0,1.06))+
#   theme(axis.title=element_text(size=12))


pFig2<-ggarrange(pH12a,pH1,pH2,ncol=3, labels=c("A","B","C"))

############################
########TEST H3############
############################

#no unpredictability U0 
load(file="Heatmaps/heatmap_df_U0.RData")
heatmap_df_U0<-heatmap_df
pU0<-plot_heatmap(heatmap_df_U0,"u = 0")
pU0_lambda<-plot_heatmap_lambda(heatmap_df_U0,"u = 0")

#normal conditions U=1
pU1<-plot_heatmap(heatmap_df_N,"u = 1")

#U=2
load(file="Heatmaps/heatmap_df_U2.RData")
heatmap_df_U2<-heatmap_df
pU2<-plot_heatmap(heatmap_df_U2,"u = 2")
pU2_lambda<-plot_heatmap_lambda(heatmap_df_U2,"u = 2")

#U=3
load(file="Heatmaps/heatmap_df_U3.RData")
heatmap_df_U3<-heatmap_df
pU3<-plot_heatmap(heatmap_df_U3,"u = 3")
pU3_lambda<-plot_heatmap_lambda(heatmap_df_U3,"u = 3")

#all together
pH3a<-ggarrange(pU0,pU1,pU2,pU3,common.legend = TRUE,ncol = 4,legend="left")


#boxplots
df_H3<-data.frame("seasonality"=heatmap_df_N$seasonality,"productivity"=heatmap_df_N$productivity,
                   "U0"=heatmap_df_U0$r_length,"U1"=heatmap_df_N$r_length,
                   "U2"=heatmap_df_U2$r_length,"U3"=heatmap_df_U3$r_length)


df_H3<-melt(df_H3,measure.vars = c("U0","U1","U2","U3"))

pH3b<-ggplot(df_H3,aes(x=variable,y=value))+
  geom_violin(draw_quantiles =0.5)+ 
  annotate("text",x=c(1,2,3,4),y=0.1+aggregate(df_H3$value,list(df_H3$variable),FUN=max, na.rm=TRUE)$x,label=MeanSd(df_H3))+
  stat_summary(fun="mean")+
  scale_x_discrete(name="Unpredictability", labels=c("0","1","2","3"))+
  scale_y_continuous(name="Birth seasonality (r)",limits = c(0,1.06))+
  theme(axis.title=element_text(size=12))


pFig4<-ggarrange(pH3a,pH3b,ncol=1)


############################
########TEST H4############
############################
#high growth rate (daily reproductive energy expenditure) GR = 1.5
load(file="Heatmaps/heatmap_df_GR15.RData")
heatmap_df_GR15<-heatmap_df
pGR15<-plot_heatmap(heatmap_df_GR15,"GR = 7.5 g/day")
pGR15_lambda<-plot_heatmap_lambda(heatmap_df_GR15,"GR = 7.5 g/day")

#normal conditions GR = 1
pGR1<-plot_heatmap(heatmap_df_N,"GR = 5 g/day")

#all together
pH41<-ggarrange(pGR1,pGR15,common.legend = TRUE,ncol = 2,legend="left")

#boxplot
df_H4<-data.frame("seasonality"=heatmap_df_N$seasonality,"productivity"=heatmap_df_N$productivity,
                  "GR1"=heatmap_df_N$r_length,"GR15"=heatmap_df_GR15$r_length)
df_H4<-melt(df_H4,measure.vars = c("GR1","GR15"))


pH42<-ggplot(df_H4,aes(x=variable,y=value))+
  geom_violin(draw_quantiles = 0.5)+ 
  annotate("text",x=c(1,2),y=0.1+aggregate(df_H4$value,list(df_H4$variable),FUN=max, na.rm=TRUE)$x,label=MeanSd(df_H4))+
  stat_summary(fun="mean")+
  scale_x_discrete(name="Growth rate (Daily reproductive energy expenditure)",labels=c("5 g/day","7.5 g/day"))+
  scale_y_continuous(name="Birth seasonality (r)",limits = c(0,1.06))+
  theme(axis.title=element_text(size=12))


#final graph
pFig5<-ggarrange(pH41,pH42,ncol=1,heights = c(2,1.5))

         
############################
########TEST H5 and 6#######
############################
#unpredictability
#IBI (637) = year length (637)
load(file="Heatmaps/heatmap_df_YL637.RData")
heatmap_df_YL637<-heatmap_df
pYL637<-plot_heatmap(heatmap_df_YL637,"IBI = 1 YL")
pYL637_lambda<-plot_heatmap_lambda(heatmap_df_YL637,"IBI = 1 YL")

#IBI  = 1.5 year length (425)
load(file="Heatmaps/heatmap_df_YL425.RData")
heatmap_df_YL425<-heatmap_df
pYL425<-plot_heatmap(heatmap_df_YL425,"IBI = 1.5 YL")
pYL425_lambda<-plot_heatmap_lambda(heatmap_df_YL425,"IBI = 1.5 YL")

#IBI (637) = 1.75 year length (365)
pYL365<-plot_heatmap(heatmap_df_N,"IBI = 1.7 YL")

#all together
pH51<-ggarrange(pYL637,pYL425,pYL365,common.legend = TRUE,ncol = 3,legend="left")

#boxplot
df_H5<-data.frame("seasonality"=heatmap_df_N$seasonality,"productivity"=heatmap_df_N$productivity,
                  "YL637"=heatmap_df_YL637$r_length,"YL425"=heatmap_df_YL425$r_length,"YL365"=heatmap_df_N$r_length)
df_H5<-melt(df_H5,measure.vars = c("YL637","YL425","YL365"))


pH52<-ggplot(df_H5,aes(x=variable,y=value))+
  geom_violin(draw_quantiles = 0.5)+
  annotate("text",x=c(1,2,3),y=0.1+aggregate(df_H5$value,list(df_H5$variable),FUN=max, na.rm=TRUE)$x,label=MeanSd(df_H5))+
  stat_summary(fun="mean")+
  scale_x_discrete(name="Interbirth Interval (Reproductive cycle length)", labels=c("IBI = 1 YL","IBI = 1.5 YL","IBI = 1.7 YL")  )+
  scale_y_continuous(name="Birth seasonality (r)",limits = c(0,1.06))+
  theme(axis.title=element_text(size=12))

######################################
#mortality

#no extrinsic mortality M0 
load(file="Heatmaps/heatmap_df_M0.RData")
heatmap_df_M0<-heatmap_df
pM0<-plot_heatmap(heatmap_df_M0,"M = 0%")
pM0_lambda<-plot_heatmap_lambda(heatmap_df_M0,"M = 0%")

#normal conditions M1
pM1<-plot_heatmap(heatmap_df_N,"M = 11.61%")

#high extrinsic mortality M4 H5
load(file="Heatmaps/heatmap_df_M4.RData")
heatmap_df_M4<-heatmap_df
pM4<-plot_heatmap(heatmap_df_M4,"M = 46.44%")
pM4_lambda<-plot_heatmap_lambda(heatmap_df_M4,"M = 46.44%")

#all together
pH61<-ggarrange(pM0,pM1,pM4,common.legend = TRUE,ncol = 3,legend="left")


#boxplot
df_H6<-data.frame("seasonality"=heatmap_df_N$seasonality,"productivity"=heatmap_df_N$productivity,
                  "M0"=heatmap_df_M0$r_length,"M1"=heatmap_df_N$r_length,"M4"=heatmap_df_M4$r_length)
df_H6<-melt(df_H6,measure.vars = c("M0","M1","M4"))


pH62<-ggplot(df_H6,aes(x=variable,y=value))+
  geom_violin(draw_quantiles = 0.5)+ 
  annotate("text",x=c(1,2,3),y=0.1+aggregate(df_H6$value,list(df_H6$variable),FUN=max, na.rm=TRUE)$x,label=MeanSd(df_H6))+
  stat_summary(fun="mean")+
  scale_x_discrete(name="Infant mortality", labels =c("0%", "11.61%", "46.44%"))+ 
  scale_y_continuous(name="Birth seasonality (r)",limits = c(0,1.06))+
  theme(axis.title=element_text(size=12))



############################

pFig6<-ggarrange(pH52,pH62,ncol=1,heights = c(1,1),labels = c("A","B"))

pFigS7<-ggarrange(pYL637,pYL425,pYL365,pM0,pM1,pM4,common.legend = TRUE,ncol = 3,nrow=2,legend="left",labels = c("A",NA,NA,"B",NA,NA))

##########################
#### Heatmap lambda #####
#########################

pFigS6<-ggarrange(pH12a_lambda,pU0_lambda,pU2_lambda,pU3_lambda,
                  pGR15_lambda,pYL637_lambda,pYL425_lambda,
                  pM0_lambda,pM4_lambda,common.legend = TRUE,ncol = 3,nrow=3)


##########################
#### all plots #####
#########################

#Figure 2
pFig2

#Figure 4
pFig4

#Figure 5
pFig5

#Figure 6
pFig6

#Figure S6
pFigS6

#Figure S7
pFigS7


