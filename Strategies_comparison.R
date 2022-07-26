library(RepSeason)
library(popbio)

#Change working directory to folder with your files. This command works if you use RStudio, if you use R directly you will need to use setwd with the file location.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("Functions/CircularFunctions.R") #create circular graphs and compute r_length

#number of simulations :
nruns<-2000

#PHENOLOGY STRATEGY
monthLength<-365/12
test_beginningOfReproductiveWindow<-round(seq(1,365,by=monthLength)) #first day of each month
test_lengthOfReproductiveWindow<-round(seq(1*monthLength,11*monthLength,by=monthLength))# 1 to 11 months

# PARAMETERS USED IN SIMULATION
#raw parameters (from literature)
adultMass<-11.9 #kg
ageAtBeginningOfWeaning<-56 #days
ageAtSexualMaturity<-1643 #days
allometricCoefficient<-578.41 #kJ kg^-0.75/day
allometricExponent<-0.75
birthMass<-0.710 #kg
bodyFatIncreaseDuration <- 1 #days
energyInFatMass<-39500 # kJ/kg
energyInLeanMass <- 20920 # kJ/kg
gestationLength<-178 #days
growthRate<- 0.005 #kg/day
infantProportionOfExternalDeaths<-0.215*0.54
juvenileProportionOfExternalDeaths<-infantProportionOfExternalDeaths
lactationEfficiency<-0.8
lactationStartCoefficient<-1.11
lactationMaxCoefficient<-1.27
leanProportion<-0.981
longevity<-9862 #days
maximalDailyFatStorage<-growthRate*(1-leanProportion) #kg/day
maximalEnergyIntake<-4891.1 #kJ/day (no repro)
maximalReductionOfLeanMass<-0.85
meanppaDuration<-322 #days
placentalProportion<-0.25 #proportion of foetus mass
storageEfficiency<-0.9

#raw data for NDVI & lifetime and cycling duration 
load(file="Input/df_NDVI.RData")
NDVI<-df_NDVI$NDVI
load(file="Input/cyclingDurations.RData")
load(file="Input/infantLifespans.RData")
load(file="Input/juvenileLifespans.RData")
load(file="Input/femaleLifespans.RData")

#calculated parameters (from raw parameters)
massAtSexualMaturity<-birthMass+ageAtSexualMaturity*0.005 #kg
weaningMass<-(birthMass+0.005*meanppaDuration) #kg
massAtBeginningOfWeaning<-(birthMass+0.005*ageAtBeginningOfWeaning) #kg

#calibrated parameters 
characteristicValueOfAge<-193
characteristicValueOfNDVI<-0.117
foetusProportionOfExternalMiscarriage<-0.07



##################################################################################
strategies_fitness<-data.frame()
strategies_births<-data.frame()

strategy<-0 #initiation


for (beginningOfReproductiveWindow in test_beginningOfReproductiveWindow){
  if(beginningOfReproductiveWindow==test_beginningOfReproductiveWindow[length(test_beginningOfReproductiveWindow)]){
    test_lengthOfReproductiveWindow<-c(test_lengthOfReproductiveWindow,365)
  }
  for(lengthOfReproductiveWindow in test_lengthOfReproductiveWindow){
    strategy<-strategy+1
    if(lengthOfReproductiveWindow!=365){
      strategy_name<-paste("Start: ",beginningOfReproductiveWindow,"\n","Length: ",lengthOfReproductiveWindow,sep="")
    }
    else {
      strategy_name<-"Non\nseasonal"
    }
    print(strategy)
    for (idSimu in 1:nruns){
      beginningOfSimulation<-sample(1:(22*365),1)
      r<- Rcpp_simulation(adultMass,
                          ageAtBeginningOfWeaning,
                          ageAtSexualMaturity,
                          allometricCoefficient,
                          allometricExponent,
                          beginningOfReproductiveWindow,
                          beginningOfSimulation,
                          birthMass,
                          bodyFatIncreaseDuration,
                          characteristicValueOfAge,
                          characteristicValueOfNDVI,
                          cyclingDurations,
                          energyInFatMass,
                          energyInLeanMass,
                          femaleLifespans,
                          foetusProportionOfExternalMiscarriage,
                          gestationLength,
                          growthRate,
                          idSimu,
                          infantLifespans,
                          infantProportionOfExternalDeaths,
                          juvenileLifespans,
                          juvenileProportionOfExternalDeaths,
                          lactationEfficiency,
                          lactationMaxCoefficient,
                          lactationStartCoefficient,
                          leanProportion,
                          lengthOfReproductiveWindow,
                          longevity,
                          massAtBeginningOfWeaning,
                          massAtSexualMaturity,
                          maximalDailyFatStorage,
                          maximalEnergyIntake,
                          maximalReductionOfLeanMass,
                          NDVI,
                          placentalProportion,
                          storageEfficiency,
                          weaningMass,
                          365)
      
      
      Off_df<-data.frame()
      F_df<-data.frame(do.call(cbind,r$FoetusHistoric))
      I_df<-data.frame(do.call(cbind,r$InfantHistoric))
      J_df<-data.frame(do.call(cbind,r$JuvenileHistoric))
      for (id in F_df$idOffspring){ #for each foetus
        if (id%in%J_df$idOffspring){
          Off_df<-rbind(Off_df,subset(J_df,J_df$idOffspring==id))
        }
        else if(id%in%I_df$idOffspring){
          Off_df<-rbind(Off_df,subset(I_df,I_df$idOffspring==id))
        }
        else{
          Off_df<-rbind(Off_df,subset(F_df,F_df$idOffspring==id))
        }
      }
      
      Off_df[Off_df==-999]<-NA
      
      #only keep juvenile that have reached sexual maturity
      Juv_df<-subset(Off_df,!is.na(Off_df$sexualMaturityTimes))
      
      #computation of lambda ind
      Juv_df$birthYear<-(Juv_df$birthTimes+ageAtSexualMaturity)%/%365 #year since the birth of female 
      yearOfDeath<-(r$FemaleDeathTime+ageAtSexualMaturity)%/%365+1
      numberOfOffspringByYear<-rep(0,yearOfDeath)
      for (i in Juv_df$birthYear){
        numberOfOffspringByYear[i]<- numberOfOffspringByYear[i]+1/2 #add 1/2 for each birth
      }
      
      A<-matrix(0,yearOfDeath,yearOfDeath)
      A[col(A)==row(A)-1]<-1
      A[row(A)==1]<-numberOfOffspringByYear
      
      LambdaInd<-lambda(A)
      
      #record of fitness
      strategies_fitness<-rbind(strategies_fitness,c(strategy,strategy_name,LambdaInd))
      
      #record of births
      n_births<-length(I_df$birthTimes)
      if(n_births>0){
        strategies_births<-rbind(strategies_births,data.frame("strategy"=rep(strategy,n_births),
                                                              "strategy_name"=rep(strategy_name,n_births),
                                                              "birthTimes"=I_df$birthTimes+beginningOfSimulation))
      }
    }
  }
}

# format strategies_fitness
colnames(strategies_fitness)<-c("strategy","strategy_name","lambda")
strategies_fitness$strategy<-as.factor(strategies_fitness$strategy)
strategies_fitness$strategy_name<-as.factor(strategies_fitness$strategy_name)
strategies_fitness$lambda<-as.numeric(strategies_fitness$lambda)

# mean of fitness by strategy
mean_strategies_fitness<-aggregate(strategies_fitness[3],list(strategies_fitness$strategy),mean)
colnames(mean_strategies_fitness)<-c("strategy", "lambda")


#COMPARISON OF LAMBDA


#select a sample of n strategies, including the best, the worst and the non seasonal strategies
n_strategies<-10
ordered_strategies<-mean_strategies_fitness$strategy[order(mean_strategies_fitness$lambda, decreasing = TRUE)]
non_seas_strat<-strategy # the non seasonal strategy is the last strategy tested in the previous loop
ordered_seasonal_strategies<-setdiff(ordered_strategies,non_seas_strat) #keep only seasonal strategies
#select a sample
sample_strategies<-ordered_seasonal_strategies[round(seq(1,length(ordered_seasonal_strategies),length.out=n_strategies-1))]
sample_strategies<-c(sample_strategies,non_seas_strat) # add the non seasonal strategy
strategies_fitness_sample<-subset(strategies_fitness,strategies_fitness$strategy%in%sample_strategies)

#list of side by side comparison with first strategy : 1,2 / 1,3 / 1,4 ... / 1,n
comp<-list()
for (s in 2:n_strategies){
  comp<-c(comp,list(c(1,s)))
}


#plot the comparison of these strategies
ggplot(strategies_fitness_sample,aes(x=reorder(strategy_name,-lambda,fun="mean"),y=lambda))+
  geom_violin()+   #
  geom_signif(comparisons = comp, map_signif_level=TRUE,test="t.test",step_increase = 0.09)+
  stat_summary(fun="mean")+
  scale_x_discrete(name="Phenology strategy")+
  scale_y_continuous(name=expression(paste("Fitness (",lambda[ind],")")),breaks=c(0,0.25,0.5,0.75,1,1.25))+
  theme(axis.title=element_text(size=12))

#select the non discriminated strategies
best<-ordered_strategies[1]
others<-as.numeric(setdiff(ordered_strategies,best))
non_discriminated<-as.numeric(as.character(best))
for (s in others){
  x<-subset(strategies_fitness,strategies_fitness$strategy==best)$lambda
  y<-subset(strategies_fitness,strategies_fitness$strategy==s)$lambda
  if(t.test(x,y)$p.value>0.05){ #the two samples are not significantly different
    non_discriminated<-c(non_discriminated,s)
  }
}

#Plot circular graph for all births from non discriminated strategies 
all_births<-subset(strategies_births,strategies_births$strategy%in%non_discriminated)$birthTimes
circular_graph(all_births,"Birth seasonality",365)
