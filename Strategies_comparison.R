library(RepSeason)
library(popbio)
library(plotly)

#Change working directory to folder with your files. This command works if you use RStudio, if you use R directly you will need to use setwd with the file location.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("Functions/CircularFunctions.R") #create circular graphs and compute r_length

#SIMULATION
#number of simulations :
nruns<-2000
sizeEffectThreshold<-0.05 

#PARAMETERS TO TEST
#ecology
seasonality<-1
productivity<-1
unpredictability<-1
#life history
growthRateCoef<-1
mortalityCoef<-1
yearLength<-365 #days

#PHENOLOGY STRATEGY
monthLength<-yearLength/12
test_beginningOfReproductiveWindow<-round(seq(1,yearLength,by=1*monthLength)) #first day of each month
test_lengthOfReproductiveWindow<-round(seq(1*monthLength,11*monthLength,by=1*monthLength))# 1 to 11 months

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

#modified parameters :
#NDVI decomposition
K<-mean(df_NDVI$NDVI)
centeredNDVI<-df_NDVI$NDVI-K
seasonalNDVI<-vector()
for (i in 1:yearLength){
  seasonalNDVI[i]=mean(centeredNDVI[df_NDVI$DayOfYear==i])
}
S<-seasonalNDVI[df_NDVI$DayOfYear]
NS<-centeredNDVI-S #NDVI = K+S+NS
NDVI<-productivity*K+seasonality*S+unpredictability*NS
NDVI[NDVI>1]<-1
NDVI[NDVI<0]<-0

#growth rate and gestation length
growthRate<-growthRate*growthRateCoef #kg/day
gestationLength<-round(1.25*0.710/growthRate) #day

#mortality
infantProportionOfExternalDeaths<-infantProportionOfExternalDeaths*mortalityCoef

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
                          yearLength)
      
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
      strategies_fitness<-rbind(strategies_fitness,c(strategy,strategy_name,beginningOfReproductiveWindow,lengthOfReproductiveWindow,LambdaInd))
      
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
colnames(strategies_fitness)<-c("strategy","strategy_name","start","length","lambda")
strategies_fitness$strategy<-as.factor(strategies_fitness$strategy)
strategies_fitness$strategy_name<-as.factor(strategies_fitness$strategy_name)
strategies_fitness$start<-as.numeric(strategies_fitness$start)
strategies_fitness$length<-as.numeric(strategies_fitness$length)
strategies_fitness$lambda<-as.numeric(strategies_fitness$lambda)


# mean of fitness by strategy
mean_strategies_fitness<-aggregate(strategies_fitness[3:5],list(strategies_fitness$strategy),mean)
colnames(mean_strategies_fitness)<-c("strategy","start","length", "lambda")
mean_strategies_fitness$start<-as.factor(mean_strategies_fitness$start)
mean_strategies_fitness$length<-as.factor(mean_strategies_fitness$length)


#COMPARISON OF LAMBDA
#plot the comparison of these strategies (FIGURE 3)
strategy_matrix<-strategy_matrix_rv<-matrix(,nrow=12,ncol=12)
  for (i in 1:133){
    strategy_matrix[mean_strategies_fitness$start[i],mean_strategies_fitness$length[i]]<-mean_strategies_fitness$lambda[i]
  }

pFig3<-plot_ly(x=levels(mean_strategies_fitness$length), y=levels(mean_strategies_fitness$start), z=strategy_matrix, type="surface",  contours = list(
  z = list(show = TRUE, start = 0.95*max(mean_strategies_fitness$lambda), end = 10, size = 100,color='red'))) %>% 
  layout(scene = list(
    xaxis=list(title='Length (d)', autorange = "reversed"),
    yaxis=list(title='Start (d)'),
    zaxis=list(title='Fitness')))


#select the non discriminated strategies (between the size effect threshold)
ordered_strategies<-mean_strategies_fitness$strategy[order(mean_strategies_fitness$lambda, decreasing = TRUE)]
best<-ordered_strategies[1]
others<-as.numeric(setdiff(ordered_strategies,best))
non_discriminated<-as.numeric(as.character(best))
x<-subset(strategies_fitness,strategies_fitness$strategy==best)$lambda
if(mean(x)!=0){
  for (s in others){
    y<-subset(strategies_fitness,strategies_fitness$strategy==s)$lambda
    if(mean(y)>(1-sizeEffectThreshold)*mean(x)){ #the decrease in mean fitness for strategy s is less than sizeEffectThjreshold%
      non_discriminated<-c(non_discriminated,s)
    } 
  }
  
}
non_dis_char<-paste(as.character(non_discriminated),collapse=" ")

#Plot circular graph for all births from non discriminated strategies (FIGURE S5)
all_births<-subset(strategies_births,strategies_births$strategy%in%non_discriminated)$birthTimes
pFigS5<-circular_graph(all_births,"Birth seasonality",365)


##########################
#### plots #####
#########################

#Figure 3
pFig3

#Figure S5
pFigS5
