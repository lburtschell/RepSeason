library(RepSeason)
library(popbio)


#Change working directory to folder with your files. This command works if you use RStudio, if you use R directly you will need to use setwd with the file location.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


source("Functions/ExtractNDVI.R") #extract NDVI from coordinates
source("Functions/ModifyNDVI.R") #interpol and extend NDVI (possibility to modify year length)


#SIMULATION
#number of simulations :
nruns<-2000

#PARAMETERS TO TEST
#ecology
test_seasonality<-seq(0,3,by=0.5)
test_productivity<-seq(0.85,1.4,by=0.05)
unpredictability<-1
#life history
growthRateCoef<-1
mortalityCoef<-1
yearLength<-365 #days

#NDVI
#1. easy and quick : load NDVI from Amboseli
#normal year length
load(file="Input/df_NDVI.RData")
#yearLength=425 days
      #load(file="Input/df_NDVI_YL425.RData")
#yearLength=637 days
      #load(file="Input/df_NDVI_YL637.RData")

#2. if you want to load other NDVI from other coordinates
      #raw_NDVI<-extractNDVI(37.04, -2.75, 37.11, -2.70) #Amboseli coordinates
      #df_NDVI<-modifyNDVI(raw_NDVI,yearLength, 3) #possibility to change the year length and repeat the time series 3 times

#PHENOLOGY STRATEGY
monthLength<-yearLength/12
test_beginningOfReproductiveWindow<-round(seq(1,yearLength,by=monthLength)) #first day of each month
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

#raw data for lifetime and cycling duration
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


#NDVI decomposition
K<-mean(df_NDVI$NDVI)
centeredNDVI<-df_NDVI$NDVI-K
seasonalNDVI<-vector()
for (i in 1:yearLength){
  seasonalNDVI[i]=mean(centeredNDVI[df_NDVI$DayOfYear==i])
}
S<-seasonalNDVI[df_NDVI$DayOfYear]
NS<-centeredNDVI-S #NDVI = K+S+NS

#modified parameters :

#growth rate and gestation length
growthRate<-growthRate*growthRateCoef #kg/day
gestationLength<-round(1.25*0.710/growthRate) #day
#mortality
infantProportionOfExternalDeaths<-infantProportionOfExternalDeaths*mortalityCoef


##################################################################################
#Run the model
##################################################################################

heatmap_df<-data.frame()

for (seasonality in test_seasonality){
  print(seasonality)
  for (productivity in test_productivity){
    print(productivity)
    
    #NDVI modification
    NDVI<-productivity*K+seasonality*S+unpredictability*NS
    NDVI[NDVI>1]<-1
    NDVI[NDVI<0]<-0
    
    #Test of all strategies
    strategies_fitness<-data.frame()
    strategies_births<-data.frame()
    
    strategy<-0 #initiation
    
    test_lengthOfReproductiveWindow_init<-test_lengthOfReproductiveWindow
    
    for (beginningOfReproductiveWindow in test_beginningOfReproductiveWindow){
      if(beginningOfReproductiveWindow==test_beginningOfReproductiveWindow[length(test_beginningOfReproductiveWindow)]){
        test_lengthOfReproductiveWindow_init<-c(test_lengthOfReproductiveWindow_init,yearLength)
      }
      for(lengthOfReproductiveWindow in test_lengthOfReproductiveWindow_init){
        strategy<-strategy+1
        if(lengthOfReproductiveWindow!=yearLength){
          strategy_name<-paste("Start: ",beginningOfReproductiveWindow,"\n","Length: ",lengthOfReproductiveWindow,sep="")
        }
        else {
          strategy_name<-"Non\nseasonal"
        }
        for (idSimu in 1:nruns){
          beginningOfSimulation<-sample(1:(22*yearLength),1)
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
    
    #select the non discriminated strategies
    ordered_strategies<-mean_strategies_fitness$strategy[order(mean_strategies_fitness$lambda, decreasing = TRUE)]
    best<-ordered_strategies[1]
    others<-as.numeric(setdiff(ordered_strategies,best))
    non_discriminated<-as.numeric(as.character(best))
    x<-subset(strategies_fitness,strategies_fitness$strategy==best)$lambda
    if(mean(x)!=0){
      for (s in others){
        
        y<-subset(strategies_fitness,strategies_fitness$strategy==s)$lambda
        if(t.test(x,y)$p.value>0.05){ #the two samples are not significantly different
          non_discriminated<-c(non_discriminated,s)
        } 
      }
      
    }
    non_dis_char<-paste(as.character(non_discriminated),collapse=" ")
    
    #is the non-seasonal strategy in the non discriminated strategy ?
    NS_select<-strategy%in%non_discriminated
    
    
    #All birth pooled
    all_births<-subset(strategies_births,strategies_births$strategy%in%non_discriminated)$birthTimes
    all_lambda<-subset(strategies_fitness,strategies_fitness$strategy%in%non_discriminated)$lambda

    #R and lambda from each of the non discriminated strategy
    r_non_discriminated<-vector()
    lambda_non_discriminated<-vector()
    for (s in non_discriminated){
      births_s<-subset(strategies_births,strategies_births$strategy==s)$birthTimes # all births from strategy s
      lambda_s<-subset(strategies_fitness,strategies_fitness$strategy==s)$lambda #all lambda from strategy s
      r_non_discriminated<-c(r_non_discriminated,r_from_births(births_s,yearLength)$r_length)
      lambda_non_discriminated<-c(lambda_non_discriminated,mean(lambda_s))
    }
    #R and lambda from each of the non seasonal strategy
    births_NS<-subset(strategies_births,strategies_births$strategy==strategy)$birthTimes 
    lambda_NS<-subset(strategies_fitness,strategies_fitness$strategy==strategy)$lambda
    NS_r<-r_from_births(births_NS,yearLength)$r_length
    NS_lambda<-mean(lambda_NS)

    heatmap_df<-rbind(heatmap_df,list(seasonality,productivity,
                                   r_from_births(all_births,yearLength)$r_length,mean(all_lambda)))
  }
}

colnames(heatmap_df)<-c("seasonality","productivity",
                        "r_length","lambda")

heatmap_df$r_length[heatmap_df$lambda==0]<-NA


