library(RepSeason)
library(popbio)

#Change working directory to folder with your files. This command works if you use RStudio, if you use R directly you will need to use setwd with the file location.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#number of simulations :
nruns<-2000

#PHENOLOGY STRATEGY
beginningOfReproductiveWindow<-1
lengthOfReproductiveWindow<-365

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

MeanFP<-MeanTM<-Age_Death<-LRS<-Lambda<-vector()
TotalOffspring<-data.frame()
  
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
    
    
    #gather all offspring information
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
    
    if(nrow(Off_df)>0){
      Off_df[Off_df==-999]<-NA
      
      #IBI, PPA, gestation and cycling durations
      Off_df$gestationDuration<-Off_df$birthTimes-Off_df$conceptionTimes
      Off_df$cyclingDuration<-Off_df$conceptionTimes-Off_df$cycleTimes
      Off_df$ppaDuration<-Off_df$weaningTimes-Off_df$birthTimes
      Off_df$ageAtDeath<-Off_df$deathTimes-Off_df$birthTimes #for infants and juveniles
      Off_df$ageAtDeath[is.na(Off_df$birthTimes)]<-Off_df$deathTimes[is.na(Off_df$birthTimes)]-Off_df$conceptionTimes[is.na(Off_df$birthTimes)]#for foetus
      Off_df$IBI<-NA
      for(i in 2:nrow(Off_df)){
        Off_df$IBI[i-1]<-Off_df$birthTimes[i]-Off_df$birthTimes[i-1]
      }
      Off_df$IBI[Off_df$ageAtDeath<365]<-NA #IBI only if first infant survived until 1 year
      
      #Seasonality (day of year) of conception, birth and death
      Off_df$conceptionDay<-(Off_df$conceptionTimes+beginningOfSimulation)%%365
      Off_df$conceptionDay[Off_df$conceptionDay==0]<-365
      
      Off_df$birthDay<-(Off_df$birthTimes+beginningOfSimulation)%%365
      Off_df$birthDay[Off_df$birthDay==0]<-365
      
      Off_df$deathDay<-(Off_df$deathTimes+beginningOfSimulation)%%365
      Off_df$deathDay[Off_df$deathDay==0]<-365
      
      
      #Cause of Death
      Off_df$CauseOfDeath<-NA #not dead
      Off_df$CauseOfDeath[!is.na(Off_df$deathTime)]<-"Drought" #dead from lack of energy
      motherDeath<-(Off_df$deathTimes-r$FemaleDeathTime)==0&is.na(Off_df$weaningTimes) #death same day as mother 
      Off_df$CauseOfDeath[motherDeath]<-"Mother's death" #dead because mothers dead (Foetus and Infants)
      externalDeath<-Off_df$ageAtDeath==Off_df$lifespans # death from external causes
      Off_df$CauseOfDeath[externalDeath]<-"External death"
      
      #birth year (to compute fitness lambda ind)
      Off_df$birthYear<-(Off_df$birthTimes+ageAtSexualMaturity)%/%365 #year since the birth of female
      
      
      #Add offsprings from current simulation to the dataframe with all offsprings
      if(nrow(Off_df)!=0){
        Off_df$idSimu<-idSimu
        TotalOffspring<-rbind(TotalOffspring,Off_df)
      }
    }
    
    
    
    #Female life history traits
    #mean fat percent
    MeanFP<-c(MeanFP,mean(r$RecordOfFatMass/(r$RecordOfLeanMass+r$RecordOfFatMass))*100)
    
    #mean total mass
    MeanTM<-c(MeanTM,mean(r$RecordOfLeanMass+r$RecordOfFatMass))
    
    #mean age at death
    Age_Death<-c(Age_Death,r$FemaleDeathTime+ageAtSexualMaturity)
    
    #LRS (all infant born)
    LRS<-c(LRS,nrow(I_df))
    
    #lambda ind 
    yearOfDeath<-(r$FemaleDeathTime+ageAtSexualMaturity)%/%365+1
    numberOfOffspringByYear<-rep(0,yearOfDeath)
    for (i in Off_df$birthYear){
      numberOfOffspringByYear[i]<- numberOfOffspringByYear[i]+1/2 #add 1/2 for each birth
    }
    
    A<-matrix(0,yearOfDeath,yearOfDeath)
    A[col(A)==row(A)-1]<-1
    A[row(A)==1]<-numberOfOffspringByYear
    
    Lambda<-c(Lambda,lambda(A))
  }
  
  
  #Losses
  nFoetus<-nrow(TotalOffspring)
  nInfant<-sum(!is.na(TotalOffspring$birthTimes))
  nJuvenile<-sum(!is.na(TotalOffspring$weaningTimes))
  nMatSex<-sum(!is.na(TotalOffspring$sexualMaturityTimes))
  
  FoetusLoss<-1-nInfant/nFoetus
  InfantLoss<-1-nJuvenile/nInfant
  JuvenileLoss<-1-nMatSex/nJuvenile
  TotalLoss<-1-nMatSex/nInfant
  
  
  df_result<-data.frame("lambda"=mean(Lambda),"LRS"=mean(LRS),"Death"=mean(Age_Death)/365,
                        "Fat"=mean(MeanFP),"Mass"=mean(MeanTM),
                        "FoetusLoss"=FoetusLoss,"InfantLoss"=InfantLoss,"JuvenileLoss"=JuvenileLoss,"TotalLoss"=TotalLoss,
                        "IBI"=mean(TotalOffspring$IBI,na.rm=T),"Cycling"=mean(TotalOffspring$cyclingDuration,na.rm=T),
                        "PPA"=mean(TotalOffspring$ppaDuration,na.rm=T),"Preg"=mean(TotalOffspring$gestationDuration,na.rm=T))

  
  df_result_sd<-data.frame("lambda"=sd(Lambda),"LRS"=sd(LRS),"Death"=sd(Age_Death)/365,
                           "Fat"=sd(MeanFP),"Mass"=sd(MeanTM),
                           "IBI"=sd(TotalOffspring$IBI,na.rm=T),"Cycling"=sd(TotalOffspring$cyclingDuration,na.rm=T),
                           "PPA"=sd(TotalOffspring$ppaDuration,na.rm=T),"Preg"=sd(TotalOffspring$gestationDuration,na.rm=T))



#Print life history trait tables
df_result_sd
df_result
