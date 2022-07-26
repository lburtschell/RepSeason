#include "Parameter.h"

// Constructor

Parameter::Parameter(double adultMass,
                         int ageAtBeginningOfWeaning,
                         int ageAtSexualMaturity,
                         double allometricCoefficient,
                         double allometricExponent,
                         int beginningOfReproductiveWindow,
                         int beginningOfSimulation,
                         double birthMass,
                         int bodyFatIncreaseDuration,
                         double characteristicValueOfAge,
                         double characteristicValueOfNDVI,
                         Rcpp::NumericVector cyclingDurations,
                         double energyInFatMass,
                         double energyInLeanMass,
                         Rcpp::NumericVector femaleLifespans,
                         double foetusProportionOfExternalMiscarriage,
                         int gestationLength,
                         double growthRate,
                         Rcpp::NumericVector infantLifespans,
                         double infantProportionOfExternalDeaths,
                         Rcpp::NumericVector juvenileLifespans,
                         double juvenileProportionOfExternalDeaths,
                         double lactationEfficiency,
                         double lactationMaxCoefficient,
                         double lactationStartCoefficient,
                         double leanProportion,
                         int lengthOfReproductiveWindow,
                         int longevity,
                         double massAtBeginningOfWeaning,
                         double massAtSexualMaturity,
                         double maximalDailyFatStorage,
                         double maximalEnergyIntake,
                         double maximalReductionOfLeanMass,
                         double placentalProportion,
                         double storageEfficiency,
                         double weaningMass,
                         int yearLength):
                     m_adultMass(adultMass),
                     m_ageAtBeginningOfWeaning(ageAtBeginningOfWeaning),
                     m_allometricCoefficient(allometricCoefficient),
                     m_allometricExponent(allometricExponent),
                     m_ageAtSexualMaturity(ageAtSexualMaturity),
                     m_beginningOfReproductiveWindow(beginningOfReproductiveWindow),
                     m_beginningOfSimulation(beginningOfSimulation),
                     m_birthMass(birthMass),
                     m_bodyFatIncreaseDuration(bodyFatIncreaseDuration),
                     m_cyclingDurations(cyclingDurations),
                     m_characteristicValueOfAge(characteristicValueOfAge),
                     m_characteristicValueOfNDVI(characteristicValueOfNDVI),
                     m_energyInFatMass(energyInFatMass),
                     m_energyInLeanMass(energyInLeanMass),
                     m_femaleLifespans(femaleLifespans),
                     m_foetusProportionOfExternalMiscarriage(foetusProportionOfExternalMiscarriage),
                     m_gestationLength(gestationLength),
                     m_growthRate(growthRate),
                     m_infantLifespans(infantLifespans),
                     m_infantProportionOfExternalDeaths(infantProportionOfExternalDeaths),
                     m_juvenileLifespans(juvenileLifespans),
                     m_juvenileProportionOfExternalDeaths(juvenileProportionOfExternalDeaths),
                     m_lactationEfficiency(lactationEfficiency),
                     m_lactationMaxCoefficient(lactationMaxCoefficient),
                     m_lactationStartCoefficient(lactationStartCoefficient),
                     m_leanProportion(leanProportion),
                     m_lengthOfReproductiveWindow(lengthOfReproductiveWindow),
                     m_longevity(longevity),
                     m_massAtBeginningOfWeaning(massAtBeginningOfWeaning),
                     m_massAtSexualMaturity(massAtSexualMaturity),
                     m_maximalDailyFatStorage(maximalDailyFatStorage),
                     m_maximalEnergyIntake(maximalEnergyIntake),
                     m_maximalReductionOfLeanMass(maximalReductionOfLeanMass),
                     m_offspringId(0),
                     m_placentalProportion(placentalProportion),
                     m_storageEfficiency(storageEfficiency),
                     m_weaningMass(weaningMass),
                     m_yearLength(yearLength),
                     t(-1)
{
if (lengthOfReproductiveWindow>=yearLength){
  m_endOfReproductiveWindow=yearLength*2; //never reached: non seasonal
  }
else{
  int endOfReproductiveWindow=(beginningOfReproductiveWindow+lengthOfReproductiveWindow)%yearLength;
  if (endOfReproductiveWindow==0){
    endOfReproductiveWindow=yearLength;
  }
  m_endOfReproductiveWindow=endOfReproductiveWindow;
  }
}

int Parameter::getDayOfYear(int time)
{
    int dayOfYear((time+m_beginningOfSimulation)%m_yearLength); //when time=0 (first day of simulation), dayOfYear(time)=beginningOfSimulation
    if (dayOfYear==0)
    {
        dayOfYear=m_yearLength;
    }
    return dayOfYear;
}

void Parameter::modifyT(int time){
  t=time;
  }
void Parameter::incrementT(){
  t++;
}
void Parameter::incrementOffspringId(){
  m_offspringId++;
  }