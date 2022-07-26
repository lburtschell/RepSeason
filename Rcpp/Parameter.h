#ifndef PARAMETER_H_INCLUDED
#define PARAMETER_H_INCLUDED
#include <Rcpp.h>


class Parameter
{

public:
//Constructor
    Parameter() = default;
    Parameter(double adultMass,
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
              int yearLength);

//Parameters
    double m_adultMass;
    int m_ageAtBeginningOfWeaning;
    int m_ageAtSexualMaturity;
    double m_allometricCoefficient;
    double m_allometricExponent;
    int m_beginningOfReproductiveWindow;
    int m_beginningOfSimulation;
    double m_birthMass;
    int m_bodyFatIncreaseDuration;
    double m_characteristicValueOfAge;
    double m_characteristicValueOfNDVI;
    Rcpp::NumericVector m_cyclingDurations;
    double m_energyInFatMass;
    double m_energyInLeanMass;
    Rcpp::NumericVector m_femaleLifespans;
    double m_foetusProportionOfExternalMiscarriage;
    double m_foragingEfficiencyReduction;
    int m_gestationLength;
    double m_growthRate;
    Rcpp::NumericVector m_infantLifespans;
    double m_infantProportionOfExternalDeaths;
    Rcpp::NumericVector m_juvenileLifespans;
    double m_juvenileProportionOfExternalDeaths;
    double m_lactationEfficiency;
    double m_lactationMaxCoefficient;
    double m_lactationStartCoefficient;
    double m_leanProportion;
    int m_lengthOfReproductiveWindow;
    int m_lengthOfForagingEfficiencyReduction;
    int m_longevity;
    double m_massAtBeginningOfWeaning;
    double m_massAtSexualMaturity;
    double m_maximalDailyFatStorage;
    double m_maximalEnergyIntake;
    double m_maximalReductionOfLeanMass;
    double m_placentalProportion;
    double m_storageEfficiency;
    double m_weaningMass;
    int m_yearLength;



    //calculated variable
    int m_endOfReproductiveWindow;
    
    //shared variable
    int t;
    int m_offspringId;
    

    //shared function
    int getDayOfYear(int time);
    void modifyT(int time);
    void incrementT();
    void incrementOffspringId();
};

#endif // PARAMETER_H_INCLUDED
