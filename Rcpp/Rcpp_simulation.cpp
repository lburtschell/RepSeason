#include "Female.h"
#include <algorithm>
#include <memory>


using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List Rcpp_simulation(double adultMass,
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
                     NumericVector cyclingDurations,
                     double energyInFatMass,
                     double energyInLeanMass,
                     NumericVector femaleLifespans,
                     double foetusProportionOfExternalMiscarriage,
                     int gestationLength,
                     double growthRate,
                     int idSimu,
                     NumericVector infantLifespans,
                     double infantProportionOfExternalDeaths,
                     NumericVector juvenileLifespans,
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
                     NumericVector NDVI,
                     double placentalProportion,
                     double storageEfficiency,
                     double weaningMass,
                     int yearLength)
{
    std::shared_ptr<Parameter> param = std::make_shared<Parameter>(adultMass, // the parameters are stored in a Parameter object to be shared with all indvididuals
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
                                                                   placentalProportion,
                                                                   storageEfficiency,
                                                                   weaningMass,
                                                                   yearLength);
    
    
    
    Female female(param); //Creation of the female
    
    while(female.isAlive()){
        
        param->t++;  //A new day begins (t=0 the first day)
        
        //Who is dying today (lifespan reached)?
        
        if (female.isDying()){
            female.die();
        }
        
        if(female.getReproductiveStatus()=="gestation"){
            if(female.isMisscarrying()){
                female.miscarry();
            }
        }
        
        if (female.getReproductiveStatus()=="lactation"){
            if(female.isLosingInfant()){
                female.loseInfant();
            }
        }
        
        if (female.isAlive()){ //If the female is dead, no need to continue...
            
            // Are we in the reproductive window today ?
            
            if (param->t==0){ // During the first loop of the simulation, t can be in the middle of the reproductive window
                female.checkReproductiveWindow();
            }
            
            else{ // Then, the female enters the window at every beginning of the reproductive window and exits it at the end
                if(param->getDayOfYear(param->t)==param->m_beginningOfReproductiveWindow){
                    female.enterReproductiveWindow();
                }
                if(param->getDayOfYear(param->t)==param->m_endOfReproductiveWindow){
                    female.exitReproductiveWindow();
                }
            }
            
            // Any change of reproductive status today ?
            
            if(female.getReproductiveStatus()=="anoestrus"){ // can the female start cycling ?
                
                if (female.isInReproductiveWindow()){
                    if (female.isReadyToCycle()){ // condition-dependent
                        
                        female.startCycling();
                    }
                }
            }
            
            else if (female.getReproductiveStatus()=="oestrus"){ // can the female conceive ?
                
                // If the reproductive Window has ended, the female goes to anoestrus 
                if ((!female.isInReproductiveWindow())){
                    female.stopCycling();
                }
                else if (female.isReadyToConceive()){//depends on cycling duration
                    female.conceive();
                }
            }
            
            
            else if (female.getReproductiveStatus()=="gestation"){ // can the female give birth ?
                
                if (female.isReadyToGiveBirth()){
                    female.giveBirth();
                }
            }
            
            else if (female.getReproductiveStatus()=="lactation"){ // can the infant be weaned ?
                
                if (female.isReadyToWean()){
                    female.wean();
                }
            }
            
            // What are the female energy needs today ?
            
            female.updateEnergyNeeds();
            
            // What is the female energy intake today ?
            
            female.forage(NDVI[param->t+param->m_beginningOfSimulation]);
            
            // What is her energy balance ? (energy available - energy needs)
            female.updateEnergyBalance();
            
            // Can the female store some energy today?
            
            if (female.getEnergyBalance()>=0){
                female.storeEnergy(female.getEnergyBalance());
            }
            
            else{// Some energy is missing, the female has to make sacrifices
                
                //1. Reduction of energy needs by reducing growth of female or infant if possible
                
                female.slowGrowth();
                
                if (female.getEnergyBalance()<0){
                    
                    //2. Energy missing is taken from energy stored in fat mass 
                    female.releaseEnergyFromFatMass();
                    
                    if (female.getEnergyBalance()<0){
                        
                        //3. Reduction of energy needs by stopping reproduction
                        //3.1 Miscarry
                        if (female.getReproductiveStatus()=="gestation"){
                            female.miscarry();
                        }
                        //3.2 Lose infant
                        else if (female.getReproductiveStatus()=="lactation"){
                            female.loseInfant();
                        }
                        
                        //4. Last possibility : release energy from lean mass
                        if (female.getEnergyBalance()<0){
                            female.releaseEnergyFromLeanMass(); //If too much lean mass is taken, the female dies
                        }
                    }
                }
            }
            
            // Growth of the female and her potential foetus/infant
            female.grow();
            
            //Recording some data
            
            female.recordMass();
            female.recordEnergy();
            
            female.recordReproductiveStatus();
            female.updateNumberOfDaysFattening();
        }
    }
    
    //Once the female is dead (object female still exists), juveniles can live independently and maybe reach sexual maturity
    
    if (female.getNumberOfJuveniles()>0){
        female.offspringIndependentGrowth(NDVI);
    }
    
    //For results
    return List::create(Named("FemaleDeathTime")=female.getDeathTime(),
                        Named("RecordOfFatMass")=female.getRecordOfFatMass(),
                        Named("RecordOfLeanMass")=female.getRecordOfLeanMass(),
                        Named ("FoetusHistoric")=female.getFoetusHistoric(),
                        Named ("InfantHistoric")=female.getInfantHistoric(),
                        Named ("JuvenileHistoric")=female.getJuvenileHistoric(),
                        Named("EnergyIntake")=female.getRecordOfEnergyIntake(),
                        Named("EnergyNeeds")=female.getRecordOfEnergyNeeds(),
                        Named("Status")=female.getRecordOfReproductiveStatus());
}

 