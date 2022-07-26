#include "Juvenile.h"

using namespace std;

//Constructors:

Juvenile::Juvenile(Infant infant):Infant(infant),m_weaningTime(m_param->t),m_sexualMaturityTime(-999),m_energyNeeds(0), m_energyIntake(0),m_energyReleased(0), m_energyBalance(0)
{
    double randomNumber= (double) rand()/RAND_MAX;
    if(randomNumber<(m_param->m_juvenileProportionOfExternalDeaths)){ //chance to die before SexualMaturity from external causes
        m_lifespan=m_param->m_juvenileLifespans[rand() % m_param->m_juvenileLifespans.size()];
    }
    else{
        m_lifespan=m_param->m_longevity;
    }
}


//Accessors:

int Juvenile::getWeaningTime() const
{
    return m_weaningTime;
}

int Juvenile::getSexualMaturityTime() const
{
    return m_sexualMaturityTime;
}

double Juvenile::getEnergyIntake() const
{
    return m_energyIntake;
}

double Juvenile::getEnergyNeeds() const
{
    return m_energyNeeds;
}

double Juvenile::getEnergyBalance() const
{
    return m_energyBalance;
}


//State descriptors:

bool Juvenile::isGrownUp() const
{
    return m_leanMass>=m_param->m_adultMass*m_param->m_leanProportion;
}


//Actions:

void Juvenile::forage(double NDVI)
{
     double energyIntake (m_param->m_maximalEnergyIntake*(1-exp(-(float)(NDVI/m_param->m_characteristicValueOfNDVI))));
   energyIntake*=(1-exp(-(float)(Infant::getAge()/m_param->m_characteristicValueOfAge)));
   energyIntake*=pow(m_totalMass/m_param->m_adultMass,m_param->m_allometricExponent);
    m_energyIntake=energyIntake;
}

void Juvenile::grow()
{
    Infant::grow();
    m_energyReleased=0;
}

void Juvenile::slowGrowth()
{  if(!isGrownUp()){
    double energyMissing=-getEnergyBalance();
    double newGrowthAllocation=1-(energyMissing/(m_param->m_energyInLeanMass*m_param->m_growthRate));// if possible growth is not stoped (0) but slowed
    if(newGrowthAllocation<0){
        Infant::modifyGrowthAllocation(0);
        }
    else{ Infant::modifyGrowthAllocation(newGrowthAllocation);
    }
    updateEnergyNeeds(); //Energy needs has decreased
    updateEnergyBalance(); //Energy balance has increased
}
}

void Juvenile::loseFat(double mass)
{    m_fatMass-=mass;
    m_totalMass=m_fatMass+m_leanMass;
}

void Juvenile::loseLeanMass(double mass)
{   
   m_leanMass-=mass;
     m_totalMass=m_fatMass+m_leanMass;
    }    

void Juvenile::releaseEnergyFromFatMass()
{
    double energyNeeded=-m_energyBalance;
    
    double energyInFatMass=m_fatMass*m_param->m_energyInFatMass;
    
    if (energyInFatMass<=energyNeeded)//all of energy available is taken
    {m_energyReleased=energyInFatMass;
    }
    
    else //just the energy needed is taken
    {
        m_energyReleased=energyNeeded;
    }
    double massToRelease=m_energyReleased/m_param->m_energyInFatMass;
    loseFat(massToRelease);
    
    updateEnergyBalance(); //Energy balance has increased
}

void Juvenile ::releaseEnergyFromLeanMass (){
    
    double energyNeeded=-m_energyBalance;
    double minLeanMass= *max_element(m_recordOfLeanMass.begin(),m_recordOfLeanMass.end())*m_param->m_maximalReductionOfLeanMass;
    double energyInLeanMass=(m_leanMass-minLeanMass)*m_param->m_energyInLeanMass;  
    
    if (energyInLeanMass<energyNeeded)//all of energy stored is taken
    {
        die();
    }
    
    else //just the energy needed is taken
    {
        double massToRelease=energyNeeded/m_param->m_energyInLeanMass;
        
        loseLeanMass(massToRelease);
    }
}

void Juvenile::reachSexualMaturity()
{
    m_sexualMaturityTime=m_param->t;
    }
    
//Others:
void Juvenile::updateEnergyNeeds()
{
m_energyNeeds=m_growthAllocation*(m_param->m_energyInLeanMass*m_param->m_growthRate)+(m_param->m_allometricCoefficient*pow(m_totalMass,m_param->m_allometricExponent));

}

void Juvenile::updateEnergyBalance()
{
m_energyBalance=m_energyIntake+m_energyReleased-m_energyNeeds;    
}

void Juvenile::recordEnergy()
{
    m_recordOfEnergyIntake.push_back(m_energyIntake); 
    m_recordOfEnergyNeeds.push_back(m_energyNeeds);
}

std::vector <double> Juvenile::getRecordOfEnergyNeeds()
{
    return m_recordOfEnergyNeeds;
}

std::vector <double> Juvenile::getRecordOfEnergyIntake()
{
    return m_recordOfEnergyIntake;
}
