#include "Infant.h"


//Constructors:

Infant::Infant(Foetus foetus):Foetus(foetus),m_leanMass(m_totalMass*m_param->m_leanProportion),m_recordOfLeanMass(),
m_fatMass(m_totalMass*(1-m_param->m_leanProportion)),m_recordOfFatMass(),m_growthAllocation(1),m_birthTime(m_param->t)
{
    double randomNumber= (double) rand()/RAND_MAX;
    if(randomNumber<(m_param->m_infantProportionOfExternalDeaths)){ //chance to die before weaning from external causes
        m_lifespan=m_param->m_infantLifespans[rand() % m_param->m_infantLifespans.size()];
    }
    else{
        m_lifespan=m_param->m_longevity;
    }
}


//Accessors:

double Infant::getFatMass() const
{
    return m_fatMass;
}

double Infant::getLeanMass() const
{
    return m_leanMass;
}

int Infant::getAge() const
{
    return (m_param->t-m_birthTime);
}

int Infant::getBirthTime() const
{
    return m_birthTime;
}

double Infant::getGrowthAllocation()const
{
    return m_growthAllocation;
}

double Infant::getDependence() const
{
    double independance = (m_totalMass-m_param->m_massAtBeginningOfWeaning)/(m_param->m_weaningMass-m_param->m_massAtBeginningOfWeaning);
    double dependence;
    if (m_totalMass<=m_param->m_massAtBeginningOfWeaning){
        dependence=1;
    }
    else{
        dependence=1-independance;
    }
    return dependence;
}

std::vector <double> Infant::getRecordOfLeanMass() const
{
    return m_recordOfLeanMass;
}

std::vector <double> Infant::getRecordOfFatMass() const
{
    return m_recordOfFatMass;
}


//State descriptors:

bool Infant::isDying() const
{
    return (Infant::getAge()>=m_lifespan);
}


//Actions:

void Infant::grow()
{
    m_leanMass+=m_growthAllocation*m_param->m_growthRate*(m_param->m_leanProportion);
    m_fatMass+=m_growthAllocation*m_param->m_growthRate*(1-m_param->m_leanProportion);
    m_totalMass=m_leanMass+m_fatMass;
    m_growthAllocation=1;
}

void Infant::storeEnergy(double energyToStore)
{
    double massToStore(m_param->m_storageEfficiency*energyToStore/m_param->m_energyInFatMass);
    if(massToStore>m_param->m_maximalDailyFatStorage){
        massToStore=m_param->m_maximalDailyFatStorage;
    }
    m_fatMass+=massToStore;
    m_totalMass=m_fatMass+m_leanMass;
}

void Infant::modifyGrowthAllocation(double newGrowthAllocation)
{
    m_growthAllocation=newGrowthAllocation;
}


//Other:

void Infant::recordMass()
{
    m_recordOfFatMass.push_back(m_fatMass);
    m_recordOfLeanMass.push_back(m_leanMass);
}

