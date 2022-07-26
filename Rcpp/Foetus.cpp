#include "Foetus.h"


using namespace std;

//Constructors:

Foetus::Foetus(const std::shared_ptr<Parameter> param,int cycleTime): m_id(param->m_offspringId+1),m_totalMass(0),
m_alive(true),m_cycleTime(cycleTime),m_conceptionTime(param->t),m_deathTime(-999),m_param(param)
{  
    double randomNumber= (double) rand()/RAND_MAX;
    if(randomNumber<(m_param->m_foetusProportionOfExternalMiscarriage)){ //chance to die before weaning from external causes
        m_lifespan=rand() % m_param->m_gestationLength;
    }
    else{
        m_lifespan=m_param->m_gestationLength+1;
    }
    
    m_param->incrementOffspringId();
}


//Accessors:

int Foetus::getId()const
{
    return m_id;
}

double Foetus::getTotalMass() const
{
    return m_totalMass;
}

int Foetus::getFoetusAge() const
{
    return (m_param->t-m_conceptionTime);
    
}

int Foetus::getCycleTime() const
{
    return m_cycleTime;
}

int Foetus::getConceptionTime() const
{
    return m_conceptionTime;
}

int Foetus::getDeathTime() const
{
    return m_deathTime;
}    

int Foetus::getLifespan() const
{
    return m_lifespan;
}

std::vector<double> Foetus::getRecordOfTotalMass() const
{
    return m_recordOfTotalMass;
}


//State descriptors:

bool Foetus::isAlive() const
{
    return m_alive;
}

bool Foetus::isDying() const
{
    return (getFoetusAge()>=m_lifespan);
}


//Actions:

void Foetus::grow()
{
    m_totalMass+=m_param->m_growthRate/(1+m_param->m_placentalProportion); //doesn't include placenta
}

void Foetus::die()
{
    m_alive=false;
    m_deathTime=m_param->t;
}


//Other:

void Foetus::recordMass()
{
    m_recordOfTotalMass.push_back(m_totalMass);
}



