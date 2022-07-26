#ifndef FOETUS_H_INCLUDED
#define FOETUS_H_INCLUDED
#include "Parameter.h"
#include <memory> // use of shared_ptr

class Foetus
{
public:
    //Constructors:
    Foetus() = default;
    Foetus(const std::shared_ptr<Parameter> param,int cycleTime);
    
    //Accessors:
    int getId()const;
    double getTotalMass() const;
    int getFoetusAge() const;
    int getCycleTime() const;
    int getConceptionTime() const;
    int getDeathTime()const;
    int getLifespan() const;
    std::vector<double> getRecordOfTotalMass() const;
    
    //State descriptors:
    bool isAlive() const;
    bool isDying() const;
    
    //Actions:
    void grow();
    void die();
    
    //Other:
    void recordMass();
    
    
protected:
    //Id:
    int m_id;
    
    //Mass:
    double m_totalMass; 
    std::vector<double> m_recordOfTotalMass;
    
    //State:
    bool m_alive;
    
    //Timings:
    int m_cycleTime;
    int m_conceptionTime;
    int m_deathTime;
    int m_lifespan;
    
    //Parameters:
    std::shared_ptr<Parameter> m_param;
};

#endif // FOETUS_H_INCLUDED
