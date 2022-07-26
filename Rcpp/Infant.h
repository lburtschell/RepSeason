#ifndef INFANT_H_INCLUDED
#define INFANT_H_INCLUDED
#include "Foetus.h"

class Infant : public Foetus
{
public:
    
    //Constructors:
    Infant() = default;
    Infant(Foetus foetus);
    
    //Accessors:
    double getFatMass() const;
    double getLeanMass() const;
    int getAge() const;
    int getBirthTime() const;
    double getGrowthAllocation() const;
    double getDependence() const;
    std::vector <double> getRecordOfLeanMass() const;
    std::vector <double> getRecordOfFatMass() const;
    
    //State descriptors:
    bool isDying() const;
    
    //Actions:
    void grow();
    void storeEnergy(double energyToStore);
    void modifyGrowthAllocation(double newGrowth);
    
    //Other:
    void recordMass();
    
    
protected:
    
    //Mass:
    double m_leanMass;
    std::vector <double> m_recordOfLeanMass;
    double m_fatMass;
    std::vector <double> m_recordOfFatMass;
    
    //State:
    double m_growthAllocation;// True if normal growth, 0 if no growth, below 1 if reduced growth
    
    //Timings:
    int m_birthTime;
};


#endif // INFANT_H_INCLUDED
