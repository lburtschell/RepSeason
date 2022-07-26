#ifndef JUVENILE_H_INCLUDED
#define JUVENILE_H_INCLUDED
#include "Infant.h"


class Juvenile : public Infant
{
public:
    //Constructors:
    Juvenile()= default;
    Juvenile(Infant infant);
    
    //Accessors:
    int getWeaningTime() const;
    int getSexualMaturityTime() const;
    double getEnergyIntake()const;
    double getEnergyNeeds() const;
    double getEnergyBalance() const;
    std::vector <double> getRecordOfEnergyNeeds();
    std::vector <double> getRecordOfEnergyIntake();
    
    //State descriptors:
    bool isGrownUp() const;
    
    //Actions:
    void forage (double NDVI);
    void grow();
    void slowGrowth();
    void loseFat(double mass);
    void loseLeanMass(double mass);
    void releaseEnergyFromFatMass ();
    void releaseEnergyFromLeanMass ();
    void reachSexualMaturity();
    
    //Other:
    void updateEnergyNeeds();
    void updateEnergyBalance();
    void recordEnergy();
    
    
protected:
    //Timings:
    int m_weaningTime;
    int m_sexualMaturityTime;
    
    //Energy:
    double m_energyIntake;
    double m_energyNeeds;
    double m_energyReleased;
    double m_energyBalance;
    std::vector<double> m_recordOfEnergyNeeds;
    std::vector<double> m_recordOfEnergyIntake;
    
};

#endif // JUVENILE_H_INCLUDED
