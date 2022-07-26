#ifndef FEMALE_H_INCLUDED
#define FEMALE_H_INCLUDED
#include "Juvenile.h"


class Female : public Juvenile
{
public:
    //Constructor:
    Female() = default;
    Female(const std::shared_ptr<Parameter> param);
    
    //Accessors
    std::string getReproductiveStatus() const;
    std::vector <std::string> getRecordOfReproductiveStatus() const;
    int getNumberOfJuveniles() const;
    
    //State descriptors:
    bool isInReproductiveWindow() const;
    bool isReadyToCycle() const;
    bool isReadyToConceive() const;
    bool isReadyToGiveBirth() const;
    bool isReadyToWean() const;
    bool isMisscarrying() const;
    bool isLosingInfant() const;
    
    //Actions:
    void die();
    void forage(double NDVI);
    void grow();
    void slowGrowth();
    void storeEnergy(double energyToStore);
    void checkReproductiveWindow();
    void enterReproductiveWindow();
    void exitReproductiveWindow();
    void startCycling();
    void stopCycling();
    void conceive();
    void giveBirth();
    void wean();
    void miscarry();
    void loseInfant();
    void offspringIndependentGrowth(Rcpp::NumericVector NDVI);
    
    
    //Others:
    void updateEnergyNeeds();
    void updateNumberOfDaysFattening();
    void recordMass();
    void recordReproductiveStatus();
    Rcpp::List getJuvenileHistoric();
    Rcpp::List getInfantHistoric();
    Rcpp::List getFoetusHistoric();
    
protected:
    //Reproduction:
    int m_beginningOfReproductiveWindow;
    int m_lengthOfReproductiveWindow;
    bool m_reproductiveWindow;
    std::vector <Foetus> m_foetus;
    std::vector <Infant> m_infant;
    std::vector <Juvenile> m_juvenile;
    std::string m_reproductiveStatus; // "oestrus", "gestation", "lactation", "anoestrus"
    double m_cyclingDuration;
    int m_lastBeginningOfCycle;
    int m_numberOfDaysFattening;
    
    // Model outputs
    std::vector <std::string> m_recordOfReproductiveStatus;

};



#endif // FEMALE_H_INCLUDED
