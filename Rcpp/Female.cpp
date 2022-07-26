#include "Female.h"

using namespace Rcpp;
using namespace std;

//Constructors:

Female::Female(const std::shared_ptr<Parameter> param) //female initialization at sexual maturity
{ //Parameters
  m_param=param;
  //Mass
  m_totalMass=param->m_massAtSexualMaturity;
  m_leanMass=param->m_massAtSexualMaturity*param->m_leanProportion;
  m_fatMass=param->m_massAtSexualMaturity*(1-param->m_leanProportion);
  //State
  m_alive=true;
  m_growthAllocation=1;
  //Timings
  m_cycleTime=-999;
  m_conceptionTime=-999;
  m_birthTime=-param->m_ageAtSexualMaturity;
  m_sexualMaturityTime=0;
  m_weaningTime=-999;
  m_deathTime=-999;
  m_lifespan=param->m_femaleLifespans[rand() % param->m_femaleLifespans.size()];
  //Energy
  m_energyNeeds=0;
  m_energyIntake=0;
  m_energyReleased=0;
  m_energyBalance=0;
  //Reproduction
  m_beginningOfReproductiveWindow=param->m_beginningOfReproductiveWindow;
  m_lengthOfReproductiveWindow=param->m_lengthOfReproductiveWindow;
  m_reproductiveWindow=false;
  m_cyclingDuration=0;
  m_reproductiveStatus="anoestrus";
  m_cyclingDuration=0;
  m_lastBeginningOfCycle=-999;
  m_numberOfDaysFattening=0;
}

//Accessors:

std::string Female::getReproductiveStatus() const
{
  return m_reproductiveStatus;
}

std::vector <std::string> Female::getRecordOfReproductiveStatus() const
{
  return m_recordOfReproductiveStatus;
}

int Female::getNumberOfJuveniles() const
{
  return m_juvenile.size();
}

//State descriptors:

bool Female::isInReproductiveWindow() const
{
  return m_reproductiveWindow;
}

bool Female::isReadyToCycle() const
{
  return m_numberOfDaysFattening>=m_param->m_bodyFatIncreaseDuration;
}

bool Female::isReadyToConceive() const
{
  return m_param->t-m_lastBeginningOfCycle==m_cyclingDuration;
}

bool Female::isReadyToGiveBirth() const
{
  return m_param->t-m_foetus.back().getConceptionTime()>=m_param->m_gestationLength;
}

bool Female::isReadyToWean() const
{
  return m_infant.back().getTotalMass()>=m_param->m_weaningMass;
}

bool Female::isMisscarrying() const
{
  return m_foetus.back().isDying();
}

bool Female::isLosingInfant() const
{
  return m_infant.back().isDying();
}


//Actions:

void Female::die()
{
  Foetus::die();
  if (m_reproductiveStatus=="lactation"){
    m_infant.back().die();
    }
  if (m_reproductiveStatus=="gestation"){
    m_foetus.back().die();
  }
}

void Female::forage(double NDVI)
{
  Juvenile::forage(NDVI);
  double coef=1;
  
  if (m_reproductiveStatus=="gestation"){
    double pregnantMass=m_totalMass+(1+m_param->m_placentalProportion)*m_foetus.back().getTotalMass();
    coef = pow(pregnantMass/m_totalMass,m_param->m_allometricExponent); //use of pregnant mass instead of totalmass (without foetus) to calculate the increase in foraging
  }
  
  if (m_reproductiveStatus=="lactation"){
    double mass=m_infant.back().getTotalMass();
    if(mass<=m_param->m_massAtBeginningOfWeaning){
      coef=m_param->m_lactationStartCoefficient + (m_param->m_lactationMaxCoefficient-m_param->m_lactationStartCoefficient)*(mass-m_param->m_birthMass)/(m_param->m_massAtBeginningOfWeaning-m_param->m_birthMass);
    } // equals lactationStartCoefficient when mass = birthmass (birth) and lactationMaxCoefficient when mass = massAtBeginningOfWeaning (max lactation)
    else{
      coef= m_param->m_lactationMaxCoefficient+(1-m_param->m_lactationMaxCoefficient)*(mass-m_param->m_massAtBeginningOfWeaning)/(m_param->m_weaningMass-m_param->m_massAtBeginningOfWeaning);
    }//equals lactationMaxCoefficient at beginning of weaning and 1 at weaning
  }
  m_energyIntake*=coef;
}

void Female::grow()
{
  if(isGrownUp()){
    modifyGrowthAllocation(0); //the female is grown up
  }
  Juvenile::grow();
  if (m_reproductiveStatus=="gestation")
  {
    m_foetus.back().grow();
  }
  if (m_reproductiveStatus=="lactation")
  {
    m_infant.back().grow();
  }
}

void Female::slowGrowth()
{
  Juvenile::slowGrowth();//first it slows or pauses its own growth;
  if(m_energyBalance<0&&m_reproductiveStatus=="lactation"){ //if energy balance is still negative then it slows or pauses infant growth
    double energyMissing=-m_energyBalance;
    double newGrowthAllocation=1-(energyMissing*m_param->m_lactationEfficiency/(m_param->m_energyInLeanMass*m_param->m_growthRate*m_infant.back().getDependence()));// if possible growth is not stoped (0) but slowed
    if(newGrowthAllocation<0){
      m_infant.back().modifyGrowthAllocation(0);
    }
    updateEnergyNeeds(); //Energy needs has decreased
    updateEnergyBalance(); //Energy balance has increased
  }
}

void Female::storeEnergy(double energyToStore)
{
  if(m_reproductiveStatus=="lactation") // the energy is divided between the female and the infant
  {
    //for the female:
    Infant::storeEnergy(0.5*energyToStore);
    //for the infant:
    m_infant.back().storeEnergy(0.5*energyToStore);
  }
  else
  {
    Infant::storeEnergy(energyToStore);
  }
}

void Female::checkReproductiveWindow()
{
  vector <int> reproductiveWindow;
  int dayWithWindow(m_param->m_beginningOfReproductiveWindow);
  for (int i=1; i<=m_param->m_lengthOfReproductiveWindow; i++)
  {
    if (dayWithWindow==0){
      dayWithWindow=m_param->m_yearLength;
    }
    reproductiveWindow.push_back(dayWithWindow);
    dayWithWindow++;
    dayWithWindow=dayWithWindow%m_param->m_yearLength;
  }
  if(find(reproductiveWindow.begin(), reproductiveWindow.end(), m_param->getDayOfYear(m_param->t))  != reproductiveWindow.end()) //means "if t is in the reproductive window"
  {
    enterReproductiveWindow();
  }
}

void Female::enterReproductiveWindow()
{
  m_reproductiveWindow=true;
}

void Female::exitReproductiveWindow()
{
  m_reproductiveWindow=false;
}

void Female::startCycling()
{
  m_reproductiveStatus="oestrus";
  m_lastBeginningOfCycle=m_param->t;
  m_cyclingDuration=m_param->m_cyclingDurations[rand() % m_param->m_cyclingDurations.size()];
}

void Female::stopCycling()
{
  m_reproductiveStatus="anoestrus";
}

void Female::conceive()
{
  Foetus foetus(m_param,m_lastBeginningOfCycle);
  m_foetus.push_back(foetus);
  m_reproductiveStatus="gestation";
}

void Female::giveBirth()
{
  Infant infant (m_foetus.back());
  m_infant.push_back(infant);
  m_reproductiveStatus="lactation";
}

void Female::wean()
{
  Juvenile juvenile (m_infant.back());
  m_juvenile.push_back(juvenile);
  m_reproductiveStatus="anoestrus";
}

void Female::miscarry()
{  
  m_foetus.back().die();
  m_reproductiveStatus="anoestrus";
  updateEnergyNeeds();
  updateEnergyBalance();
}

void Female::loseInfant()
{
  m_infant.back().die();
  m_reproductiveStatus="anoestrus";
  updateEnergyNeeds();
  updateEnergyBalance();
}

void Female::offspringIndependentGrowth(NumericVector NDVI)
{
  for (auto& juvenile : m_juvenile){

    m_param->t=juvenile.getWeaningTime();
    while(juvenile.getAge()<m_param->m_ageAtSexualMaturity&juvenile.isAlive()){
      m_param->t++;
      if (juvenile.isDying())
      { juvenile.die();
      }
      if (juvenile.isAlive()){ //If the juvenile is dead, no need to continue...
      // What are the juvenile energy intake today ?
        juvenile.forage(NDVI[m_param->t+m_param->m_beginningOfSimulation]);

        // What are the juvenile energy needs today ?
       juvenile.updateEnergyNeeds();

        // What is its energy balance ? (energy available - energy needs)
        juvenile.updateEnergyBalance();

        if (juvenile.getEnergyBalance()>0){
            juvenile.storeEnergy(juvenile.getEnergyBalance());
        }
        else {// Some energy is missing, the juvenile has to make sacrifices
          //1. Reduction of energy needs by reducing growth of juvenile if
          juvenile.slowGrowth(); 
          if (juvenile.getEnergyBalance()<0){
            //2. Energy missing is taken from energy stored in fat mass possible
            juvenile.releaseEnergyFromFatMass();

            if (juvenile.getEnergyBalance()<0)
            { //3. Energy missing is taken from lean mass
                juvenile.releaseEnergyFromLeanMass(); //everything is taken from leanmass, if it is too much, the juvenile will die
            }
          }
        }

        // Growth of the juvenile
        juvenile.grow();

        //Recording some data
        juvenile.recordMass();
        juvenile.recordEnergy();
      }
      }
    if(juvenile.getAge()>=m_param->m_ageAtSexualMaturity){
      juvenile.reachSexualMaturity();
       }
     }
  }


//Others:

void Female::updateEnergyNeeds()
{ 
  if (m_reproductiveStatus=="anoestrus"||m_reproductiveStatus=="oestrus")
  {
    Juvenile::updateEnergyNeeds();
  }
  
  if (m_reproductiveStatus=="gestation")
  {
    double pregnantMass=m_totalMass+(1+m_param->m_placentalProportion)*m_foetus.back().getTotalMass();
    m_energyNeeds=(m_param->m_allometricCoefficient*pow(pregnantMass,m_param->m_allometricExponent))+ //Maintenance of female with foetus and placenta mass
      m_param->m_energyInLeanMass*m_param->m_growthRate*(m_growthAllocation+1); //growth of female (m_growthAllocation) and foetus + placenta (growth=1)
  }
  if (m_reproductiveStatus=="lactation")
  {
    m_energyNeeds=(m_param->m_allometricCoefficient*pow(m_totalMass,m_param->m_allometricExponent))+//Maintenance of female
      m_param->m_energyInLeanMass*m_param->m_growthRate*m_growthAllocation +//Growth of female
      (1/m_param->m_lactationEfficiency)*m_infant.back().getDependence()*
      (m_param->m_allometricCoefficient*pow(m_infant.back().getTotalMass(),m_param->m_allometricExponent)+
      m_param->m_energyInLeanMass*m_param->m_growthRate*m_infant.back().getGrowthAllocation()); //energy of growth of the infant
    
  }
}

void Female::updateNumberOfDaysFattening()
{

  if (m_param->t==0) //its the first day of simulation
  {
    m_numberOfDaysFattening=0;
  }
 else{
  
  if (m_recordOfFatMass[m_param->t]>m_recordOfFatMass[m_param->t-1])//(m_recordOfFatMass[m_param->t]>=m_recordOfFatMass[m_param->t-1]&&m_recordOfFatMass[m_param->t]!=0)
  {
    m_numberOfDaysFattening++;
  }
  else
  {
    m_numberOfDaysFattening=0;
  }
}
}


//Model outputs :

void Female::recordMass()
{
  Infant::recordMass();
  if (m_reproductiveStatus=="lactation")
  {
    m_infant.back().recordMass();
  }
  if (m_reproductiveStatus=="gestation")
  {
    m_foetus.back().recordMass();
  }
}

void Female::recordReproductiveStatus()
{
  m_recordOfReproductiveStatus.push_back(m_reproductiveStatus);
}



List Female::getJuvenileHistoric()
{
  vector <int> idOffspring;
  vector <int> cycleTimes;
  vector <int> conceptionTimes;
  vector <int> birthTimes;
  vector <int> weaningTimes;
  vector <int> sexualMaturityTimes;
  vector <int> deathTimes;
  vector <int> lifespans;
  vector <double> NDVIdeath;
  
  for(int i =0;i<m_juvenile.size();i++) {
    idOffspring.push_back(m_juvenile[i].getId());
    cycleTimes.push_back(m_juvenile[i].getCycleTime());
    conceptionTimes.push_back(m_juvenile[i].getConceptionTime());
    birthTimes.push_back(m_juvenile[i].getBirthTime());
    weaningTimes.push_back(m_juvenile[i].getWeaningTime());
    sexualMaturityTimes.push_back(m_juvenile[i].getSexualMaturityTime());
    deathTimes.push_back(m_juvenile[i].getDeathTime());
    lifespans.push_back(m_juvenile[i].getLifespan());
    
  }
  
  return List::create(Named("idOffspring")=idOffspring,Named("cycleTimes")=cycleTimes,
                      Named("conceptionTimes")=conceptionTimes,Named ("birthTimes")=birthTimes,
                      Named("weaningTimes")=weaningTimes,Named("sexualMaturityTimes")=sexualMaturityTimes,
                      Named("deathTimes")=deathTimes,Named("lifespans")=lifespans);
  }

List Female::getInfantHistoric()
{
  vector <int> idOffspring;
  vector <int> cycleTimes;
  vector <int> conceptionTimes;
  vector <int> birthTimes;
  vector <int> weaningTimes;
  vector <int> sexualMaturityTimes;
  vector <int> deathTimes;
  vector <int> lifespans;
  
  for(int i =0;i<m_infant.size();i++) {
    idOffspring.push_back(m_infant[i].getId());
    cycleTimes.push_back(m_infant[i].getCycleTime());
    conceptionTimes.push_back(m_infant[i].getConceptionTime());
    birthTimes.push_back(m_infant[i].getBirthTime());
    weaningTimes.push_back(-999);
    sexualMaturityTimes.push_back(-999);
    deathTimes.push_back(m_infant[i].getDeathTime());
    lifespans.push_back(m_infant[i].getLifespan() );
                          }
  return List::create(Named("idOffspring")=idOffspring,Named("cycleTimes")=cycleTimes,
                      Named("conceptionTimes")=conceptionTimes,Named ("birthTimes")=birthTimes,
                      Named("weaningTimes")=weaningTimes,Named("sexualMaturityTimes")=sexualMaturityTimes,
                      Named("deathTimes")=deathTimes,Named("lifespans")=lifespans);
  }

List Female::getFoetusHistoric()
{
  vector <int> idOffspring;
  vector <int> cycleTimes;
  vector <int> conceptionTimes;
  vector <int> birthTimes;
  vector <int> weaningTimes;
  vector <int> sexualMaturityTimes;
  vector <int> deathTimes;
  vector <int> lifespans;
  
  
  for(int i =0;i<m_foetus.size();i++) {
    idOffspring.push_back(m_foetus[i].getId());
    cycleTimes.push_back(m_foetus[i].getCycleTime());
    conceptionTimes.push_back(m_foetus[i].getConceptionTime());
    birthTimes.push_back(-999);
    weaningTimes.push_back(-999);
    sexualMaturityTimes.push_back(-999);
    deathTimes.push_back(m_foetus[i].getDeathTime());
    lifespans.push_back(m_foetus[i].getLifespan());
  }
  
  return List::create(Named("idOffspring")=idOffspring,Named("cycleTimes")=cycleTimes,
                      Named("conceptionTimes")=conceptionTimes,Named ("birthTimes")=birthTimes,
                     Named("weaningTimes")=weaningTimes,Named("sexualMaturityTimes")=sexualMaturityTimes,
                     Named("deathTimes")=deathTimes,Named("lifespans")=lifespans);
}




