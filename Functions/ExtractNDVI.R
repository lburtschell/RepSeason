#Beforehand, follow the instructions below (from https://philippgaertner.github.io/2019/12/earth-engine-rstudio-reticulate/)
#1. sign in on earth engine : https://signup.earthengine.google.com/
#2. install package reticulate on R Studio (be carreful to have the last version of R (at least 4.0.4))
#3. download and install anaconda on your computer (python distibution platform : https://www.anaconda.com/products/distribution)
#4. download and install the gcloud CLI : https://cloud.google.com/sdk/docs/install?hl=en ()
#5. authenticate on gcloud (with the account created in 1.)
#6. open python terminal "Anaconda Powershell Prompt" and enter the following lines :
  #conda create --name gee-demo 
  #conda activate gee-demo 
  #conda install -c conda-forge earthengine-api
  #earthengine authenticate 
  #conda install pandas
  #conda install numpy
  #pip install ipygee
#7. run the following code :

library(reticulate)

extractNDVI<-function(long_min, lat_min,long_max, lat_max){
  use_condaenv("gee-demo", conda = "auto",required = TRUE)
  
  ee = import("ee")          # Import the Earth Engine library
  ui=import("ipygee")        # Import "ipygee" library to create chart 

  ee$Initialize()            # Trigger the authentication
  
  #location coordinates
  geometry = ee$Geometry$Rectangle(long_min, lat_min,long_max, lat_max); # = South-West / North-East
  
  
  #get NDVI Data from MODIS Collections : https://developers.google.com/earth-engine/datasets/catalog/modis
  MODISNDVI = ee$ImageCollection("MODIS/006/MOD13Q1") #Terra Vegetation Indices 16-Day Global 250 m (for 500 m : "MODIS/061/MOD13A1" | for 1 km : "MODIS/006/MOD13A2" )
  
  #select location (and date)
  MODISNDVI = MODISNDVI$filterBounds(geometry)$select("NDVI")  #to add date selection : $filterDate(ee$Date('2010-01-01'), ee$Date('2020-12-31'))
  
  #create a pygal chart (graphic representation : chart$render_to_file('chart.svg'))
  chart = ui$chart$Image$series('imageCollection'= MODISNDVI, 
                                'region'= geometry,
                                'reducer'=ee$Reducer$mean(),
                                'scale'= 60)
  
  #extract dataframe
  ndvi<-chart$dataframe
  
  #format dataframe
  raw_NDVI<-data.frame("Date"=row.names(ndvi),"rawNDVI"=ndvi$NDVI,"NDVI"=ndvi$NDVI*0.0001)
  raw_NDVI$Date<-as.Date(raw_NDVI$Date)
  
  return(raw_NDVI)
}




