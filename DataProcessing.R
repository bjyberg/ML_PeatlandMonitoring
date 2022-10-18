## ########################## ##
##
## Author: Brayden Youngberg
##
## This collection of scripts were developed for my MSc dissertation to classify
## aspects of peatland condition using ultra-high resolution remote sensing data.
## 
## This script is used to process RGB and multi-spectral data, along with DEMs, to
## create variables and variable stacks for classification. This utilises a helper
## script defining the calculations for the different indices.
##
## License: GPL-3.0 license
##
## ########################## ##

library(terra)
library(RStoolbox)
source('UAV_indices.R') #Call the functions for creating Optical Indices from other script

###Setup Variables###
#Set path for processed data outputs, defaults to working directory if not listed
output_folder <- ('~/test') # - NOT REQUIRED
#If listed as 'NOT REQUIRED' don't create object unless it is being used
#The TWI and DSM must be from the same DSM dataset (i.e., MicaSense Terrain and MicaSense TWI), and a TWI cannot be used without a DSM

Site_Name <- ('1') #list site number/name for saving outputs
Area <- ('Data/ShapeFiles/SmallerTestArea.shp') #Shapefile for cropping rasters to an area of interest -- NOT REQUIRED
Micasense_Imagery <- ('Data/Site1/S1_mica.tif') # -- REQUIRED
MicaSense_DEM <- ('Data/Site1/S1_DSM_mica.tif') # -- NOT REQUIRED
#MicaSense_TWI <- () #Topographic Wetness Index (TWI) created using SAGA in QGIS -- NOT REQUIRED
DJI_Imagery <- ('Data/Site1/S1_dji.tif')# -- REQUIRED
DJI_DEM <-('Data/Site1/S1_DSM_dji.tif') # -- NOT REQUIRED
DJI_TWI <- ('Data/TWI/DJI_TWI.tif') # -- NOT REQUIRED



####import data####
if (exists("Area")) {
  AOI <- vect(Area) # AOI if using a subsection of a site 
}

Micasense.Full <-rast(Micasense_Imagery, lyrs = c(1,2,3,4,5)) 

DJI.full <-rast(DJI_Imagery, lyrs = c(1,2,3))

if (exists("MicaSense_DEM")) {
  dsm.mica.full <- rast(MicaSense_DEM)
}
if (exists("DJI_DEM")) {
  dsm.dji.full <- rast(DJI_DEM)
}
if (exists("MicaSense_TWI")) {
  twi.mica.full <- rast(MicaSense_TWI)
}
if (exists("DJI_TWI")) {
  twi.dji.full <- rast(DJI_TWI)
}

if (!exists("output_folder")) {
  output_folder <- getwd()
}

startTime <- Sys.time() #Start Clock

####Prep MicaSense####
names(Micasense.Full) <- c('blue','green', 'red', 'Nir', 'Redge')
#Micasense.full <- classify(Micasense.full, cbind(65535, NA)) #Not needed if cropping -- fixing out of extent NA values incorrectly assigned as 65535

if (!exists('AOI')) {
  Micasense <- Micasense.Full
  print('No Crop Performed')
} else {
  Micasense <- terra::crop(Micasense.Full, AOI)#crop to area of interest if provided
  print('Crop Performed')
}

MicaRef <- Mica.reflectance(Micasense, 1:5) #calculate normalized reflectance for Micasense

##Calculate Indices
NDVI.Mica <- NDVI(MicaRef)
GNDVI.Mica <- GNDVI(MicaRef) #function defaults to required bands if named correctly
NDRE.Mica <- NDRE(MicaRef)
EVI.Mica <- EVI(MicaRef)
MTVI2.Mica <- MTVI2(MicaRef)
REsimple.Mica <- RedgeSimple(MicaRef)
coreRtvi.Mica <- coreRtvi(MicaRef)
MCARI2.Mica <- MCARI2(MicaRef)
PCA.Mica <- rasterPCA(MicaRef, nComp = 2) #adjust nComp as needed
PCA1 <- rast(PCA.Mica$map$PC1)
PCA2 <- rast(PCA.Mica$map$PC2)

##stack Indicies
MicaIndices <- c(NDVI.Mica, GNDVI.Mica, NDRE.Mica, EVI.Mica, MTVI2.Mica, 
                    REsimple.Mica, coreRtvi.Mica, MCARI2.Mica, PCA1, PCA2)

names(MicaIndices) <- c('NDVI.Mica', 'GNDVI.Mica', 'NDRE.Mica', 'EVI.Mica', 'MTVI2.Mica', 
                           'REsimple.Mica', 'coreRtvi.Mica', 'MCARI2.Mica', 'PCA1', 'PCA2')

writeRaster(MicaIndices, paste0(output_folder, '/', Site_Name, 'Mica_Indicies.tif'))

###Prep Micasense Terrain

if (exists('dsm.mica.full')) {
  if (!exists('AOI')) {
  dsm.mica <- dsm.mica.full
  print('No Cropping Performed')
  } else {
  dsm.mica <- terra::crop(dsm.mica.full, AOI) #crop to area of interest if provided
  }
} else {
    print('Warning: No Terrain Data Provided for Micasense')
}
  
if (exists('dsm.mica.full')) {
  terrain.mica <- terrain(dsm.mica, v = c('slope', 'TPI','roughness'), unit = 'degrees')
  }


if (exists('twi.mica.full')) {
  if (!exists(AOI)) {
  twi.mica <- twi.mica.full
  } else {
    twi.mica <- crop(twi.mica.full, AOI)
  }
} else {
  print('No TWI Provided')
}

## Stack Mica terrain
if (exists('twi.mica.full')) {
  twi.mica <- resample(twi.mica, dsm.mica)
  Mica.TRindices <- c(dsm.mica, terrain.mica, twi.mica)
  names(Mica.TRindices) <- c('DSM', 'slope', 'tpi', 'roughness', 'twi')
} else if (exists('dsm.mica.full')) {
  Mica.TRindices <- c(dsm.mica, terrain.mica)
  names(Mica.TRindices) <- c('DSM', 'slope', 'tpi', 'roughness')
} else {
  print('No Terrain Variables Output for MicaSense')
}

if (exists('Mica.TRindices')) {
    writeRaster(Mica.TRindices, paste0(output_folder, '/', Site_Name, 'MicaTRIndices.tif'))
}

####Prep DJI####

names(DJI.full) <- c('red', 'green', 'blue')
#RGB.full <- classify(RGB.full, cbind(250, NA)) #Not needed if cropping -- fixing out of extent NA values incorrectly assigned as 250

if (!exists('AOI')) {
  DJI.rgb <- DJI.full
} else {
  DJI.rgb <- terra::crop(DJI.full, AOI) #crop to area of interest if provided 
  }

##create indices
GLI.rgb <- GLI(DJI.rgb)
SBrightness.rgb <- SoilBrightness(DJI.rgb)
Brightness_avg.rgb <- avg.Brightness(DJI.rgb)

##stack indices
DJI.indices <- c(GLI.rgb, SBrightness.rgb, Brightness_avg.rgb)
names(DJI.indices) <- c('GLI', 'SBrigntness', 'Brightness_avg')

writeRaster(DJI.indices, paste0(output_folder, '/', Site_Name, 'DJI_indicies.tif'))

##Prep DJI terrain

if (exists('dsm.dji.full')) {
  if (!exists('AOI')) {
    dsm.dji <- dsm.dji.full
    print('No Cropping Performed')
  } else {
    dsm.dji <- terra::crop(dsm.dji.full, AOI) #crop to area of interest if provided
  }
} else {
  print('Warning No Terrain Data Provided for DJI')
}

if (exists('dsm.dji.full')) {
  terrain.dji <- terrain(dsm.dji, v = c('slope', 'TPI','roughness'), unit = 'degrees')
}

if (exists('twi.dji.full')) {
  if (!exists('AOI')) {
    twi.dji <- twi.dji.full
    print('No Cropping Performed')
  } else {
    twi.dji <- crop(twi.dji.full, AOI)
  }
}

## Stack DJI terrain
if (exists('dsm.dji.full')) {
  if (exists('twi.dji.full')) {
    twi.dji <- resample(twi.dji, dsm.dji)
    DJI.TRindices <- c(dsm.dji, terrain.dji, twi.dji)
    names(DJI.TRindices) <- c('dsm', 'slope', 'tpi', 'roughness', 'twi')
  } else {
    DJI.TRindices <- c(dsm.dji, terrain.dji)
    names(DJI.TRindices) <- c('dsm', 'slope', 'tpi', 'roughness')
  }
} else {
  print('No Terrain Variable Output for DJI')
}

if (exists('DJI.TRindices')) {
  writeRaster(DJI.TRindices, paste0(output_folder, '/', Site_Name, 'DJI_TRindices.tif'))
}


####Print total processing time####
timeDiff <- Sys.time() - startTime
cat('Processing time:', format(timeDiff), '\n')

