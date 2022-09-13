library(terra)
library(RStoolbox)
source('UAV_indices.R')

####Setup Variables####
Site <- 1 #list site number for saving outputs if performing training and analysis on multiple sites
#import data paths
#If listed as "NOT REQUIRED" fill using NA or don't create object
AOI <- vect('/home/bjyberg/Peatland/Ardgowan_Data/ShapeFiles/SmallerTestArea.shp') # AOI if using a subsection of a site - NOT REQUIRED
Micasense.Full <-rast('/home/bjyberg/Peatland/Ardgowan_Data/Site1/S1_mica.tif', lyrs = c(1,2,3,4,5)) #MicaSense Imagery - REQUIRED
DJI.full <-rast('/home/bjyberg/Peatland/Ardgowan_Data/Site1/S1_dji.tif', lyrs = c(1,2,3)) #DJI RGB imagery -REQUIRED
dsm.mica.full <- rast('/home/bjyberg/Peatland/Ardgowan_Data/Site1/S1_DSM_mica.tif') #MicaSense DSM - NOT REQUIRED
dsm.dji.full <- NA #DJI DSM - NOT REQUIRED
##twi.mica.full <- rast('/data/Fullsite/S1_Mica_TWI.tif') #MicaSense Topographic Wetness Index (TWI) created using SAGA in QGIS - NOT REQUIRED
twi.dji.full <- rast('/home/bjyberg/Peatland/Ardgowan_Data/TWI/DJI_TWI.tif') #DJI TWI - NOT REQUIRED
#Set path for processed data outputs
output_folder <- ('~/test')

startTime <- Sys.time() #Start Clock

####Prep MicaSense####
names(Micasense.Full) <- c('blue','green', 'red', 'Nir', 'Redge')
#Micasense.full <- classify(Micasense.full, cbind(65535, NA)) #Not needed if cropping -- fixing out of extent NA values incorrectly assigned as 65535

if (exists("AOI")) {
  if (is.na(AOI) == TRUE) {
  Micasense <- Micasense.Full
  print('No Crop Performed')
  } else {
    Micasense <- terra::crop(Micasense.Full, AOI)#crop to area of interest if provided
    print('Crop Performed')
  }
} else {
  AOI <- NA
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

writeRaster(MicaIndices, paste0(output_folder, '/', Site, 'Mica_Indicies.tif'))

###Prep Micasense Terrain
if (!exists("dsm.mica.full")) {
  dsm.mica.full <- NA
}

if (is.na(dsm.mica.full) == FALSE) {
  if (is.na(AOI) == FALSE) {
  dsm.mica <- dsm.mica.full
  print("No Cropping Performed")
  } else {
  dsm.mica <- terra::crop(dsm.mica.full, AOI) #crop to area of interest if provided
  }
} else {
    print("Warning: No Terrain Data Provided for Micasense")
}
  
if (is.na(dsm.mica.full) == FALSE) {
    terrain.mica <- terrain(dsm.mica, v = c('slope', 'TPI','roughness'), unit = 'degrees')
  }
}

if (exists("twi.mica.full")) {
  if (is.na(twi.mica.full) == FALSE)
  if (is.na(AOI) == FALSE) {
  twi.mica <- twi.mica.full
  } else {
    twi.mica <- crop(twi.mica.full, AOI)
  }
}

## Stack Mica terrain
if (exists("twi.mica.full")) {
  if (is.na(twi.mica.full) == FALSE) {
    Mica.TRindices <- c(dsm.mica, terrain.mica, twi.mica)
    names(Mica.TRindices) <- c('DSM', 'slope', 'tpi', 'roughness', 'twi')
  } else if (exists("dsm.mica.full")) {
    Mica.TRindices <- c(dsm.mica, terrain.mica,)
    names(Mica.TRindices) <- c('DSM', 'slope', 'tpi', 'roughness')
  }
}

if (exists("Mica.TRindices")) {
  if (is.na(twi.mica.full) == FALSE) {
    writeRaster(Mica.TRindices, paste0(output_folder, '/', Site, 'MicaTRIndices.tif'))
  }
}

####Prep DJI####
names(RGB.full) <- c('red', 'green', 'blue')
#RGB.full <- classify(RGB.full, cbind(250, NA)) #Not needed if cropping -- fixing out of extent NA values incorrectly assigned as 250

if (exists(twi.mica.full))
  { if (is.na(AOI) != TRUE) {
    DJI.rgb <- RGB.full
  } else {
    DJI.rgb <- terra::crop(RGB.full, AOI) #crop to area of interest if provided 
  }
}

##create indices
GLI.rgb <- GLI(DJI.rgb)
SBrightness.rgb <- SoilBrightness(DJI.rgb)
Brightness_avg.rgb <- avg.Brightness(DJI.rgb)

##stack indices
DJI.indices <- c(GLI.rgb, SBrightness.rgb, Brightness_avg.rgb)
names(DJI.indices) <- c('GLI', 'SBrigntness', 'Brightness_avg')

writeRaster(DJI.indices, paste0(output_folder, '/', Site, 'DJI_indicies.tif'))

##Prep DJI terrain
if (exists("dsm.dji.full")) {
  if (is.na(AOI) != TRUE) {
    dsm.dji <- dsm.dji.full
    print("No Cropping Performed")
  } else {
    dsm.dji <- terra::crop(dsm.dji.full, AOI) #crop to area of interest if provided
  }
} else {
  print("Warning No Terrain Data Provided for DJI")
}

if (exists("dsm.dji.full")) {
  terrain.dji <- terrain(dsm.dji, v = c('slope', 'TPI','roughness'), unit = 'degrees')
}

if (exists("twi.dji.full")) {
  if (is.na(AOI) != TRUE) {
    twi.dji <- twi.dji.full
  } else {
    twi.dji <- crop(twi.dji.full, AOI)
  }
}

## Stack DJI terrain
if (exists("twi.dji.full")) {
  DJI.TRindices <- c(Sdsm.dji, terrain.dji, twi.dji)
  names(DJI.TRindices) <- c('dsm', 'slope', 'tpi', 'roughness', 'twi')
} else if (exists("dsm.dji.full")) {
  DJI.TRindices <- c(Sdsm.dji, terrain.dji)
  names(DJI.TRindices) <- c('dsm', 'slope', 'tpi', 'roughness')
}

if (exists("DJI.TRindices")) {
  writeRaster(DJI.TRindices, paste0(output_folder, '/', Site, 'DJI_TRindices.tif'))
}


####Print total processing time####
timeDiff <- Sys.time() - startTime
cat('Processing time:', format(timeDiff), '\n')

