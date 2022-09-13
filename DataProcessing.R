library(terra)
library(RStoolbox)
source('UAV_indices.R')

####Setup Variables####
Site <- 1 #list site number for saving outputs if performing training and analysis on multiple sites
#import data paths
AOI <- vect('/data/shp/S1Area.shp') #List as NA if not cropping sites 
Micasense.Full <-rast('/data/Fullsite/S1_mica.tif', lyrs = c(1,2,3,4,5)) #MicaSense Imagery
DJI.full <-rast('data/Fullsite/S1_dji.tif', lyrs = c(1,2,3)) #DJI RGB imagery
dsm.mica.full <- rast('/data/Fullsite/S1_mica_DSM.tif') #MicaSense DSM
dsm.dji.full <- rast('/data/Fullsite/S1_dji_DSM.tif') #DJI DSM
twi.mica.full <- rast('/data/Fullsite/S1_Mica_TWI.tif') #MicaSense Topographic Wetness Index (TWI) created using SAGA in QGIS
twi.dji.full <- rast('/home/bjyberg/Peatland/Ardgowan_Data/TWI/DJI_TWI.tif') #DJI TWI
#Set path for processed data outputs
output_folder <- ('/data/prepared')

startTime <- Sys.time() #Start Clock

####Prep MicaSense####
names(Micasense.Full) <- c('blue','green', 'red', 'Nir', 'Redge')e

#Micasense.full <- classify(Micasense.full, cbind(65535, NA)) #Not needed if cropping -- fixing out of extent NA values incorrectly assigned as 65535

if (is.na(AOI) != TRUE) {
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
REsimple.Mica <- MTVI2(MicaRef)
coreRtvi.Mica <- MTVI2(MicaRef)
MCARI.Mica <- MCARI(MicaRef)
PCA.Mica <- rasterPCA(MicaRef, nComp = 2) #adjust nComp as needed
PCA1 <- rast(PCA.Mica$map$PC1)
PCA2 <- rast(PCA.Mica$map$PC2)

##stack Indicies
MicaIndices <- c(NDVI.Mica, GNDVI.Mica, NDRE.Mica, EVI.Mica, MTVI2.Mica, 
                    REsimple.Mica, coreRtvi.Mica, MCARI.Mica, PCA1, PCA2)

names(MicaIndices) <- c('NDVI.Mica', 'GNDVI.Mica', 'NDRE.Mica', 'EVI.Mica', 'MTVI2.Mica', 
                           'REsimple.Mica', 'coreRtvi.Mica', 'MCARI.Mica', 'PCA1', 'PCA2')

writeRaster(micaIndices, paste0(output_folder, '/', Site, 'Mica_Indicies.tif'))

###Prep Micasense Terrain
if (exists("dsm.mica.full")) {
  if (is.na(AOI) != TRUE) {
   dsm.mica <- dsm.mica.full
   print("No Cropping Performed")
  } else {
    dsm.mica <- terra::crop(dsm.mica.full, AOI) #crop to area of interest if provided
  }
} else {
  print("Warning No Terrain Data Provided for Micasense")
  }

if (exists("dsm.mica.full")) {
  terrain.mica <- terrain(dsm.mica, v = c('slope', 'TPI','roughness'), unit = 'degrees')
}

if (exists("twi.mica.full")) {
  if (is.na(AOI) != TRUE) {
  twi.mica <- twi.mica.full
  } else {
    twi.mica <- crop(twi.mica.full, AOI)
  }
}

## Stack Mica terrain
if (exists("twi.mica.full")) {
  Mica.TRindices <- c(dsm.mica, terrain.mica, twi.mica)
  names(Mica.TRindices) <- c('DSM', 'slope', 'tpi', 'roughness', 'twi')
} else if (exists("dsm.mica.full")) {
  Mica.TRindices <- c(dsm.mica, terrain.mica,)
  names(Mica.TRindices) <- c('DSM', 'slope', 'tpi', 'roughness')
}

if (exists("Mica.TRindices")) {
writeRaster(Mica.TRindices, paste0(output_folder, '/', Site, 'MicaTRIndices.tif'))
}

####Prep DJI####
names(RGB.full) <- c('red', 'green', 'blue')
#RGB.full <- classify(RGB.full, cbind(250, NA)) #Not needed if cropping -- fixing out of extent NA values incorrectly assigned as 250
if (is.na(AOI) != TRUE) {
  DJI.rgb <- RGB.full
} else {
  DJI.rgb <- terra::crop(RGB.full, AOI) #crop to area of interest if provided
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

