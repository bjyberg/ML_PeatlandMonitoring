library(terra)
library(RStoolbox)
source('UAV_indices.R')

####Setup Variables###
Site <- 1 #list site number for saving outputs if performing training and analysis on multiple sites
#import data
AOI <- vect('/data/shp/S1Area.shp') #List as NA if not cropping sites 
Micasense.Full <-rast('/data/Fullsite/S1_mica.tif', lyrs = c(1,2,3,4,5)) #MicaSense Imagery
DJI.full <-rast('data/Fullsite/S1_dji.tif', lyrs = c(1,2,3)) #DJI RGB imagery
dsm.mica.full <- rast('/data/Fullsite/S1_mica_DSM.tif') #MicaSense DSM
dsm.dji.full <- rast('/data/Fullsite/S1_dji_DSM.tif') #DJI DSM
twi.mica.full <- rast('/data/Fullsite/S1_Mica_TWI.tif') #MicaSense Topographic Wetness Index (TWI) created using SAGA in QGIS
twi.dji.full <- rast('TWI/S1_DJI_TWI.tif') #DJI TWI
#Set path for processed data outputs
output_folder <- ('/data/prepared')

startTime <- Sys.time() #Start Clock

##Prep MicaSense###
names(Micasense.Full) <- c('blue','green', 'red', 'Nir', 'Redge')

#Site1_Micasense <- classify(Site1_Micasense, cbind(65535, NA)) #Not needed if cropping -- fixing out of extent NA values incorrectly assigned as 65535
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
if (is.na(AOI) != TRUE) {
  dsm.mica <- dsm.mica.full
} else {
  dsm.mica <- terra::crop(dsm.mica.full, AOI) #crop to area of interest if provided
}

terrain.mica <- terrain(dsm.mica, v = c('slope', 'TPI','roughness'), unit = 'degrees')
if (is.na(AOI) != TRUE) {
  twi.mica <- twi.mica.full
} else {
  twi.mica <- crop(twi.mica.full, AOI)
}

## Stack Mica terrain
Mica.TRindices <- c(dsm.mica, terrain.mica, twi.mica)
names(S1.Mica.TRindices) <- c('DSM', 'slope', 'tpi', 'roughness', 'twi')

writeRaster(S1.Mica.TRindices, paste0(output_folder, '/', Site, 'MicaTRIndices.tif'))

###Prep DJI###
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
dsm.dji.full <- rast('FullSite/S1_dji_DSM.tif')
dsm.dji <- terra::crop(dsm.dji.full, AOI)

terrain.dji <- terra::terrain(dsm.dji, v = c('slope', 'TPI','roughness'), unit = 'degrees')
if (is.na(AOI) != TRUE) {
  twi.dji <- twi.dji.full
} else {
  twi.dji <- crop(twi.dji.full, AOI)
}

##Stack dji terrain
DJI.TRindices <- c(Sdsm.dji, terrain.dji, twi.dji)
names(DJI.TRindices) <- c('dsm', 'slope', 'tpi', 'roughness', 'twi')
writeRaster(DJI.TRindices, paste0(output_folder, '/', Site, 'DJI_TRindices.tif'))

#Print total processing time
timeDiff <- Sys.time() - startTime
cat('Processing time:', format(timeDiff), '\n')

