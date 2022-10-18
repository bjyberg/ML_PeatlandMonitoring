library(caret)
library(raster)
library(ranger)
library(dplyr)
library(doParallel)
library(boruta)
library(sf)
source('Split_training.R') #only needed if training and validation polygons are in the same shapefile

#Setup
run_botura <- ('no') #Run Botura feature selection? (yes/no), defaults to no -- Required
run_name <- ("Micasense_all") # Name to save output maps and trained algorithm -- Required
Output_Folder <- ('test/RF_out') # Path for outputs. Defaults to working directory if not provided -- NOT REQUIRED
#set path for variables
Site_Optical_variables <- ('/home/bjyberg/test/S1Mica_Indicies.tif') # --Required
Site_Terrain_Variables <- ('/home/bjyberg/test/1MicaTRIndices.tif') # --Not Required

#import labelled data - either polygons or points are accepted
#Polygons which points are sampled from
Num_points <- (1500) #number of points to be created from polygons
Training_polygons <- () 
Validation_polygons <- () #if not provided, training_polygons will be split 80/20 by class and by polygon to avoid pseudo-replication
                          #For this to work, there each polygon must have a unique id within the attribute table
                          #split percent can be changed in Split_training.R
#Points
Training_points <- () 
Validation_points <- () # if not given, script will split training points 80/20 by class


##if using data from 2 sites (if more, code can be easily modified), otherwise skip
Site2_Optical_variables <- () #Ignore object if the random forest is only being trained on one site --NOT REQUIRED
Site2_Terrain_variables <- () # -- NOT REQUIRED

#import labelled data - either polygons or points are accepted
#Polygons which points are sampled from
Num_points2 <-()
Training_polygons2 <- () 
Validation_polygons2 <- () #if not provided, training_polygons will be split 80/20 by class and by polygon to avoid pseudo-replication
#Points
Training_points2 <- () 
Validation_points2 <- () # if not given, script will split training points 80/20 by class

####Import Data ----
S1.opti.data <- rast(Site_Optical_variables)

if (exists("Site_Terrain_Variables")) {
  S1.terrain.data <- rast(Site_Terrain_Variables)
} else {
  print("No Terrain Data Provided")
}

if (exists('Site2_Optical_variables')) {
  S2.opti.data <- rast(Site2_Optical_variables)
}

if (exists('Site2_Terrain_variables')) {
  S2.terrain.data <- rast(Site2_Optical_variables)
}
####Set up ----
if (!exists("output_folder")) {
  output_folder <- getwd()
}

####Training Data Split ----
if (exists('Validation_polygons')) {
  s1.val.poly <- vect(Validation_polygons)
  S1.train.poly <- vect(Training_polygons)
} else if (exists('Training_polygons')) {
  S1.train.poly <- st_read(Training_polygons)
  sf_use_s2(FALSE)
  s1.poly.points <- st_sample(S1.train.poly, Num_points, type = 'random', by_polygon = TRUE)
  PolySplit(s1.poly.points)
  st_write(train, paste0(output_folder, "TrainPoints", '1', ".shp"))
  st_write(val, paste0(output_folder, "ValPoints", '1', ".shp"))
  S1.train.poly <- vect(paste0(output_folder, "TrainPoints", '1', ".shp"))
  S1.val.poly <- vect(paste0(output_folder, "ValPoints", '1', ".shp"))
}

if (exists('Validation_points')) {
  s1.val.point <- vect(Validation_points)
  S1.train.point <- vect(Training_points)
} else if (exists('Training_polygons')) {
  PointSplit()
  st_write(train, paste0(output_folder, "TrainPoints", '1', ".shp"))
  st_write(val, paste0(output_folder, "ValPoints", '1', ".shp"))
  S1.train.point <- vect(paste0(output_folder, "TrainPoints", '1', ".shp"))
  S1.val.point <- vect(paste0(output_folder, "ValPoints", '1', ".shp"))
} 

if (!exists('Training_points') & !exists('Training_polygons')) {
  stop("Training data must be provided for the first site")
}


### for second site
if (exists('Validation_polygons2')) {
  s2.val.poly <- vect(Validation_polygons2)
  S2.train.poly <- vect(Training_polygons2)
} else if (exists('Training_polygons2')) {
  S2.train.poly <- st_read(Training_polygons2)
  sf_use_s2(FALSE)
  s2.poly.points <- st_sample(S2.train.poly, Num_points2, type = 'random', by_polygon = TRUE)
  PolySplit(s2.poly.points)
  st_write(train, paste0(output_folder, "TrainPoints", '2', ".shp"))
  st_write(val, paste0(output_folder, "ValPoints", '2', ".shp"))
  S2.train.poly <- vect(paste0(output_folder, "TrainPoints", '2', ".shp"))
  S2.val.poly <- vect(paste0(output_folder, "ValPoints", '2', ".shp"))
}

if (exists('Validation_points2')) {
  s2.val.point <- vect(Validation_points2)
  S2.train.point <- vect(Training_points2)
} else if (exists('Training_polygons2')) {
  PointSplit()
  st_write(train, paste0(output_folder, "TrainPoints", '2', ".shp"))
  st_write(val, paste0(output_folder, "ValPoints", '2', ".shp"))
  S2.train.point <- vect(paste0(output_folder, "TrainPoints", '2', ".shp"))
  S2.val.point <- vect(paste0(output_folder, "ValPoints", '2', ".shp"))
} 

if ((exists('Site2_Optical_Variables') | exists('Site2_Terrain_Variables')) & !exists('Training_points2') & !exists('Training_polygons2')) {
  stop("No training data provided for the second site. This is required, as a variable stack was provided for the site")
}
