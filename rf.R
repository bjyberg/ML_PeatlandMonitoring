###Due to the way the sampling is set up in the script, it is best to use training and validation points,
###rather than polygons in the script. If digitisation is preferred using polygons, it is suggested to 
###sample points from those polygons prior to use in this script - this can be done using Orfreo Toolbox and 
###other GIs software. However, it is still possible to use polygons in this script. It is designed to avoid 
###sampling training and testing data from the same polygons if the polygons are not separate files. 
###When sampling points from polygons, it is designed to sample with regard to 

## ############################################################## ##

### ################################################################# ###

library(caret)
library(terra)
library(ranger)
library(dplyr)
library(doParallel)
library(boruta)
library(sf)
source('split_training.R') #only needed if training and validation polygons are in the same shapefile

#---- Required Setup ---
#Options
run_name <- ("Micasense_all") # Name to save output maps and trained algorithm -- Required
Training_Name <- ("1500 Points") #name to save training/validation points --Required if separate points are not provided
run_botura <- ('no') #Run Botura feature selection? (yes/no), defaults to no -- Required
set.seed(6255) #Allows repeatability in random sampling/number generation -- NOT REQUIRED
split.ratio <-0.8 #determines the split percentage of labelled data (Default: 80% Training, 20% validation)
Output_Folder <- ('test/RF_out') # Path for outputs. Defaults to working directory if not provided -- NOT REQUIRED

#set path for variables
site_Optical_variables <- ('/home/bjyberg/test/s1Mica_Indicies.tif') # --Required
site_Terrain_Variables <- ('/home/bjyberg/test/1MicaTRIndices.tif') # --Not Required

#import labelled data - either polygons or points are accepted
#Polygons which points are sampled from
Num_points <- (1500)  #number of points to be created from polygons. 
                      #If Full polygons are to be used for training, leave blank
Training_polygons <- () 
Validation_polygons <- () #if not provided, training_polygons will be split 80/20 by class and by polygon
                          #For this to work, there each polygon needs an id within the attribute table
#Points
Training_points <- () 
Validation_points <- () # if not given, script will split training points 80/20 by class


##if using data from 2 sites (if more, code can be easily modified), otherwise skip
site2_Optical_variables <- () #Ignore object if the random forest is only being trained on one site --NOT REQUIRED
site2_Terrain_variables <- () # -- NOT REQUIRED

#import labelled data - either polygons or points are accepted
#Polygons which points are sampled from
Num_points2 <-()
Training_polygons2 <- () 
Validation_polygons2 <- () #if not provided, training_polygons will be split 80/20 by class and by polygon to avoid pseudo-replication
#Points
Training_points2 <- () 
Validation_points2 <- () # if not given, script will split training points 80/20 by class

####Import Data ----
s1.opti.data <- rast(site_Optical_variables)

if (exists("site_Terrain_Variables")) {
  s1.terrain.data <- rast(site_Terrain_Variables)
} else {
  print("No Terrain Data Provided")
}

if (exists('site2_Optical_variables')) {
  s2.opti.data <- rast(site2_Optical_variables)
}

if (exists('site2_Terrain_variables')) {
  s2.terrain.data <- rast(site2_Optical_variables)
}
#### Start Clock and Make Output folder----
if (!exists("output_folder")) {
  output_folder <- getwd()
}
startTime <- Sys.time() 
####Training Data split ----
if (exists('Validation_polygons') & exists("Num_points")) {
  s1.val.poly <- st_read(Validation_polygons)
  s1.train.poly <- st_read(Training_polygons)
  PolySample(s1.train.poly)
  st_write(poly.points.joined, paste0(output_folder, "Poly_Train", Training_Name, '1', ".shp"))
  PolySample(s1.val.poly)
  st_write(poly.points.joined, paste0(output_folder, "Poly_Val", Training_Name, '1', ".shp"))
  s1.train.points <- vect(paste0(output_folder, "Poly_Train", Training_Name, '1', ".shp"))
  s1.val.points <- vect(paste0(output_folder, "Poly_Val", Training_Name, '1', ".shp"))
  rm(s1.train.poly, s1.val.poly)
  
} else if (exists('Validation_polygons')) {
  s1.val.poly <- vect(Validation_polygons)
  s1.train.poly <- vect(Training_polygons)
} else if (exists('Training_polygons') & !exists('Validation_polygons')) {
  s1.train.poly <- st_read(Training_polygons)
  PolySample(s1.train.poly)
  PolySplit(poly.points.joined)
  st_write(train, paste0(output_folder, "Training_", Training_Name, '1', ".shp"))
  st_write(val, paste0(output_folder, "Val_", Training_Name, '1', ".shp"))
  s1.train.poly <- vect(paste0(output_folder, "Training_", Training_Name, '1', ".shp"))
  s1.val.poly <- vect(paste0(output_folder, "Val_", Training_Name, '1', ".shp"))
}

if (exists('Validation_points')) {
  s1.val.point <- vect(Validation_points)
  s1.train.point <- vect(Training_points)
} else if (exists('Training_points')) {
  partition.points <- vect(Training_points)
  training.part <- createDataPartition(points$class, p=split.ratio, list = FALsE)
  train_data <- points[TrainingP, ]
  test_data <- points[-TrainingP, ]
  writeVector(train_data, (paste0(output_folder, "Partitioned_Train", Training_Name, '1', ".shp")))
  writeVector(train_data, (paste0(output_folder, "Partitioned_Validation", Training_Name, '1', ".shp")))         
} 

if (!exists('Training_points') & !exists('Training_polygons')) {
  stop("Training data must be provided for the first site")
}


###split for second site####
if (exists('Validation_polygons2') & exists(Num_points2)) {
  s2.val.poly <- st_read(Validation_polygons2)
  s2.train.poly <- st_read(Training_polygons2)
  PolySample(s2.train.poly)
  st_write(poly.points.joined, paste0(output_folder, "Poly_Train", Training_Name, '2', ".shp"))
  PolySample(s2.val.poly)
  st_write(poly.points.joined, paste0(output_folder, "Poly_Val", Training_Name, '2', ".shp"))
  s2.train.point <- vect(paste0(output_folder, "poly_Train", Training_Name, '2', ".shp"))
  s2.val.point <- vect(paste0(output_folder, "Poly_Val",Training_Name, '2', ".shp"))
  rm(s2.val.poly, s2.train.poly))
} else if (exists('Validation_polygons2')) {
  s2.val.poly <- vect(Validation_polygons2)
  s2.train.poly <- vect(Training_polygons2)
} else if (exists('Training_polygons2') & !exists('Validation_polygons2')) {
  s2.train.poly <- st_read(Training_polygons2)
  PolySample(s2.train.poly)
  PolySplit(poly.points.joined)
  st_write(train, paste0(output_folder, "TrainPoints", '2', ".shp"))
  st_write(val, paste0(output_folder, "ValPoints", '2', ".shp"))
  s2.train.poly <- vect(paste0(output_folder, "TrainPoints", '2', ".shp"))
  s2.val.poly <- vect(paste0(output_folder, "ValPoints", '2', ".shp"))
}

if (exists('Validation_points2')) {
  s2.val.point <- vect(Validation_points2)
  s2.train.point <- vect(Training_points2)
} else if (exists('Training_points2')) { 
  partition.points2 <- vect(Training_points2)
  TrainingP2 <- createDataPartition(points$class, p=split.ratio, list = FALsE)
  train_data2 <- points[TrainingP2, ]
  test_data2 <- points[-TrainingP2, ]
  writeVector(train_data2, (paste0(output_folder, "Partitioned_Train", Training_Name, '2', ".shp")))
  writeVector(test_data2, (paste0(output_folder, "Partitioned_Validation", Training_Name, '2', ".shp")))
  s2.train.point <- vect(paste0(output_folder, "Partitioned_Train", Training_Name, '2', ".shp"))
  s2.val.point <- vect(paste0(output_folder, "Partitioned_Validation", Training_Name, '2', ".shp"))
  rm(TrainingP2, train_data2, test_data2)
} 

if ((exists('site2_Optical_Variables') | exists('site2_Terrain_Variables')) & !exists('Training_points2') & !exists('Training_polygons2')) {
  stop("No training data provided for the second site. This is required, as a variable stack was provided for the site")
}



###Boruta####

