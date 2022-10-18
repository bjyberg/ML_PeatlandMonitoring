library(caret)
library(raster)
library(ranger)
library(dplyr)
library(doParallel)
library(boruta)
source('Split_training.R')

#Setup
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
#Points
Training_points <- () 
Validation_points <- () # if not given, script will split training points 80/20 by class


##if using data from 2 sites (if more, code can be easily modified), otherwise skip
Site2_variables <- () #Ignore object if the random forest is only being trained on one site --NOT REQUIRED

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

if



  
if (!exists("output_folder")) {
  output_folder <- getwd()
}
####Traing Data Split ----
if (!exists(Validation_polygons)) {
  
}
