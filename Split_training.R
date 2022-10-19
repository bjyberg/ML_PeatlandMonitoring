library(sf)
library(terra)
library(caret)
library(groupdata2)

split.ratio <- split.ratio #used to change training and validation split ratio
Class.name <- ('class')
ID.name <- ('originfid')

#Split for Polygons
PolySplit <- function(Training_polygons) {
 T.polygons <- as.data.frame(read_sf("Training_polygons"))
  Partition1 <- groupdata2::partition(T.polygons, p=split.ratio, cat_col = 'class', id_col = 'originfid')
  train <- as.data.frame(Partition1[1])
  val <- as.data.frame(Partition1[2])
  st_write(train, paste0(output_folder, "TrainPoints", '1', ".shp"))
  st_write(val, paste0(output_folder, "ValPoints", '1', ".shp"))
}

#split for Points
PointSplit <- Function(Training_points) {
  T.points <- as.data.frame(read_sf("Training_points"))
  Partition1 <- groupdata2::partition(T.polygons, p=split.ratio, cat_col = 'class', id_col = 'originfid')
  s1train <- as.data.frame(Partition1[1])
  s1val <- as.data.frame(Partition1[2])
  st_write(train, paste0(output_folder, Site, "TrainPoints", ".shp"))
  st_write(val, paste0(output_folder, Site, "ValPoints", ".shp"))


Training_polygons$class
Training_polygons$originfid <- as.factor(alldata$originfid)
Partition1 <- groupdata2::partition(alldata, p=0.70, cat_col = 'class', id_col = 'originfid')
train <- as.data.frame(Partition1[1])
val <- as.data.frame(Partition1[2])

Partition2<- partition(val.tst, p=0.50, cat_col = "class", id_col = "originfid")  
val <- as.data.frame(Partition2[1])
tst <- as.data.frame(Partition2[2])