library(sf)
library(terra)
library(caret)
library(groupdata2)

split.ratio <- split.ratio #used to change training and validation split ratio
category_col <- ('class')
id_col <- ('originfid')

#Sample Polygons
PolySample <- function(Training_polygons) {
sf_use_s2(FALSE)
poly.points <- st_sample(Training_Polygons, Num_points, type = 'random', by_polygon = TRUE)
poly.points.sf <- st_sf(poly.points)
poly.points.joined <- st_join(poly.points.sf, Training_polygons)
rm(poly.points, poly.points.sf)
}

#Split for Polygons
PolySplit <- function(category_col, id_col, split.ratio) {
  T.poly.points <- as.data.frame(read_sf(poly.points.joined))
  Partition <- groupdata2::partition(T.poly.points, p=split.ratio, cat_col = category_col, id_col = id_col)
  train <- as.data.frame(Partition1[1])
  val <- as.data.frame(Partition1[2])
  rm(poly.points, poly.points.sf, poly.points.joined, T.poly.points, Partition1)
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