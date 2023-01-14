library(terra)
library(sf)
source('TileImages.R')

image_path <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\GIS\\peat\\S2_dji.tif'
labels_rast_path <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\GIS\\peat\\RF11S2.tif'
output_patches_dir <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\New_Files'

MICA_rast_bands = c(1:5)
DJI_rast_bands = c(1:3)
size = 128


rasto <- rast(image_path, lyrs =DJI_rast_bands)
labs <- rast(labels_rast_path)

labs_re <- resample(labs, rasto)
plot(labs_re)

tiles <-dl_data_tile(rasto, labs_re, size, output_patches_dir, 's1', 0.7, test = FALSE )

tiles


rast_bands <- c(1:5) #list of bands to read in - e.g., c(1,2,3,5,10) or 1:4
tile_dimensions <- c(64, 64)
length(rast_bands)
print(rast_bands)