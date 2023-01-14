library(terra)
library(sf)
source('TileImages.R')

image_path <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files\\MicaClip.tif'
labels_vect_path <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files\\Fast_labels.gpkg'
output_patches_dir <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files'
rast_bands = c(1:5)
rasto <- rast(image_path, lyrs =rast_bands)
labs <- vect(labels_vect_path)
labs$Num_class<- recode(labs$Num_class,
                        '1' = '0',
                        '2' = '1',
                        '3' = '2',
                        '4' = '3',
                        '5' = '4',
                        '6' = '5',
                        '7' = '6')

rasterise <- rasterize_labels(labs, field= 'Num_class', rasto)
tiles <-dl_data_tile(rasto, rasterise, 64, output_patches_dir, 's1', 0.7)
plot(rasterise, add = TRUE)
plot(tiles, add = TRUE)


rast_bands <- c(1:5) #list of bands to read in - e.g., c(1,2,3,5,10) or 1:4
tile_dimensions <- c(64, 64)
length(rast_bands)
print(rast_bands)