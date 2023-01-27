library(sf)
library(terra)

source('TileImages.R')

Crop_tiles <- function(large_patches, image_raster, lab_raster, site,
                       output_path) {
  stack <- c(lab_raster, image_raster)
  for (i in unique(large_patches[[1]])) {
    patch_i <- filter(large_patches, id == i)
    id <- paste0(site, '_subset', i)
    large_tile_i <- stack %>%
      crop(patch_i)
    writeRaster(large_tile_m[-1], paste0(output_path, '/', id, '_image.tif'))
    writeRaster(large_tile_m[1], paste0(output_path, '/', id, '_mask.tif'))
    #assign(paste0(id, i), large_tile_i, envir=.GlobalEnv)
  }
}

MicaS2 <- '/home/bjyberg/Peatland/data/Site2/S2_mica.tif'
labels_path <- '/home/bjyberg/Peatland/data/Site2/S2_lazy_labs.gpkg'
large_tiles <- '~/Peatland/DL_part_again/Selected_patches_S2V2.gpkg'
output_folder <- '/home/bjyberg/Peatland/test_Run'
mica_rast_bands = c(1:5)

labs <- vect(labels_path)
patches <- read_sf(large_tiles)
Mica_raster <- rast(MicaS2, lyrs = mica_rast_bands)

lab_rast <- rasterize_labels(labs, class_field = 'Class',  image_raster = Mica_raster[[1]],
                               background_value = 1)

Crop_tiles(patches, Mica_raster, lab_raster, site = 'Mica_s2', output_folder)

masks <- list.files(output_folder, pattern = '*mask.tif', full.names = T)
images <- list.files(output_folder, pattern = '*image.tif', full.names = T)

for (z in 1:length(masks)) {
  image <- rast(images[z])
  mask <- rast(masks[z])
  dl_data_tile(image, mask, 64, output_folder, paste0('S2_sub', z),
             partition =  0.7, test = F, overwrite = TRUE)
}





