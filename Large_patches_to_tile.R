library(sf)
library(terra)

source('TileImages.R')


rast_path <- '/home/bjyberg/Peatland/data/Site2/S2_mica.tif'
rast_bands = c(1:5)
labels_path <- '/home/bjyberg/Peatland/data/Site2/S2_lazy_labs.gpkg'
large_tiles <- '~/Peatland/DL_part_again/Selected_patches_S2V2.gpkg'
output_folder <- '/home/bjyberg/Peatland/test_run_dji'


labs <- vect(labels_path)
patches <- read_sf(large_tiles)
image_raster <- rast(rast_path, lyrs = rast_bands)

lab_rast <- rasterize_labels(labs, class_field = 'Class',  image_raster[[1]],
                               background_value = 1)

Crop_tiles(patches, image_raster, lab_rast, site = 'dji_s2', output_folder)

masks <- list.files(output_folder, pattern = '*mask.tif', full.names = T)
images <- list.files(output_folder, pattern = '*image.tif', full.names = T)

for (z in 1:length(masks)) {
  image <- rast(images[z])
  mask <- rast(masks[z])
  dl_data_tile(image, mask, 128, output_folder, paste0('S2_sub', z),
             partition =  0.7, test = F, overwrite = TRUE)
}

dl_data_tile(image_raster, output_path = output_folder, n_pixels = 64,
             site_name = 's2')





