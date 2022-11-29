#function for augmentation - copy the magic + some
library(stars)
library(sf)
library(terra)
library(doParallel)
image_path <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files\\MicaClip.tif'
labels_path <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files\\Fast_train.tif'
labels_vect_path <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files\\Fast_labels.gpkg'
#Make labels shp and rasterise to image
output_patches_dir <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files'
dl_output_path <- ''
rast_bands <- c(1:5) #list of bands to read in - e.g., c(1,2,3,5,10) or 1:4
tile_dimensions <- c(64, 64)
length(rast_bands)
print(rast_bands)
####terra functions test runs 
labs <- rasterize_labels(labels_vect_path, image_path, Field='Num_class')
rast <- rast(image_path, lyrs = rast_bands)
grid <- rast(nrow = 800, ncol = 800, resolution = 16)
xyres <- res(rast)
round(xyres[1], 3) == round(xyres[2], 3) #is it square?
n_pixels <- 32 #number of pixels per tile#
cell_size <- round((n_pixels*xyres), 3) #size (m) per patch
cell_grid <- st_make_grid(rast, cellsize = cell_size)
patch_cells <- lengths(st_within(cell_grid, st_as_sfc(st_bbox(rast)))) >0
patch_grid <- cell_grid[patch_cells]
plot(rast$MicaClip_1)
plot(patch_grid, add = TRUE)
rast <- c(labs, rast)

for (i in 1:length(patch_grid)) { #add option for parallell and put in function
  tile_i <- rast[[-'labels']] %>% 
    crop(patch_grid[i])
  dir.create(paste0(output_patches_dir, '/', 'patched_images'))
  image.patch.dir <- paste0(output_patches_dir, '/', 'patched_images')
  writeRaster(tile_i, paste0(image.patch.dir, '/','image_patch_', i, '.tif'))
  
  label_tile_i <- rast[['labels']] %>%
    crop(patch_grid[i])
  dir.create(paste0(output_patches_dir, '/', 'patched_labs'))
  writeRaster(label_tile_i, paste0(label.patch.dir,
                                   '/','label_patch_', i, '.tif'))
}

label_tile_I <- rast['Num_class'] %>% #[must be title or layer, make groud mask always 1]
  crop(patch_grid[50])

plot(label_tile_I)

plot(crop(rast[[1]],patch_grid[1]))
plot(patch_grid[1], add= TRUE)
plot(patch_grid[c(1,2,3,4,5)])
####Star test runs
labs <- read_stars(labels_path)
im <- read_stars(image_path)
st_dimensions(im)
st_dimensions(labs)
warped <- st_warp(labs, im)
cropped <- st_crop(im, labs)
st_crs(im)[1]
st_crs(labs)[2]


#Terra Functions
rasterize_labels <- function (labels_path, image_path, Field, output_path) {
  labels <- vect(labels_path)
  example.raster <- rast(image_path, lyr=1)
  if (crs(labels) != crs(example.raster)) {
    print(paste('Image transformation required for labels from',
                as.character(st_crs(labels)[1]), 'to', 
                as.character(st_crs(example.raster)[1])))
    labels <- project(labels,example.raster)
  } 
  labs_raster <- rasterize(labels, example.raster, field=Field)
  if (!missing(output_path)) {#not yet tested
      writeRaster(labs_raster, paste0(output_path,'Label_Raster', '.tif'))
  }
  return(labs_raster)
}

dl_training_tile <- function (image_path, bands, labels_path, output_path) {
  labels <- rast(labels_path)
  raster <- rast(image_path, lyrs = bands)
  cropped
  stack <- c(raster, labels)
  
}





#Star Functions
rasterize_labels <- function (labels_path, image_path, output_path) {
  labs<- read_sf(labels_path)
  labs_raster <- st_rasterize(labs)
  return(labs_raster)
}


dl_training_tile <- function (image_path, label_path, output_path, dimensions) {
  rasterio <- c(bands = rast_bands) # check if c() required
  image <- read_stars(image_path, proxy = TRUE, RasterIO = rasterio)
  labels <- read_stars(label_path, proxy = TRUE) #rm for full image tile
  if (st_crs(labels) != st_crs(image)) {
   labels <- st_transform(labels, st_crs(image))
   #st_warp(labels,image)
    print(paste('Image transformation required for labels from',
                as.character(st_crs(labels)[1]), 'to', 
                as.character(st_crs(image)[1])))
  }
  cropped <- st_crop(image, labels) #rm for full image tile --- Proxy??
  warped <- st_warp(labels, cropped) #not sure of order - above or below the crop
  image_lab_stack <- c(image, warped) #or cropped # or sometin else
  return(image_lab_stack)
}

out <- dl_training_tile(image_path, labels_path)

tiles <- st_tile(image_lab_stack, tile_dimensions[1], tile_dimensions[2])



dl_untile <- function

test <- read_stars('Scotland_150m.tif')

if exists(crop) && crop == FALSE {
  
}

                   