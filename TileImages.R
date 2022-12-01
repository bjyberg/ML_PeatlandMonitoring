#function for augmentation - copy the magic + some
library(sf)
library(terra)
library(doParallel)
library(foreach)
library(dplyr)
library(caret)

####test Run####
image_path <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files\\MicaClip.tif'
labels_vect_path <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files\\Fast_labels.gpkg'
output_patches_dir <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files'
output_patches_dir <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files'
rast_bands = c(1:5)
rasto <- rast(image_path, lyrs =rast_bands)
labs <- vect(labels_vect_path)
rasterise <- rasterize_labels(labs, field= 'Num_class', rasto)
tiles <-dl_training_tile(rasto, rasterise[[2]], 64, output_patches_dir, 's1')


rast_bands <- c(1:5) #list of bands to read in - e.g., c(1,2,3,5,10) or 1:4
tile_dimensions <- c(64, 64)
length(rast_bands)
print(rast_bands)

#Terra Functions----
rasterize_labels <- function(labels, field, image_raster, output_path){
  start.time <- Sys.time()
  if (crs(labels) != crs(image_raster)) {
    print(paste('Image transformation required for labels from',
                as.character(st_crs(labels)[1]), 'to', 
                as.character(st_crs(image_raster)[1])))
    labels <- project(labels, image_raster)
  } 
  labs_raster <- rasterize(labels, image_raster, field=field)
  if (!missing(output_path)) {
      writeRaster(labs_raster, paste0(output_path,'Label_Raster', '.tif'))
  }
  stop.time <- Sys.time()
  overall_time <- stop.time - start.time
  
  return(list(overall_time, labs_raster))
}

dl_data_tile <- function(image_raster, label_raster, n_pixels, output_path,
                             site_name, partition, seed) {
  start.time <- Sys.time()
  
  cropped.labels <- crop(label_raster, image_raster)
  rast.stack <- c(cropped.labels, image_raster)
  xyres <- res(rast.stack)
  round(xyres[1], 3) == round(xyres[2], 3) #is it square? ~~~~add a warning 
  cell_size <- round((n_pixels*xyres), 3)
  cell_grid <- st_make_grid(rast.stack, cellsize = cell_size)
  patch_cells <- lengths(st_within(cell_grid, 
                                   st_as_sfc(st_bbox(rast.stack)))) >0
  patch_grid <- cell_grid[patch_cells] #keep only the patches with complete data
  
  if (!missing(partition)) {
    if (missing(seed)) {
      set.seed(6255)
      print('Automatically set seed to 6255')
    } else {
      set.seed(seed)
    }
    grid.df <- as.data.frame(patch_grid)
    grid.df$id <- 1:nrow(grid.df)
    train.part <- createDataPartition(grid.df$id, p=partition, list=FALSE)
    train.data <- grid.df[train.part,]
    val.test_unsplit <- grid.df[-train.part,]
    test.val.part <- createDataPartition(val.test_unsplit$id,
                                         p=0.6, list=FALSE)
    val.data <- val.test_unsplit[test.val.part,]
    test.data <- val.test_unsplit[-test.val.part,]
    
    test.data$partition <- 'test'
    train.data$partition <- 'train'
    val.data$partition <- 'validation'
    
    combined.df <- rbind(test.data, train.data, val.data)
    partitioned_tiles <- st_as_sf(combined.df[-2])
    
    test.sfc <- st_sfc(test.data$geometry)
    train.sfc <-st_sfc(train.data$geometry)
    val.sfc <-st_sfc(val.data$geometry)
    
    #create directories for the outputs
    dir.create(paste0(output_patches_dir, '/', 'training/patched_images'),
               recursive = TRUE)
    dir.create(paste0(output_patches_dir, '/', 'validation/patched_images'),
               recursive = TRUE)
    dir.create(paste0(output_patches_dir, '/', 'test/patched_images'),
               recursive = TRUE)
    dir.create(paste0(output_patches_dir, '/', 'training/patched_labels'))
    dir.create(paste0(output_patches_dir, '/', 'validation/patched_labels'))
    dir.create(paste0(output_patches_dir, '/', 'test/patched_labels'))
    
    train_image_tiles <- for(i in 1:length(train.sfc)) {
      tile_i <- rast.stack[[-1]] %>% 
        crop(train.sfc[[i]])%>%
        writeRaster(paste0(output_patches_dir, '/', 'training/patched_images',
                           '/', site_name, '_image_patch_', i, '.tif'))
    }
    val_image_tiles <- for(i in 1:length(val.sfc)) {
      tile_i <- rast.stack[[-1]] %>% 
        crop(val.sfc[[i]])%>%
        writeRaster(paste0(output_patches_dir, '/', 'validation/patched_images',
                           '/', site_name, '_image_patch_', i, '.tif'))
    }
    test_image_tiles <- for(i in 1:length(test.sfc)) {
      tile_i <- rast.stack[[-1]] %>% 
        crop(test.sfc[[i]])%>%
        writeRaster(paste0(output_patches_dir, '/', 'test/patched_images',
                           '/', site_name, '_image_patch_', i, '.tif'))
    }
    train_label_tiles <- for(i in 1:length(train.sfc)) {
      label_tile_i <- rast.stack[[1]] %>%
        crop(train.sfc[[i]]) %>%
        writeRaster(paste0(output_patches_dir, '/', 'training/patched_labels',
                           '/', site_name,'_label_patch_', i, '.tif'))
    }
    val_label_tiles <- for(i in 1:length(val.sfc)) {
      label_tile_i <- rast.stack[[1]] %>%
        crop(val.sfc[[i]]) %>%
        writeRaster(paste0(output_patches_dir, '/', 'validation/patched_labels',
                           '/', site_name,'_label_patch_', i, '.tif'))
    }
    test_label_tiles <- for(i in 1:length(test.sfc)) {
      label_tile_i <- rast.stack[[1]] %>%
        crop(test.sfc[[i]]) %>%
        writeRaster(paste0(output_patches_dir, '/', 'test/patched_labels',
                           '/', site_name,'_label_patch_', i, '.tif'))
    }
    print(paste('Total number of training patches', length(train.sfc)))
    print(paste('Total number of validation patches', length(val.sfc)))
    print(paste('Total number of test patches', length(test.sfc)))
    
  } else {
    dir.create(paste0(output_patches_dir, '/', 'patched_images'))
    image.patch.dir <- paste0(output_patches_dir, '/', 'patched_images')
  
    image_tiles <- for(i in 1:length(patch_grid)) {
    tile_i <- rast.stack[[-1]] %>% 
      crop(patch_grid[[i]])%>%
      writeRaster(paste0(image.patch.dir,
                         '/', site_name, '_image_patch_', i, '.tif'))
        }
  
    dir.create(paste0(output_patches_dir, '/', 'patched_labs'))
    label.patch.dir <- paste0(output_patches_dir, '/', 'patched_labs')
  
    label_tiles <- for(i in 1:length(patch_grid)) {
      label_tile_i <- rast.stack[[1]] %>%
        crop(patch_grid[[i]]) %>%
        writeRaster(paste0(label.patch.dir,
                         '/', site_name,'_label_patch_', i, '.tif'))
    }
  }
  stop.time <- Sys.time()
  overall_time <- stop.time - start.time
  
  print(paste('Elapsed time:', overall_time))
  print(paste('Image and label tiles saved to:', output_patches_dir))
  print(paste('Total number of patches:', length(patch_grid)))
  print(paste('Size of tiles:', cell_size[1]))
  if (exists('partitioned_tiles')) {
    plot(partitioned_tiles)
    return(partitioned_tiles)
  }
}

test_train_split <- function(image_directory, label_directory) {
  list.files('./Data/Imagery/PeakDistrict/TrainingTiles',
             full.names=T,pattern='tif')
}

Slow_parallel_dl_training_tile <- function(image_raster, label_raster, n_pixels, output_path,
                            site_name, n_cores) {
  start.time <- Sys.time()
  cropped.labels <- crop(label_raster, image_raster)
  rast.stack <- c(cropped.labels, image_raster)
  xyres <- res(rast.stack)
  round(xyres[1], 3) == round(xyres[2], 3) #is it square? ~~~~add a warning 
  cell_size <- round((n_pixels*xyres), 3)
  cell_grid <- st_make_grid(rast.stack, cellsize = cell_size)
  patch_cells <- lengths(st_within(cell_grid, 
                                   st_as_sfc(st_bbox(rast.stack)))) >0
  patch_grid <- cell_grid[patch_cells] #keep only the patches with complete data
  par.stack <- wrap(rast.stack)
  if (missing(n_cores)) {
    n_cores <- detectCores()-4
  }
  cl <- makeCluster(n_cores) # we start the parallel cluster
  registerDoParallel(cl) # we register it
  
  dir.create(paste0(output_patches_dir, '/', 'patched_images'))
  image.patch.dir <- paste0(output_patches_dir, '/', 'patched_images')
  
  image_tiles <- foreach(i = 1:length(patch_grid),
                      .packages = c('dplyr', 'terra'),
                      .export= c('par.stack', 'patch_grid')) %dopar% {
    rast.stack <- rast(par.stack)                    
    tile_i <- rast.stack[[-1]] %>% 
      crop(patch_grid[[i]])%>%
      writeRaster(paste0(image.patch.dir,
                         '/', site_name, '_image_patch_', i, '.tif'))
                      }
  
  
  dir.create(paste0(output_patches_dir, '/', 'patched_labs'))
  label.patch.dir <- paste0(output_patches_dir, '/', 'patched_labs')
  
  label_tiles <- foreach(i = 1:length(patch_grid),
                       .packages = c('dplyr', 'terra'),
                       .export = c('par.stack', 'patch_grid'),
                       .inorder=TRUE) %dopar% {
    rast.stack <- rast(par.stack)
    label_tile_i <- rast.stack[[1]] %>%
      crop(patch_grid[[i]]) %>%
      writeRaster(paste0(label.patch.dir,
                         '/', site_name,'_label_patch_', i, '.tif'))
                       }
  stopCluster(cl)
  stop.time <- Sys.time()
  overall_time <- stop.time - start.time
  
  print(paste('Elapsed time:', overall_time))
  print(paste('Image and label tiles saved to:', output_patches_dir,
              'Total # Label Patches:', length(label_tiles),
              'Total # Image Patches', length(image_tiles)))
  print(paste('Size of tiles:', cell_size))
  return(c(overall_time, cell_size, image_tiles, label_tiles))
}

#increase_training_augmentation <- function













########################################################
image_path <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files\\MicaClip.tif'
labels_vect_path <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files\\Fast_labels.gpkg'
output_patches_dir <- 'C:\\Users\\byoungberg\\OneDrive - SRUC\\Documents\\Files'

rasto <- rast(image_path, lyrs=rast_bands)
labs <- vect(labels_vect_path)
rasterise <- rasterize_labels(labs, field= 'Num_class', rasto)
tiles <-dl_data_tile(rasto, rasterise[[2]], 100, output_patches_dir, 's1')




t.crop <- crop(rast.stack,patch_grid)
plot(t.crop$Num_class)

rast.stack <- c(rasterise, rasto)
n_pixels <-32
xyres <- res(rast.stack)
cell_size <- round((n_pixels*xyres), 3) #size (m) per patch ~~~~add a print
cell_grid <- st_make_grid(rast.stack, cellsize = cell_size)
patch_cells <- lengths(st_within(cell_grid, 
                                 st_as_sfc(st_bbox(rast.stack)))) >0
patch_grid <- cell_grid[patch_cells]
plot(patch_grid[1])#

n_cores <- detectCores()-4
cl <- makeCluster(n_cores) # we start the parallel cluster
registerDoParallel(cl) # 
par.stack <- wrap(rast.stack)

labs <- foreach(i = 1:length(patch_grid), .packages = c('terra','sf'),
                .export = c('par.stack', 'patch_grid'), 
                .inorder = TRUE) %dopar% {
label_tile_i <-  crop(par.stack[3], patch_grid[[i]])
  #rast.stack[[1]] %>%
 #crop(patch_grid[[i]])
}

dir.create(paste0(output_patches_dir, '/', 'patched_labs'))
lab.patch.dir <- paste0(output_patches_dir, '/', 'patched_labs')
writeRaster(label_tile_i, paste0(lab.patch.dir,
                                 '/','label_patch_', i, '.tif'))
test <- dl_training_tile(rasto, rasterise[[2]], 400, output_patches_dir, 's1', n_cores=7)
test2 <- NOPARA_dl_training_tile(rasto, rasterise[[2]], 400, output_patches_dir, 's1')
#Star Functions ----
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

                   
####terra functions test runs ####
labs <- rasterize_labels(labels_vect_path, image_path, Field='Num_class')

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

label_tile_I <- rast['Num_class'] %>% #[must be title or layer, make ground mask always 1]
  crop(patch_grid[50])

plot(label_tile_I)

plot(crop(rast[[1]],patch_grid[1]))
plot(patch_grid[1], add = TRUE)
plot(patch_grid[c(1,2,3,4,5)])












NOPARA_dl_training_tile <- function(image_raster, label_raster, n_pixels, output_path,
                             site_name) {
  start.time <- Sys.time()
  cropped.labels <- crop(label_raster, image_raster)
  rast.stack <- c(cropped.labels, image_raster)
  xyres <- res(rast.stack)
  round(xyres[1], 3) == round(xyres[2], 3) #is it square? ~~~~add a warning 
  cell_size <- round((n_pixels*xyres), 3)
  cell_grid <- st_make_grid(rast.stack, cellsize = cell_size)
  patch_cells <- lengths(st_within(cell_grid, 
                                   st_as_sfc(st_bbox(rast.stack)))) >0
  patch_grid <- cell_grid[patch_cells] #keep only the patches with complete data

  #if (missing(n_cores)) {
   # n_cores <- detectCores()-4
#  }
 # cl <- makeCluster(n_cores) # we start the parallel cluster
  #registerDoParallel(cl) # we register it
  
  dir.create(paste0(output_patches_dir, '/', 'patched_images'))
  image.patch.dir <- paste0(output_patches_dir, '/', 'patched_images')
  
  image_tiles <- for(i in 1:length(patch_grid)) {
                           tile_i <- rast.stack[[-1]] %>% 
                             crop(patch_grid[[i]])%>%
                             writeRaster(paste0(image.patch.dir,
                                                '/', site_name, '_image_patch_', i, '.tif'))
                         }
  
  
  dir.create(paste0(output_patches_dir, '/', 'patched_labs'))
  label.patch.dir <- paste0(output_patches_dir, '/', 'patched_labs')
  
  label_tiles <- for(i in 1:length(patch_grid)) {
                           label_tile_i <- rast.stack[[1]] %>%
                             crop(patch_grid[[i]]) %>%
                             writeRaster(paste0(label.patch.dir,
                                                '/', site_name,'_label_patch_', i, '.tif'))
                         }
  #stopCluster(cl)
  stop.time <- Sys.time()
  overall_time <- stop.time - start.time
  
  print(paste('Elapsed time:', overall_time))
  print(paste('Image and label tiles saved to:', output_patches_dir,
              'Total # Label Patches:', length(label_tiles),
              'Total # Image Patches', length(image_tiles)))
  print(paste('Size of tiles:', cell_size))
  return(c(overall_time, cell_size, image_tiles, label_tiles))
}
