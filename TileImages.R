#function for augmentation - copy the magic + some
library(sf)
library(terra)
#library(doParallel)
#library(foreach)
library(dplyr)
library(caret)

#Terra Functions#
rasterize_labels <- function(labels, field, image_raster, output_path){
  start.time <- Sys.time()
  if (crs(labels) != crs(image_raster)) {
    print(paste('Image transformation required for labels from',
                as.character(st_crs(labels)[1]), 'to', 
                as.character(st_crs(image_raster)[1])))
    labels <- project(labels, image_raster)
  } 
  if (ext(labels) > ext(image_raster)) {
    labels  <- crop(labels, image_raster)
  } else if (ext(labels) < ext(image_raster)) {
    image_raster <- crop(image_raster, labels)
  }
  labs_raster <- rasterize(labels, image_raster, field=field)
  if (!missing(output_path)) {
      writeRaster(labs_raster, paste0(output_path,'Label_Raster', '.tif'))
  }
  stop.time <- Sys.time()
  overall_time <- stop.time - start.time
  return(labs_raster)
}

dl_data_tile <- function(image_raster, label_raster, n_pixels, output_path,
                             site_name, partition, seed) {
  start.time <- Sys.time()
  
  if (ext(label_raster) > ext(image_raster)) {
    labels  <- crop(label_raster, image_raster)
  } else if (ext(label_raster) < ext(image_raster)) {
    image_raster <- crop(image_raster, label_raster)
  }
  rast.stack <- c(label_raster, image_raster)
  xyres <- res(rast.stack)
  if (!(round(xyres[1], 3) == round(xyres[2], 3))) {
    stop('Cells are not square')
  }
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


# Slow_parallel_dl_training_tile <- function(image_raster, label_raster, n_pixels, output_path,
#                             site_name, n_cores) {
#   start.time <- Sys.time()
#   cropped.labels <- crop(label_raster, image_raster)
#   rast.stack <- c(cropped.labels, image_raster)
#   xyres <- res(rast.stack)
#   round(xyres[1], 3) == round(xyres[2], 3) #is it square? ~~~~add a warning
#   cell_size <- round((n_pixels*xyres), 3)
#   cell_grid <- st_make_grid(rast.stack, cellsize = cell_size)
#   patch_cells <- lengths(st_within(cell_grid,
#                                    st_as_sfc(st_bbox(rast.stack)))) >0
#   patch_grid <- cell_grid[patch_cells] #keep only the patches with complete data
#   par.stack <- wrap(rast.stack)
#   if (missing(n_cores)) {
#     n_cores <- detectCores()-4
#   }
#   cl <- makeCluster(n_cores) # we start the parallel cluster
#   registerDoParallel(cl) # we register it
# 
#   dir.create(paste0(output_patches_dir, '/', 'patched_images'))
#   image.patch.dir <- paste0(output_patches_dir, '/', 'patched_images')
# 
#   image_tiles <- foreach(i = 1:length(patch_grid),
#                       .packages = c('dplyr', 'terra'),
#                       .export= c('par.stack', 'patch_grid')) %dopar% {
#     rast.stack <- rast(par.stack)
#     tile_i <- rast.stack[[-1]] %>%
#       crop(patch_grid[[i]])%>%
#       writeRaster(paste0(image.patch.dir,
#                          '/', site_name, '_image_patch_', i, '.tif'))
#                       }
# 
# 
#   dir.create(paste0(output_patches_dir, '/', 'patched_labs'))
#   label.patch.dir <- paste0(output_patches_dir, '/', 'patched_labs')
# 
#   label_tiles <- foreach(i = 1:length(patch_grid),
#                        .packages = c('dplyr', 'terra'),
#                        .export = c('par.stack', 'patch_grid'),
#                        .inorder=TRUE) %dopar% {
#     rast.stack <- rast(par.stack)
#     label_tile_i <- rast.stack[[1]] %>%
#       crop(patch_grid[[i]]) %>%
#       writeRaster(paste0(label.patch.dir,
#                          '/', site_name,'_label_patch_', i, '.tif'))
#                        }
#   stopCluster(cl)
#   stop.time <- Sys.time()
#   overall_time <- stop.time - start.time
# 
#   print(paste('Elapsed time:', overall_time))
#   print(paste('Image and label tiles saved to:', output_patches_dir,
#               'Total # Label Patches:', length(label_tiles),
#               'Total # Image Patches', length(image_tiles)))
#   print(paste('Size of tiles:', cell_size))
#   return(c(overall_time, cell_size, image_tiles, label_tiles))
# }

