#function for augmentation - copy the magic + some
library(sf)
library(terra)
#library(doParallel)
#library(foreach)
library(dplyr)
library(caret)

#Terra Functions#
rasterize_labels <- function(labels, class_field, image_raster, background_value, 
                             output_path, Overwrite = FALSE){
  start.time <- Sys.time()
  if (crs(labels) != crs(image_raster)) {
    cat(paste('Image transformation required for labels from',
                as.character(st_crs(labels)[1]), 'to', 
                as.character(st_crs(image_raster)[1])), sep = '\n')
    labels <- project(labels, image_raster)
  } 
  if (ext(labels) > ext(image_raster)) {
    labels  <- crop(labels, image_raster)
  } else if (ext(labels) < ext(image_raster)) {
    image_raster <- crop(image_raster, labels)
  }
  
  if (missing(background_value)) {
    background_value <- length(unique(labels[class_field])) + 1
  }
  cat(paste('Raster background value assigned to:', background_value), sep = '\n')
  
  labs_raster <- rasterize(labels, image_raster, field=class_field,
                           background = background_value)
  if (!missing(output_path)) {
    writeRaster(labs_raster, paste0(output_path,'Label_Raster', '.tif'),
                overwrite = Overwrite)
  }
  stop.time <- Sys.time()
  overall_time <- stop.time - start.time
  cat(paste('Time:',overall_time))
  return(labs_raster)
}

dl_data_tile <- function(image_raster, label_raster, n_pixels, output_path,
                             site_name, partition, seed, test = TRUE) {
  start.time <- Sys.time()
  
  if (missing(label_raster)){
    xyres <- res(image_raster)
    cell_size <- round((n_pixels*xyres), 3)
    cell_grid <- vect(st_make_grid(image_raster, cellsize = cell_size))
    if (round(ext(cell_grid),2) != (round(ext(image_raster),2))) {
      image_raster <- extend(image_raster, cell_grid, fill=NA)
      cat(paste('Warning: NA values introduced on edge of raster to allow for 
                output patches of', n_pixels, 'pixels' ))
    }
    dir.create(paste0(output_path, '/', 'patched_image'),
               recursive = TRUE)
    
    image_tiles <- for(i in 1:length(cell_grid)) {
      tile_i <- image_raster %>% 
        crop(cell_grid[i])%>%
        writeRaster(paste0(output_path, '/', 'patched_image',
                           '/', site_name, '_image_patch_', i, '.tif'))
      cat(paste('Tile progress:', i, 'tile of', length(cell_grid)),sep="\n")
    }
    stop.time <- Sys.time()
    overall_time <- stop.time - start.time
    
    cat(paste('Elapsed time:', overall_time), sep="\n")
    cat(paste('Tiles saved to:', paste0(output_path,'patched_image')),sep="\n")
    cat(paste('Total number of patches:', length(cell_grid)),sep="\n")
    cat(paste('Size of tiles:', cell_size[1]),sep="\n")
  } else {
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
    train.data$partition <- 'train'
    val.test_unsplit <- grid.df[-train.part,]
    
    if (test == TRUE) {
    test.val.part <- createDataPartition(val.test_unsplit$id,
                                         p=0.6, list=FALSE)
    val.data <- val.test_unsplit[test.val.part,]
    test.data <- val.test_unsplit[-test.val.part,]
    test.data$partition <- 'test'
    val.data$partition <- 'validation'
    combined.df <- rbind(test.data, train.data, val.data)
    partitioned_tiles <- st_as_sf(combined.df[-2])
    test.sfc <- st_sfc(test.data$geometry)
    } else {
      val.data <- val.test_unsplit
      val.data$partition <- 'validation'
      combined.df <- rbind(train.data, val.data)
      partitioned_tiles <- st_as_sf(combined.df[-2])
    }
    
    train.sfc <-st_sfc(train.data$geometry)
    val.sfc <-st_sfc(val.data$geometry)
    
    #create directories for the outputs
    dir.create(paste0(output_path, '/', 'training/patched_images'),
               recursive = TRUE)
    dir.create(paste0(output_path, '/', 'training/patched_labels'))
    
    dir.create(paste0(output_path, '/', 'validation/patched_images'),
               recursive = TRUE)
    dir.create(paste0(output_path, '/', 'validation/patched_labels'))
    
    if (test == TRUE) {
    dir.create(paste0(output_path, '/', 'test/patched_images'),
               recursive = TRUE)
    dir.create(paste0(output_path, '/', 'test/patched_labels'))
    }
    
    train_image_tiles <- for(i in 1:length(train.sfc)) {
      tile_i <- rast.stack[[-1]] %>% 
        crop(train.sfc[[i]])%>%
        writeRaster(paste0(output_path, '/', 'training/patched_images',
                           '/', site_name, '_image_patch_', i, '.tif'))
      cat(paste('Training tile progress:', i, 'tile of', length(train.sfc)),sep="\n")
    }
    val_image_tiles <- for(i in 1:length(val.sfc)) {
      tile_i <- rast.stack[[-1]] %>% 
        crop(val.sfc[[i]])%>%
        writeRaster(paste0(output_path, '/', 'validation/patched_images',
                           '/', site_name, '_image_patch_', i, '.tif'))
      cat(paste('Val tile progress:', i, 'tile of', length(val.sfc)),sep="\n")
    }
    if (test == TRUE) {
      test_image_tiles <- for(i in 1:length(test.sfc)) {
        tile_i <- rast.stack[[-1]] %>% 
          crop(test.sfc[[i]])%>%
          writeRaster(paste0(output_path, '/', 'test/patched_images',
                            '/', site_name, '_image_patch_', i, '.tif'))
       cat(paste('Test tile progress:', i, 'tile of', length(test.sfc)),sep="\n")
      }
    }
    
    train_label_tiles <- for(i in 1:length(train.sfc)) {
      label_tile_i <- rast.stack[[1]] %>%
        crop(train.sfc[[i]]) %>%
        writeRaster(paste0(output_path, '/', 'training/patched_labels',
                           '/', site_name,'_label_patch_', i, '.tif'))
      cat(paste('Train label progress:', i, 'tile of', length(train.sfc)),sep="\n")
    }
    val_label_tiles <- for(i in 1:length(val.sfc)) {
      label_tile_i <- rast.stack[[1]] %>%
        crop(val.sfc[[i]]) %>%
        writeRaster(paste0(output_path, '/', 'validation/patched_labels',
                           '/', site_name,'_label_patch_', i, '.tif'))
      cat(paste('Val label progress:', i, 'tile of', length(val.sfc)),sep="\n")
    }
    if (test == TRUE) {
      test_label_tiles <- for(i in 1:length(test.sfc)) {
        label_tile_i <- rast.stack[[1]] %>%
          crop(test.sfc[[i]]) %>%
          writeRaster(paste0(output_path, '/', 'test/patched_labels',
                            '/', site_name,'_label_patch_', i, '.tif'))
        cat(paste('test label progress:', i, 'tile of', length(test.sfc)),sep="\n")
      }
    }
    cat(paste('Total number of training patches:', length(train.sfc)),sep="\n")
    cat(paste('Total number of validation patches:', length(val.sfc)),sep="\n")
    if (test == TRUE) {
      cat(paste('Total number of test patches:', length(test.sfc)),sep="\n")
    }
  } else {
    dir.create(paste0(output_path, '/', 'patched_images'))
    image.patch.dir <- paste0(output_path, '/', 'patched_images')
  
    image_tiles <- for(i in 1:length(patch_grid)) {
    tile_i <- rast.stack[[-1]] %>% 
      crop(patch_grid[[i]])%>%
      writeRaster(paste0(image.patch.dir,
                         '/', site_name, '_image_patch_', i, '.tif'))
    cat(paste('Image tile progress:', i, 'tile of', length(patch_grid)),sep="\n")
        }
  
    dir.create(paste0(output_path, '/', 'patched_labs'))
    label.patch.dir <- paste0(output_path, '/', 'patched_labs')
  
    label_tiles <- for(i in 1:length(patch_grid)) {
      label_tile_i <- rast.stack[[1]] %>%
        crop(patch_grid[[i]]) %>%
        writeRaster(paste0(label.patch.dir,
                         '/', site_name,'_label_patch_', i, '.tif'))
      cat(paste('label tile progress:', i, 'tile of', length(patch_grid)),sep="\n")
    }
  }
  stop.time <- Sys.time()
  overall_time <- stop.time - start.time
  
  cat(paste('Elapsed time:', overall_time), sep = '\n')
  cat(paste('Image and label tiles saved to:', output_path),sep="\n")
  cat(paste('Total number of patches:', length(patch_grid)),sep="\n")
  cat(paste('Size of tiles:', cell_size[1]),sep="\n")
  if (exists('partitioned_tiles')) {
    tryCatch(
      {
        plot(partitioned_tiles)
        },
      error = function(e) {
        message('An error occured while plotting:')
        print(e)
      },
      warning = function(w) {
        message('A warning occured while plotting:')
        print(w)
      }
    )
    return(partitioned_tiles)
  }
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
#   dir.create(paste0(output_path, '/', 'patched_images'))
#   image.patch.dir <- paste0(output_path, '/', 'patched_images')
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
#   dir.create(paste0(output_path, '/', 'patched_labs'))
#   label.patch.dir <- paste0(output_path, '/', 'patched_labs')
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
#   print(paste('Image and label tiles saved to:', output_path,
#               'Total # Label Patches:', length(label_tiles),
#               'Total # Image Patches', length(image_tiles)))
#   print(paste('Size of tiles:', cell_size))
#   return(c(overall_time, cell_size, image_tiles, label_tiles))
# }

