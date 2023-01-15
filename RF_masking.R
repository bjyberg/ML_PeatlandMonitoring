library(terra)
library(sf)
source('TileImages.R')

image_path <- '/home/bjyberg/Peatland/data/Site2/S2_dji.tif'
labels_rast_path <- '/home/bjyberg/Peatland/data/RFoutputs/RF11S2.tif'
output_folder <- '/home/bjyberg/Peatland/DL_part_again'

DJI_rast_bands = c(1:3)

x <- rast(image_path, lyrs = DJI_rast_bands)
y <- rast(labels_rast_path)

y_re <- resample(y,x)

make_masking_grid <- function (image_raster, Dl_size, factor, output_path) {
  cell_res <- (Dl_size * factor)
  xyres <- res(image_raster)
  cell_size <- round((cell_res*xyres), 3)
  cell_grid <- vect(st_make_grid(image_raster, cellsize = cell_size))
  cell_grid$id <- 1:nrow(cell_grid)
  plot(image_raster)
  plot(cell_grid, add = T)
  text(cell_grid, "id", cex=.8, halo=TRUE)
  cat(paste("Output cells are:", cell_res, 'by', cell_res, 'pixels'), sep = '\n')
  return(cell_grid)
   # if (round(ext(cell_grid),2) != (round(ext(image_raster),2))) {
   #   image_raster <- extend(image_raster, cell_grid, fill=NA)
   #   cat(paste('Warning: NA values introduced on edge of raster to allow for 
   #               output patches of', n_pixels, 'pixels' ), sep = '\n')
   #}
}

grid <- make_masking_grid(y_re, Dl_size = 128, factor = 20)

selected_areas <- c(82,68,76,77,39,27,71,52,50,42,35,74,66,67,37,13)

patch <- y %>% 
  crop(grid[])
names(patch) <- paste0(5, "g")

cut_subset <- function(i, image_raster, grid, output_path, identifier, ...) {
  patch <- image_raster %>% 
    crop(grid[i])
  if (!missing(output_path)) {
  writeRaster(patch, paste0(output_path, '/', identifier,
                            "subset", i, '.tif'), ...)
  }
}

x_selected_mask <- lapply(selected_areas, cut_subset, x,
                          grid, output_folder, identifier ='X', overwrite = T)

y_selected_mask <- lapply(selected_areas, cut_subset, y_re,
                          grid, output_folder, identifier = 'Y')

funct <- function(i, cell_grid) {
  xyres <- res(i)
  cell_size <- round((n_pixels * xyres), 3)
  cell_grid <- vect(st_make_grid(i, cellsize = cell_size))
  if (round(ext(cell_grid), 2) != (round(ext(i), 2))) {
    image_raster <- extend(image_raster, cell_grid, fill = NA)
    cat(
      paste(
        'Warning: NA values introduced on edge of raster to allow for
                output patches of', n_pixels,'pixels'), sep = '\n'
    )
  }
  dir.create(paste0(output_path, '/', 'patched_image'),
             recursive = TRUE)
  
  image_tiles <- for (i in 1:length(cell_grid)) {
    tile_i <- image_raster %>%
      crop(cell_grid[i])
    if (missing(subset_name)) {
      writeRaster(tile_i, paste0(output_path, '/', 'patched_image', '/',
                                 site_name, '_image_patch_', i, '.tif'), ...)
    } else {
      writeRaster(tile_i,paste0(
          output_path, '/', 'patched_image','/', site_name, subset_name,
          '_image_patch_', i, '.tif'), ...)
    }
    cat(paste('Tile progress:', i, 'tile of', length(cell_grid)), sep = "\n")
  }
  stop.time <- Sys.time()
  overall_time <- stop.time - start.time
  
  cat(paste('Elapsed time:', overall_time), sep = "\n")
  cat(paste('Tiles saved to:', paste0(output_path, '/patched_image')), sep =
        "\n")
  cat(paste('Total number of patches:', length(cell_grid)), sep = "\n")
  cat(paste('Size of tiles:', cell_size[1]), sep = "\n")
}

tools::file_path_sans_ext(basename(sources(x_selected_mask[[1]])))

lapply(x_selected_mask, function(i) {
  name <- tools::file_path_sans_ext(basename(sources(i)))
  dl_data_tile(i, n_pixels = 128, output_path = output_folder, site_name = name)
  }
)

lapply(y_selected_mask, function(i) {
  name <- tools::file_path_sans_ext(basename(sources(i)))
  dl_data_tile(i, n_pixels = 128, output_path = output_folder, site_name = name)
  }
)
