library(terra)
library(sf)

#Inputs
site1_path<- '/home/bjyberg/Peatland/data/Site1/S1_dji.tif'
site2_path <- '/home/bjyberg/Peatland/data/Site2/S2_dji.tif'

output_folder <- '/home/bjyberg/Peatland/DL_part_again'

CNN_input_size <- 128



#functions used
make_grid <- function(raster, dl_tile_size, factor, output_path, name) {
  cell_res <- (dl_tile_size * factor)
  xyres <- res(raster)
  cell_size <- round((cell_res*xyres), 3)
  cell_grid <- vect(st_make_grid(raster, cellsize = cell_size))
  cell_grid$id <- 1:nrow(cell_grid)
  plot(raster)
  plot(cell_grid, add = T)
  text(cell_grid, "id", cex=.8, halo=TRUE)
  cat(paste("Output cells are:", cell_res, 'by', cell_res, 'pixels'), sep = '\n')
  cat(paste('A total of', cell_res^2/dl_tile_size^2, 'input patches of',
            dl_tile_size,'by', dl_tile_size, 'can be made per patch'))
  if (!missing(output_path)) {
    if (missing(name)) {
      name <- tools::file_path_sans_ext(basename(sources(raster)))
    }
    writeVector(cell_grid, paste0(output_path, '/', name, 'tiles.gpkg'), ...)
  }
  return(cell_grid)
  # if (round(ext(cell_grid),2) != (round(ext(image_raster),2))) {
  #   image_raster <- extend(image_raster, cell_grid, fill=NA)
  #   cat(paste('Warning: NA values introduced on edge of raster to allow for 
  #               output patches of', n_pixels, 'pixels' ), sep = '\n')
  #}
}

# match_resolutions <- function(raster_list) {
#   res_list <- lapply(raster_list, function(i) round(res(i),4))
#   flat_res <- unlist(res_list)
#   if (max(flat_res) != min(flat_res)) {
#     lapply(raster_list, resample)
#   }
# }

make_grid(site_1[[1]], dl_tile_size = 128, factor = 20,
          output_path = output_folder, name = 'S1')

make_grid(site_2[[1]], dl_tile_size = 128, factor = 20,
          output_path = output_folder, name = 'S2')

site_1 <- rast(site1_path)
site_2 <-rast(site2_path)
