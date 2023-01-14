library(terra)
library(sf)

image_path <- '/home/bjyberg/Peatland/data/Site2/S2_dji.tif'
labels_rast_path <- '/home/bjyberg/Peatland/data/RFoutputs/RF11S2.tif'

x <- rast(image_path)
y <- rast(labels_rast_path)

make_masking_grid <- function (image_raster, Dl_size, factor) {
  cell_res <- (Dl_size * factor)
  xyres <- res(image_raster)
  cell_size <- round((cell_res*xyres), 3)
  cell_grid <- vect(st_make_grid(image_raster, cellsize = cell_size))
  plot(image_raster)
  plot(cell_grid, add = T)
  print(paste("Output cells are:", cell_res, 'by', cell_res, 'pixels' ))
  return(cell_grid)
   # if (round(ext(cell_grid),2) != (round(ext(image_raster),2))) {
   #   image_raster <- extend(image_raster, cell_grid, fill=NA)
   #   cat(paste('Warning: NA values introduced on edge of raster to allow for 
   #               output patches of', n_pixels, 'pixels' ), sep = '\n')
   #}
}



grid <- make_masking_grid(x[[1]], Dl_size = 128, factor = 20)  

zoom(x$S2_dji_1, grid[15], new = T)
grid[15]
                          