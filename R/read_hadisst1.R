#' Read HadISST1 dataset
#'
#' @param filename Path of netCDF file
#' @param ... Arguments passed to raster::brick()
#'
#' @return RasterBrick object
#' @export
read_hadsst1 <- function(filename, ...) {
  # Create RasterBrick object
  raster_brick <- raster::brick(filename, ...)

  # Set land masses as NA
  raster::NAvalue(raster_brick) <- -32768

  return(raster_brick)
}
