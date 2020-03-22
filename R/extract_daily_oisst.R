#' Extract Daily SST from OISST
#'
#' Extract mean sea surface temperature data from NOAA's Optimum Interpolated Sea Surface Temperature (OISST) v2 High Resolution daily or weekly datasets.
#'
#' @param filename full path to unzipped netCDF data file
#' @param landsea_mask full path to land-sea mask netCDF file
#' @param long_w western-most longitude of search area, must be smaller than long_e
#' @param long_e (optional) eastern-most longitude of search area, must be larger than long_w
#' @param lat_s southern-most latitude of search area, must be smaller than lat_n
#' @param lat_n (optional) northern-most latitude of search area, must be larger than lat_s
#'
#' @return A 2-dimensional matrix with latitudes in rows and longitudes in columns
#' @export
extract_daily_oisst <- function(filename, landsea_mask, long_w, long_e, lat_s, lat_n) {
  # Parse latitudes and longitudes ---------------------------

  # Generate set of grid cell-center latitudes (from S to N)
  lat_list <- seq(-89.875, 89.875, 0.25)
  # Generate set of grid cell-center longitudes
  long_list <- seq(0.125, 359.875, 0.25)

  # Get index of nearest longitude value
  long_w_index <- which.min(abs(long_w - long_list))
  if (missing(long_e)) {
    # If long_e is not specified, reuse long_w
    long_e <- long_w
    long_e_index <- long_w_index
    message("Only 1 longitude specified")
  } else {
    # Get index of nearest longitude value to long_e
    long_e_index <- which.min(abs(long_e - long_list))
  }

  # Get index of nearest latitude value
  lat_s_index <- which.min(abs(lat_s - lat_list))
  if (missing(lat_n)) {
    # If lat_n is not specified, reuse lat_s
    lat_n <- lat_s
    lat_n_index <- lat_s_index
    message("Only 1 latitude specified")
  } else {
    # Get index of nearest latitude value to lat_n
    lat_n_index <- which.min(abs(lat_n - lat_list))
  }

  # Get number of longitudes and latitudes to extract
  nlon <- (long_e_index - long_w_index) + 1
  nlat <- (lat_n_index - lat_s_index) + 1

  # Extract the date from the file ---------------------------

  # date_ref <- sst_nc$dim$time$units
  # date_ref <- sub("days since ", "", date_ref, ignore.case = TRUE)
  # date1 <- as.Date(sst_nc$dim$time$vals[1], origin = data_ref)

  # Read SST from netCDF file --------------------------------

  # Create connection to netCDF data file
  sst_nc <- ncdf4::nc_open(filename)

  # Define the output array
  sst_out <- matrix(data = NA, nrow = nlon, ncol = nlat)

  # Extract the data from the netCDF file
  sst_out[, ] <- ncdf4::ncvar_get(
    sst_nc,
    varid = "sst",
    start = c(long_w_index, lat_s_index, 1, 1),
    count = c(nlon, nlat, 1, 1)
  )

  # Replace 32767 value with NA
  sst_out <- ifelse(sst_out == 32767, NA, sst_out)

  # Apply land-sea mask --------------------------------------

  # Open the netCDF land-sea mask
  landsea_nc <- ncdf4::nc_open(landsea_mask)

  # Create array to hold land-sea mask
  mask <- array(data = NA, dim = c(nlon, nlat, 1))

  # Get land-sea mask values (0 or 1)
  mask[, , ] <- ncdf4::ncvar_get(
    landsea_nc,
    varid = "landsea_mask",
    start = c(long_w_index, lat_s_index, 1),
    count = c(nlon, nlat, 1)
  )
  # Replace zeros with NAs
  mask <- ifelse(mask == 0, NA, 1)

  # All masked values become NA
  sst_out[, ] <- sst_out[, ] * as.matrix(mask[, , 1])

  # Add dimension names
  attr(sst_out, "dimnames") <- list(
    Long = seq(long_list[long_w_index], long_list[long_e_index], by = 0.25),
    Lat = seq(lat_list[lat_s_index], lat_list[lat_n_index], by = 0.25)
  )

  # Rearrange the output data so that:
  # Latitudes run from N to S down the rows
  # Longitudes run from W to E across columns
  sst_out <- t(sst_out)
  sst_out <- sst_out[nrow(sst_out):1, ]

  ncdf4::nc_close(sst_nc)
  ncdf4::nc_close(landsea_nc)

  return(sst_out)
}
