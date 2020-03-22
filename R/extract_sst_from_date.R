#' Title
#'
#' @param date Date
#' @param dataset Dataset
#' @param long Longitude
#' @param lat Latitude
#' @param ... Additional arguments
#'
#' @return Mean sea surface temperature
#' @export
extract_sst_from_date <- function(date, dataset, long, lat, ...) {
  if (dataset == "oisst") {
    file_id <- paste0(
      lubridate::year(date),
      sprintf("%02d", lubridate::month(date)),
      sprintf("%02d", lubridate::day(date))
    )

    folder_id <- substr(file_id, 1, 6)

    oisst_file <- paste0("oisst-data/avhrr-only-v2.", file_id, ".nc")

    if (!file.exists(oisst_file)) {
      utils::download.file(
        url = paste0(
          "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/access/avhrr-only/",
          folder_id,
          "/avhrr-only-v2.",
          file_id,
          ".nc"
        ),
        destfile = oisst_file
      )
    }

    sst <- read_oisst(
      filename = oisst_file,
      long_w = long,
      long_e = long,
      lat_s = lat,
      lat_n = lat,
      ...
    )
  } else {
    sst <- NULL
  }

  return(sst)
}
