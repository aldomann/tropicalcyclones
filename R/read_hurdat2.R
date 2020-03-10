#' Title
#'
#' @param con a connection object or a character string. This is the file path or URL for the HURDAT2 dataset to read.
#' @param interp_wind whether missing wind speeds should be linearly interpolated.
#'
#' @return Hurricane observations dataframe
#' @export
#'
#' @importFrom rlang .data
read_hurdat2 <- function(con, interp_wind = FALSE) {
  # Read and split raw data ----------------------------------

  hurr_tracks <- readLines(con)
  hurr_tracks <- lapply(hurr_tracks, stringr::str_split, pattern = ",", simplify = TRUE)

  # Clean the raw data ---------------------------------------

  # Split the hurr_tracks into meta and observation lists
  hurr_lengths <- sapply(hurr_tracks, length)
  hurr_meta <- hurr_tracks[hurr_lengths == 4]
  hurr_obs_raw <- hurr_tracks[hurr_lengths == 21]

  # Create and clean meta data frame
  hurr_meta <- lapply(hurr_meta, tibble::as_tibble)
  hurr_meta <- dplyr::bind_rows(hurr_meta)

  hurr_meta <- hurr_meta %>%
    dplyr::select(-.data$V4) %>%
    dplyr::rename(storm_id = .data$V1, storm_name = .data$V2, n_obs = .data$V3) %>%
    dplyr::mutate(
      n_obs = as.numeric(.data$n_obs)
    )

  storm_ids <- rep(hurr_meta$storm_id, times = hurr_meta$n_obs)

  # Create and clean obs data frame
  hurr_obs <- lapply(hurr_obs_raw, tibble::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(storm_id = storm_ids) %>%
    dplyr::select(.data$storm_id, dplyr::everything(), -.data$V21) %>%
    dplyr::rename(
      date = .data$V1, time = .data$V2, record_id = .data$V3, status = .data$V4,
      lat = .data$V5, long = .data$V6, max_wind = .data$V7, min_pressure = .data$V8,
      wind_34kt_radii_ne = .data$V9, wind_34kt_radii_se = .data$V10,
      wind_34kt_radii_sw = .data$V11, wind_34kt_radii_nw = .data$V12,
      wind_50kt_radii_ne = .data$V13, wind_50kt_radii_se = .data$V14,
      wind_50kt_radii_sw = .data$V15, wind_50kt_radii_nw = .data$V16,
      wind_64kt_radii_ne = .data$V17, wind_64kt_radii_se = .data$V18,
      wind_64kt_radii_sw = .data$V19, wind_64kt_radii_nw = .data$V20
    ) %>%
    # Add storm_name
    dplyr::left_join(
      hurr_meta,
      by = "storm_id"
    )

  # Clean variables ------------------------------------------

  # Trim whitespaces and some cleanup
  hurr_obs <- hurr_obs %>%
    dplyr::mutate_all(stringr::str_trim) %>%
    # Replace record_id NAs
    dplyr::mutate(record_id = replace(.data$record_id, .data$record_id == "", NA)) %>%
    # Clean numeric values and replace NAs
    dplyr::mutate_at(
      .vars = dplyr::vars(
        .data$max_wind, .data$min_pressure,
        .data$wind_34kt_radii_ne, .data$wind_34kt_radii_se,
        .data$wind_34kt_radii_sw, .data$wind_34kt_radii_nw,
        .data$wind_50kt_radii_ne, .data$wind_50kt_radii_se,
        .data$wind_50kt_radii_sw, .data$wind_50kt_radii_nw,
        .data$wind_64kt_radii_ne, .data$wind_64kt_radii_se,
        .data$wind_64kt_radii_sw, .data$wind_64kt_radii_nw
      ),
      .funs = function(x) {
        ifelse(x %in% c("-99", "-999"), NA, as.numeric(x))
      }
    )

  # Change date and time & unite them
  hurr_obs <- hurr_obs %>%
    tidyr::unite(
      col = "datetime",
      .data$date, .data$time
    ) %>%
    dplyr::mutate(
      datetime = lubridate::ymd_hm(.data$datetime),
      storm_year = lubridate::year(.data$datetime)
    )

  # Meaningful status names
  hurr_obs <- hurr_obs %>%
    dplyr::mutate(
      status = factor(.data$status,
        levels = c("TD", "TS", "HU", "EX", "SD", "SS", "LO", "WV", "DB"),
        labels = c(
          "Tropical depression", "Tropical storm", "Hurricane",
          "Extratropical cyclone", "Subtropical depression", "Subtropical storm",
          "Other low", "Tropical wave", "Disturbance"
        )
      )
    )

  # Morph coordinates
  morph_long <- function(long) {
    long <- ifelse(
      stringr::str_extract(long, "[A-Z]") == "W",
      -as.numeric(stringr::str_extract(long, "[^A-Z]+")),
      as.numeric(stringr::str_extract(long, "[^A-Z]+"))
    )
    return(long)
  }

  morph_lat <- function(lat) {
    lat <- ifelse(
      stringr::str_extract(lat, "[A-Z]") == "S",
      -as.numeric(stringr::str_extract(lat, "[^A-Z]+")),
      as.numeric(stringr::str_extract(lat, "[^A-Z]+"))
    )
    return(lat)
  }

  # Split the numeric coordinates from their directions
  hurr_obs <- hurr_obs %>%
    dplyr::mutate(
      lat_num = as.numeric(stringr::str_extract(.data$lat, "[^A-Z]+")),
      lat_hem = stringr::str_extract(.data$lat, "[A-Z]"),
      lat = morph_lat(.data$lat),
      long_num = as.numeric(stringr::str_extract(.data$long, "[^A-Z]+")),
      long_hem = stringr::str_extract(.data$long, "[A-Z]"),
      long = morph_long(.data$long)
    )

  # Clean non-standard data ----------------------------------

  # Ignore data outside the delta_t = 6 hours
  # hurr_obs <- hurr_obs %>%
  #   dplyr::filter(
  #     lubridate::hour(.data$datetime) %in% c(0, 6, 12, 18),
  #     lubridate::minute(.data$datetime) == 0
  #   ) %>%
  #   dplyr::group_by(.data$storm_id) %>%
  #   dplyr::mutate(n_obs = dplyr::n())

  # Interpolate missing wind speeds --------------------------

  if (interp_wind) {
    hurr_obs <- hurr_obs %>%
      dplyr::group_by(.data$storm_id) %>%
      dplyr::mutate(
        max_wind = ifelse(
          .data$n_obs - sum(is.na(.data$max_wind)) >= 2,
          stats::approx(.data$datetime, .data$max_wind, .data$datetime)$y,
          .data$max_wind
        )
      ) %>%
      dplyr::ungroup()
  }

  # Add useful info to data frame ----------------------------

  # Add category 5 hurricanes boolean
  # hurr_obs <- hurr_obs %>%
  #   dplyr::group_by(.data$storm_id) %>%
  #   dplyr::mutate(
  #     cat_5 = ifelse(
  #       .data$n_obs - sum(is.na(.data$max_wind)) > 0,
  #       max(.data$max_wind, na.rm = TRUE) >= 137,
  #       NA
  #     )
  #   ) %>%
  #   dplyr::ungroup()

  # Rearrange hurr_obs data frame columns
  hurr_obs <- hurr_obs %>%
    dplyr::select(
      .data$storm_id, .data$storm_name, .data$n_obs,
      .data$storm_year, .data$datetime,
      .data$status, .data$record_id,
      .data$lat, .data$lat_num, .data$lat_hem,
      .data$long, .data$long_num, .data$long_hem,
      .data$max_wind,
      # cat_5,
      dplyr::everything()
    )

  return(hurr_obs)
}
