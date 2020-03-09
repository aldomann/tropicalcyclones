#' Title
#'
#' @param con a connection object or a character string. This is the file path or URL for the HURDAT2 dataset to read.
#'
#' @return Hurricane observations dataframe
#' @export
read_hurdat2 <- function(con) {
  # Read and split raw data ----------------------------------

  # tracks_url <- paste0("http://www.aoml.noaa.gov/hrd/hurdat/", "hurdat2-nepac-1949-2016-apr2017.txt")
  # tracks_url <- paste0("http://www.aoml.noaa.gov/hrd/hurdat/", "hurdat2-1851-2016-apr2017.txt")

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
    dplyr::select(-V4) %>%
    dplyr::rename(storm_id = V1, storm_name = V2, n_obs = V3) %>%
    dplyr::mutate(
      storm_name = stringr::str_trim(storm_name),
      n_obs = as.numeric(n_obs)
    )

  storm_ids <- rep(hurr_meta$storm_id, times = hurr_meta$n_obs)

  # Create and clean obs data frame
  hurr_obs <- lapply(hurr_obs_raw, tibble::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(storm_id = storm_ids) %>%
    dplyr::select(storm_id, V1:V20) %>%
    dplyr::rename(
      date = V1, time = V2, record_id = V3, status = V4, lat = V5, long = V6, max_wind = V7, min_pressure = V8,
      wind_34kt_radii_ne = V9, wind_34kt_radii_se = V10, wind_34kt_radii_sw = V11, wind_34kt_radii_nw = V12,
      wind_50kt_radii_ne = V13, wind_50kt_radii_se = V14, wind_50kt_radii_sw = V15, wind_50kt_radii_nw = V16,
      wind_64kt_radii_ne = V17, wind_64kt_radii_se = V18, wind_64kt_radii_sw = V19, wind_64kt_radii_nw = V20
    )

  # Trim whitespaces some cleanup
  hurr_obs <- hurr_obs %>%
    dplyr::mutate_all(stringr::str_trim) %>%
    # Replace record_id NAs
    dplyr::mutate(record_id = replace(record_id, record_id == "", NA)) %>%
    # Clean numeric values and replace NAs
    dplyr::mutate(max_wind = ifelse(max_wind == "-99", NA, as.numeric(max_wind))) %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(
        min_pressure,
        wind_34kt_radii_ne, wind_34kt_radii_se, wind_34kt_radii_sw, wind_34kt_radii_nw,
        wind_50kt_radii_ne, wind_50kt_radii_se, wind_50kt_radii_sw, wind_50kt_radii_nw,
        wind_64kt_radii_ne, wind_64kt_radii_se, wind_64kt_radii_sw, wind_64kt_radii_nw
      ),
      .funs = function(x) {
        ifelse(x == "-999", NA, as.numeric(x))
      }
    )


  # Change date and time & unite them
  hurr_obs <- hurr_obs %>%
    tidyr::unite(datetime, date, time) %>%
    dplyr::mutate(datetime = lubridate::ymd_hm(datetime))

  # Meaningful status names
  hurr_obs <- hurr_obs %>%
    dplyr::mutate(
      status = factor(status,
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
      lat_num = as.numeric(stringr::str_extract(lat, "[^A-Z]+")),
      lat_hem = stringr::str_extract(lat, "[A-Z]"),
      lat = morph_lat(lat),
      long_num = as.numeric(stringr::str_extract(long, "[^A-Z]+")),
      long_hem = stringr::str_extract(long, "[A-Z]"),
      long = morph_long(long)
    )

  # Clean non-standard data ----------------------------------

  # Ignore data outside the delta_t = 6 hours
  hurr_obs <- hurr_obs %>%
    dplyr::filter(
      lubridate::hour(datetime) %in% c(0, 6, 12, 18),
      lubridate::minute(datetime) == 0
    )

  # Clean up wind column -------------------------------------

  # Manually change odd middle values for AL191976 & AL111973
  hurr_obs <- hurr_obs %>%
    dplyr::mutate(
      max_wind = ifelse(storm_id == "AL191976" & is.na(max_wind), 20, max_wind),
      max_wind = ifelse(storm_id == "AL111973" & is.na(max_wind), 30, max_wind),
      max_wind = ifelse(storm_id == "AL111973" & lubridate::month(datetime) == 9 &
        lubridate::day(datetime) == 12 & lubridate::hour(datetime) == 12, NA, max_wind)
    ) %>%
    dplyr::filter(!is.na(max_wind))

  # Add useful info to data frame ----------------------------

  # Add category 5 hurricanes boolean
  hurr_obs <- hurr_obs %>%
    dplyr::group_by(storm_id) %>%
    dplyr::mutate(cat_5 = max(max_wind) >= 137) %>%
    dplyr::ungroup()

  # Add storm_name and storm_year to hurr_obs
  hurr_obs <- hurr_obs %>%
    dplyr::left_join(
      hurr_meta,
      by = "storm_id"
    ) %>%
    dplyr::mutate(storm_year = lubridate::year(datetime))

  # Recalculate n_obs
  hurr_obs <- hurr_obs %>%
    dplyr::group_by(storm_id) %>%
    dplyr::mutate(n_obs = dplyr::n())

  # Rearrange hurr_obs data frame columns
  hurr_obs <- hurr_obs %>%
    dplyr::select(
      storm_id, storm_name, n_obs, storm_year, datetime,
      status, record_id,
      lat, lat_num, lat_hem, long, long_num, long_hem,
      max_wind, cat_5, dplyr::everything()
    )

  return(hurr_obs)
}
