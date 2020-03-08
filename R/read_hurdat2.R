#' Title
#'
#' @param filename HURDAT2 file path
#'
#' @return Hurricane observations dataframe
#' @export
read_hurdat2 <- function(filename) {
  # Read and split raw data ----------------------------------

  # tracks_url <- paste0("http://www.aoml.noaa.gov/hrd/hurdat/", "hurdat2-nepac-1949-2016-apr2017.txt")
  # tracks_url <- paste0("http://www.aoml.noaa.gov/hrd/hurdat/", "hurdat2-1851-2016-apr2017.txt")
  tracks_file <- filename

  hurr_tracks <- readLines(tracks_file)
  hurr_tracks <- lapply(hurr_tracks, stringr::str_split, pattern = ",", simplify = TRUE)

  # Clean the raw data ---------------------------------------

  # Split the hurr_tracks into meta and observation lists
  hurr_lengths <- sapply(hurr_tracks, length)
  hurr_meta <- hurr_tracks[hurr_lengths == 4]
  hurr_obs <- hurr_tracks[hurr_lengths == 21]

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

  storm_id <- rep(hurr_meta$storm_id, times = hurr_meta$n_obs)

  # Create and clean obs data frame
  hurr_obs <- lapply(hurr_obs, tibble::as_tibble)
  hurr_obs <- dplyr::bind_rows(hurr_obs) %>%
    dplyr::mutate(storm_id = storm_id) %>%
    dplyr::select(storm_id, V1:V7) %>%
    dplyr::rename(date = V1, time = V2, record_id = V3, status = V4, lat = V5, long = V6, wind = V7)

  # Change date and time & unite them
  hurr_obs <- hurr_obs %>%
    tidyr::unite(datetime, date, time) %>%
    dplyr::mutate(datetime = lubridate::ymd_hm(datetime))

  # Meaningful status names
  storm_levels <- c("TD", "TS", "HU", "EX", "SD", "SS", "LO", "WV", "DB")
  storm_labels <- c(
    "Tropical depression", "Tropical storm", "Hurricane",
    "Extratropical cyclone", "Subtropical depression", "Subtropical storm",
    "Other low", "Tropical wave", "Disturbance"
  )
  hurr_obs <- hurr_obs %>%
    dplyr::mutate(
      status = factor(stringr::str_trim(status),
        levels = storm_levels,
        labels = storm_labels
      )
    )

  # Clean record identifier
  hurr_obs <- hurr_obs %>%
    dplyr::mutate(record_id = gsub("\\s+", "", record_id)) %>%
    dplyr::mutate(record_id = replace(record_id, record_id == "", NA))

  # Morph coordinates
  morph_long <- function(long) {
    long <- ifelse(stringr::str_extract(long, "[A-Z]") == "W",
      -as.numeric(stringr::str_extract(long, "[^A-Z]+")),
      as.numeric(stringr::str_extract(long, "[^A-Z]+"))
    )
    return(long)
  }
  morph_lat <- function(lat) {
    lat <- ifelse(stringr::str_extract(lat, "[A-Z]") == "S",
      -as.numeric(stringr::str_extract(lat, "[^A-Z]+")),
      as.numeric(stringr::str_extract(lat, "[^A-Z]+"))
    )
    return(lat)
  }

  # Split the numeric coordinates from their directions
  hurr_obs <- hurr_obs %>%
    dplyr::mutate(
      lat_num = as.numeric(stringr::str_extract(lat, "[^A-Z]+")),
      lat_dir = stringr::str_extract(lat, "[A-Z]"),
      lat = morph_lat(lat),
      long_num = as.numeric(stringr::str_extract(long, "[^A-Z]+")),
      long_dir = stringr::str_extract(long, "[A-Z]"),
      long = morph_long(long)
    )

  # Clean non-standard data ----------------------------------

  # Ignore data outside the delta_t = 6 hours
  hurr_obs <- hurr_obs %>%
    dplyr::filter(
      lubridate::hour(datetime) == 00 |
      lubridate::hour(datetime) == 06 |
      lubridate::hour(datetime) == 12 |
      lubridate::hour(datetime) == 18
    ) %>%
    dplyr::filter(
      lubridate::minute(datetime) == 00
    )

  # Clean up wind column -------------------------------------

  # Manually change odd middle values for AL191976 & AL111973
  hurr_obs <- hurr_obs %>%
    dplyr::mutate(
      wind = ifelse(storm_id == "AL191976" & wind == " -99", 20, wind),
      wind = ifelse(storm_id == "AL111973" & wind == " -99", 30, wind),
      wind = ifelse(storm_id == "AL111973" & lubridate::month(datetime) == 9 &
        lubridate::day(datetime) == 12 & lubridate::hour(datetime) == 12, NA, wind)
    ) %>%
    dplyr::filter(is.na(wind) != TRUE)

  # Clean and reformat the wind column
  hurr_obs <- hurr_obs %>%
    dplyr::mutate(wind = ifelse(wind == " -99", NA, as.numeric(wind)))

  # Add useful info to data frame ----------------------------

  # Add category 5 hurricanes boolean
  # hurr_obs <- hurr_obs %>%
  # 	group_by(storm_id) %>%
  # 	mutate(cat_5 = max(wind) >= 137) %>%
  # 	ungroup()

  # Add storm_name and storm_year to hurr_obs
  hurr_obs <- hurr_obs %>%
    dplyr::left_join(hurr_meta, by = "storm_id") %>%
    dplyr::mutate(storm_year = lubridate::year(datetime))

  # Recalculate n_obs
  hurr_obs <- hurr_obs %>%
    dplyr::group_by(storm_id) %>%
    dplyr::mutate(n_obs = length(wind))

  # Rearrange hurr_obs data frame columns
  hurr_obs <- hurr_obs[c(
    "storm_id", "storm_name", "n_obs", "datetime",
    "status", "record_id", "lat", "long",
    "wind", "storm_year"
  )]
  # Unused variables
  # 	"delta_t" after "datetime"
  # 	"cat_5" after "wind"

  return(hurr_obs)
}
