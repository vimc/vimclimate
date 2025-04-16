#' Get climate data prepared for VIMC and stored locally
#'
#' @description
#' Load climate data prepared for VIMC from a local directory.
#'
#'
#' @param country A string for the country name, or for the country ISO 2- or 3-
#' character code. E.g. one of "Canada", "CA", or "CAN". Input is checked
#' against names and codes in \pkg{countrycode}.
#'
#' @param date_range A two element vector of `<Date>`s, giving the interval for
#' which to get data.
#'
#' @param data_location A location relative to the current working directory
#' to search for the data. Data for `country` are expected to live in
#' `data_location/XYZ` as Parquet files, where `XYZ` is the ISO 3-character code
#' for `country`. Defaults to the working directory.
#'
#' @param data_source A string giving the climate data to search for. Names must
#' match exactly.
#'
#' @param admin_level A numeric giving the GADM admin level for which to get
#' data. Only one unit may be passed at a time.
#'
#' @return A `<data.frame>` of climate data.
#'
#' @keywords data_access
#'
#' @export
load_local_vimc_climate <- function(
  country,
  date_range,
  data_location = here::here(),
  data_source = c(
    "CHIRPS",
    "ERA5_mean",
    "ERA5_min",
    "ERA5_max",
    "ERA5_RH",
    "ERA5_SH",
    "PERSIANN"
  ),
  admin_level = c(0, 1, 2, 3)
) {
  country <- validate_country(country)
  date_range <- validate_date_range(date_range)
  data_source <- rlang::arg_match(data_source, multiple = FALSE)
  admin_level <- validate_admin_level(admin_level)

  target_file <- locate_parquet_file(
    data_location,
    country,
    admin_level,
    data_source
  )

  # read files and return data.frame as being of familiar format
  data <- arrow::read_parquet(target_file)
  data <- as.data.frame(data)

  # filter on date, assume timezone is UTC
  data$Date <- lubridate::as_date(data$Date)

  data[
    data$Date >= min(date_range) & data$Date <= max(date_range),
  ]
}
