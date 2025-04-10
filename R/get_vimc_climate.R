#' Get climate data Parquets prepared for VIMC
#'
#' @param country A string for the country name, or for the country ISO 2- or 3-
#' character code. E.g. one of "Canada", "CA", or "CAN". Input is checked
#' against names and codes in \pkg{countrycode}.
#' @param date_range A two element vector of either strings or `<Date>`s, giving
#' the interval for which to get data. If a character vector, must be in a
#' format that can be converted to a `<Date>` by [lubridate::as_date()].
#' @param data_location A location relative to the current working directory
#' to search for the data. Data for `country` are expected to live in
#' `data_location/XYZ` as Parquet files, where `XYZ` is the ISO 3-character code
#' for `country`. Defaults to the working directory.
#' @param data_source A string giving the climate data to search for. Names must
#' match exactly.
#' @param admin_level A numeric giving the GADM admin level for which to get
#' data. Only one unit may be passed at a time.
#'
#' @return A `<data.frame>` of climate data.
#'
#' @export
get_vimc_climate <- function(
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
  # more checks below
  checkmate::assert_string(
    country,
    min.chars = 2
  )

  # allow date as string because actually using Date class is annoying
  if (!checkmate::test_vector(date_range, len = 2)) {
    cli::cli_abort(
      "`date_range` must be a 2-element vector; currently has a length of
      length(date_range)"
    )
  }
  is_good_daterange <- checkmate::test_date(
    date_range,
    any.missing = FALSE
  ) ||
    checkmate::test_character(
      date_range
    )
  if (!is_good_daterange) {
    cli::cli_abort(
      "`date_range` must be either a `<Date>` or `string` vector, but is not."
    )
  } else if (is.character(date_range)) {
    # convert to Date
    date_range <- lubridate::as_date(date_range)
  }

  # check for sensible date values and throw a warning if not good
  # this will need to be fixed later to date ranges for which climate data
  # are actually available
  sensible_limits <- lubridate::interval("1950-01-01", "2100-12-31")
  if (!all(lubridate::`%within%`(date_range, sensible_limits))) {
    cli::cli_warn(
      "`date_range` requested is outside of the range {sensible_limits}"
    )
  }

  # allow only one choice at a time to avoid ambiguous return type
  data_source <- rlang::arg_match(data_source)
  admin_level <- as.character(admin_level)
  admin_level <- rlang::arg_match(admin_level, allowed_admin_levels)

  # convert country to ISO 3166-1 3C country code
  # note `nomatch = NULL` is preferable but is a special case
  # in pkg countrycode
  nchar_country <- as.character(nchar(country))
  country_iso3c <- switch(
    nchar_country,
    "2" = countrycode::countrycode(
      country,
      "iso2c",
      "iso3c",
      warn = FALSE,
      nomatch = NA
    ),
    "3" = countrycode::countrycode(
      country,
      "iso3c",
      "iso3c",
      warn = FALSE,
      nomatch = NA
    ),
    countrycode::countryname(
      country,
      "iso3c",
      warn = FALSE,
      nomatch = NA
    )
  )

  if (is.na(country_iso3c)) {
    cli::cli_abort(
      "Could not convert `country` '{country}' to an ISO 3 character
      country code; please check `country`."
    )
  }

  # abs path to data; currently only supporting local data
  data_location <- here::here(data_location, country_iso3c)
  if (dir.exists(data_location)) {
    files <- list.files(data_location, full.names = TRUE)
    data_source_id <- data_source_names[data_source]
    search_pattern <- sprintf(
      "%s_admin_%s.parquet",
      data_source_id,
      admin_level
    )

    # search for files
    target_file <- files[grepl(search_pattern, files)]

    # read files and return data.frame
    data <- arrow::read_parquet(target_file)
    data <- as.data.frame(data)

    # filter on date, assume timezone is UTC
    data$Date <- lubridate::as_date(data$Date)

    data[
      lubridate::`%within%`(
        data$Date,
        lubridate::interval(min(date_range), max(date_range))
      ),
    ]
  } else {
    cli::cli_abort(
      "Could not find a data directory for '{country}'
      in '{data_location}'; please check `country` and `data_location`."
    )
  }
}
