#' @title Return checked inputs or print informative errors
#'
#' @name input_validators
#' @rdname input_validators
#'
#' @inheritParams load_local_vimc_climate
#'
#' @keywords internal validator
#'
#' @return
#' A checked and in some cases processed input. Most functions also have
#' side-effects.
#'
#' - `validate_country()`: A country ISO 3 character code. Has the side-effect
#' of erroring when `country` is either not a string, or when the country code
#' cannot be found from `country`.
#'
#' - `validate_date_range()`: Returns `date_range` after checking that it is
#' a two-element `<Date>` vector.
#'
#' - `validate_admin_level()`: Returns `admin_level` after checking it is an
#' integer-like number in the range `[0, 3]`.
validate_country <- function(country) {
  # convert country to ISO 3166-1 3C country code
  # note `nomatch = NULL` is preferable but is a special case
  # in pkg countrycode
  assert_scalar_character(country)
  nchar_country <- nchar(country)
  checkmate::assert_integerish(
    nchar_country,
    lower = 2,
    any.missing = FALSE
  )

  if (nchar_country > 3L) {
    country_iso3c <- countrycode::countryname(
      country,
      "iso3c",
      warn = FALSE,
      nomatch = NA
    )
  } else if (nchar_country %in% c(2L, 3L)) {
    country_iso3c <- countrycode::countrycode(
      country,
      sprintf("iso%ic", nchar_country),
      "iso3c",
      warn = FALSE,
      nomatch = NA
    )
  }

  if (is.na(country_iso3c)) {
    cli::cli_abort(
      "Could not convert `country` '{country}' to an ISO 3 character \\
      country code; please check `country`."
    )
  }

  country_iso3c
}

#' @name input_validators
#'
#' @inheritParams date_range
#'
#' @keywords internal validator
validate_date_range <- function(date_range) {
  assert_length(date_range, 2)
  assert_date(date_range)
}

#' @name input_validators
#'
#' @inheritParams load_local_vimc_climate
#'
#' @keywords internal validator
validate_admin_level <- function(admin_level) {
  checkmate::assert_integerish(
    admin_level,
    lower = 0,
    upper = 3,
    len = 1,
    any.missing = FALSE
  )
}

#' @name input_validators
#'
#' @inheritParams load_local_vimc_climate
#'
#' @keywords internal validator
validate_data_location <- function(data_location) {
  assert_scalar(data_location)
  assert_is_directory(data_location)
  invisible(data_location) # assert_is_directory() has no return
}
