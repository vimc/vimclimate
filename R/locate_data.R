#' Locate local data files
#'
#' @inheritParams load_local_vimc_climate
#'
#' @param country_iso3c Country ISO 3 character code. Passed from top-level
#' function [load_local_vimc_climate()] after a call to [validate_country()].
#'
#' @keywords internal data_locator
#'
#' @return The location of a local data Parquet file. Has a number of
#' side-effects to inform users when `data_location`, the county data directory
#' within `data_location`, or the file corresponding to the data requested are
#' not found.
locate_parquet_file <- function(
  data_location,
  country_iso3c,
  admin_level,
  data_source
) {
  data_location <- file.path(data_location, country_iso3c)
  assert_is_directory(data_location)

  data_source_id <- data_source_names[[data_source]]
  search_pattern <- sprintf(
    "%s_admin_%s.parquet",
    data_source_id,
    admin_level
  )

  # NOTE: we expect only a single file to be returned
  files <- list.files(
    data_location,
    pattern = search_pattern,
    full.names = TRUE
  )

  # using explicit check for more informative messages
  if (length(files) == 0L) {
    cli::cli_abort(
      "File '{search_pattern}' not found; please check that data are present in
      '{data_location}'"
    )
  }

  files
}
