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
  checkmate::assert_directory_exists(
    data_location
  )
  data_location <- file.path(data_location, country_iso3c)
  checkmate::assert_directory_exists(
    data_location
  )

  files <- list.files(data_location, full.names = TRUE)
  data_source_id <- data_source_names[data_source]
  search_pattern <- sprintf(
    "%s_admin_%s.parquet",
    data_source_id,
    admin_level
  )

  # search for files
  target_file <- files[grepl(search_pattern, files)]

  # check that file is found
  if (identical(target_file, character(0))) {
    cli::cli_abort(
      "File '{target_file}' not found; please check that data are present in
      '{data_location}'"
    )
  }

  target_file
}
