#' Climate data source names
#' @keywords internal constants
data_source_names <- c(
  CHIRPS = "chirps",
  ERA5_mean = "era5mean",
  ERA5_min = "era5min",
  ERA5_max = "era5max",
  ERA5_RH = "era5rh",
  ERA5_SH = "era5sh",
  PERSIANN = "persiann"
)

#' Allowed GADM admin levels
#' @keywords internal constants
allowed_admin_levels <- as.character(seq(0, 3))
