test_that("Acessing local Parquet files works", {
  country <- "Malawi" # only supported country at present
  daterange <- lubridate::as_date(c("1990-01-01", "1990-01-10"))
  admin_level <- 0

  data <- load_local_vimc_climate(
    country,
    daterange,
    test_path("testdata"),
    "CHIRPS",
    admin_level
  )
  checkmate::expect_data_frame(data)
  expect_identical(
    unique(data$admin_unit_0),
    as.character(admin_level)
  )

  # for admin level 1
  admin_level <- 1
  data <- load_local_vimc_climate(
    country,
    daterange,
    test_path("testdata"),
    "ERA5_mean",
    admin_level
  )
  checkmate::expect_data_frame(data)

  # expect dataframe list for all climate data sources
  admin_level <- 0
  checkmate::expect_list(
    lapply(
      names(data_source_names),
      load_local_vimc_climate,
      country = country,
      date_range = daterange,
      data_location = test_path("testdata"),
      admin_level = admin_level
    ),
    "data.frame"
  )
})

# Errors and warnings
test_that("vimclimate generates expected errors and warnings", {
  daterange <- as.Date(c("1990-01-01", "1990-01-10"))
  admin_level <- 0
  # badly formed country name
  expect_error(
    load_local_vimc_climate(
      country = "MLWI",
      daterange,
      test_path("testdata"),
      "CHIRPS",
      admin_level
    ),
    "Could not convert `country`"
  )

  # data source not included
  expect_error(
    load_local_vimc_climate(
      country = "MWI",
      daterange,
      test_path("testdata"),
      "BIOCLIM",
      admin_level
    ),
    "'data_source' must be one of"
  )

  # date ranges are bad
  expect_error(
    load_local_vimc_climate(
      country = "MWI",
      date_range = c(as.POSIXct("1990-01-01"), as.POSIXct("1990-01-10")),
      test_path("testdata"),
      "PERSIANN",
      admin_level
    ),
    "Expected 'date_range' to be a Date"
  )
  expect_error(
    load_local_vimc_climate(
      country = "MWI",
      date_range = as.Date("1990-01-01"),
      test_path("testdata"),
      "PERSIANN",
      admin_level
    ),
    "Expected 'date_range' to have length 2"
  )

  # data directory or file is not available
  expect_error(
    load_local_vimc_climate(
      country = "MWI",
      daterange,
      test_path("testdata", "dummy"),
      "PERSIANN",
      admin_level
    ),
    "Directory does not exist"
  )

  expect_error(
    load_local_vimc_climate(
      country = "MWI",
      daterange,
      test_path("testdata"),
      "PERSIANN",
      admin_level = 1
    ),
    "(File)*(not found); please check that data are present"
  )
})
