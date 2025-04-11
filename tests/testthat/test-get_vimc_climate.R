test_that("Acessing local Parquet files works", {
  country <- "Malawi" # only supported country at present
  daterange <- c("1990-01-01", "1990-01-10")
  admin_level <- 0

  expect_no_condition(
    get_vimc_climate(
      country,
      daterange,
      test_path("testdata"),
      "CHIRPS",
      admin_level
    )
  )

  data <- get_vimc_climate(
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
  data <- get_vimc_climate(
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
      get_vimc_climate,
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
  daterange <- c("1990-01-01", "1990-01-10")
  admin_level <- 0
  # badly formed country name
  expect_error(
    get_vimc_climate(
      country = "MLWI",
      daterange,
      test_path("testdata"),
      "CHIRPS",
      admin_level
    )
  )

  # data source not included
  expect_error(
    get_vimc_climate(
      country = "MWI",
      daterange,
      test_path("testdata"),
      "BIOCLIM",
      admin_level
    )
  )

  # date ranges are bad
  expect_error(
    get_vimc_climate(
      country = "MWI",
      date_range = c(as.POSIXct("1990-01-01"), as.POSIXct("1990-01-10")),
      test_path("testdata"),
      "PERSIANN",
      admin_level
    )
  )
  expect_error(
    get_vimc_climate(
      country = "MWI",
      date_range = c(as.Date("1990-01-01")),
      test_path("testdata"),
      "PERSIANN",
      admin_level
    )
  )

  # data directory or file is not available
  expect_error(
    get_vimc_climate(
      country = "MWI",
      daterange,
      test_path("testdata", "dummy"),
      "PERSIANN",
      admin_level
    )
  )

  expect_error(
    get_vimc_climate(
      country = "MWI",
      daterange,
      test_path("testdata"),
      "PERSIANN",
      admin_level = 2
    )
  )
})
