# -Tests for the function "get_stations"

context("get_stations")
test_that("get_stations returns correctly named columns and positive number of rows", {
  df_stations <- get_stations()
  expect_true("tbl_df" %in% class(df_stations))
  expect_true(nrow(df_stations) > 0)
  expect_true(all(c("name", "station_id", "lat", "lon") %in% colnames(df_stations)))
})


