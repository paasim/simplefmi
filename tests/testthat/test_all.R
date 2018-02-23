# -Tests for the function "fmi_download"
# -Tests for the function "get_stations"

context("get_stations")
test_that("get_stations returns correctly named columns and positive number of rows", {
  df_stations <- get_stations()
  expect_true("tbl_df" %in% class(df_stations))
  expect_true(nrow(df_stations) > 0)
  expect_identical(colnames(df_stations), c("name", "station_id", "lat", "lon"))
})

context("fmi_download")
test_that("construct_query works for a simple case", {
  fmi_apikey <- "test_ak"
  start <- today() - hours(1)
  end <- start
  station_id <- "test_id"
  hourly <- FALSE
  params <- "test_params"
  query <- construct_query(fmi_apikey, start, end, station_id, params, hourly)

  query_expect <- str_c(
    "http://data.fmi.fi/fmi-apikey/test_ak/wfs?request=getFeature",
    "&storedquery_id=fmi::observations::weather::daily::simple",
    "&timestep=1440&parameters=test_params&fmisid=test_id",
    "&starttime=", fmi_date_form(start), "&endtime=", fmi_date_form(end))
  expect_equal(query, query_expect)
})

test_that("construct_query works for multi-req daily data", {
  fmi_apikey <- "test"
  start <- today() - years(3)
  end <- today() - days(4)
  station_id <- "test"
  hourly <- FALSE
  params <- "test"

  query <- construct_query(fmi_apikey, start, end, station_id, params, hourly)
  expect_length(query, 3)

  start_exp <- str_c("&starttime=", fmi_date_form(start + years(0:2)))
  end_exp <- str_c("&endtime=",
                   fmi_date_form(c(start + years(1:2) - days(1), end)))

  for (i in 1:3) {
    expect_true(str_detect(query[i], "&timestep=1440"))
    expect_true(str_detect(query[i], start_exp[i]))
    expect_true(str_detect(query[i], end_exp[i]))
  }
})

test_that("construct_query works for multi-req hourly data", {
  fmi_apikey <- "test"
  start <- today() - weeks(3)
  end <- today() - hours(4)
  station_id <- "test"
  hourly <- TRUE
  params <- "test"

  query <- construct_query(fmi_apikey, start, end, station_id, params, hourly)
  expect_length(query, 3)

  start_exp <- str_c("&starttime=", fmi_date_form(start + weeks(0:2)))
  end_exp <- str_c("&endtime=",
                   fmi_date_form(c(start + weeks(1:2) - hours(1), end)))

  for (i in 1:3) {
    expect_true(str_detect(query[i], "&timestep=60"))
    expect_true(str_detect(query[i], start_exp[i]))
    expect_true(str_detect(query[i], end_exp[i]))
  }
})

test_that("process_content seems to work as expected", {
  df1 <- process_content(res_list_test)
  expect_true("tbl_df" %in% class(df1))
  expect_equal(nrow(df1), 24)
  expect_equal(ncol(df1), 3)
  expect_equal(round(mean(df1$t2m)), -12L)
  expect_equal(median(df1$date), ymd_hms("2018-02-22 11:30:00 UTC"))

})

test_that("simplify_colnames seems to work as expected", {
  tb1 <- simplify_colnames(tibble(t2m = 1, vis = 2))
  tb2 <- tibble(temp = 1, visibility = 2)
  expect_identical(tb1, tb2)
})

test_that("report_errors passes the error to R", {
  expect_error(fmi_download("fake_id", today()), regexp = "apikey")
})
