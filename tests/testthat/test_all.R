# -Tests for the function "fmi_download"
# -Tests for the function "get_stations"
library(tidyverse)
library(lubridate)

context("get_stations")
test_that("get_stations returns correctly named columns and positive number of rows", {
  df_stations <- get_stations()
  expect_true("tbl_df" %in% class(df_stations))
  expect_true(nrow(df_stations) > 0)
  expect_identical(colnames(df_stations), c("name", "station_id", "lat", "lon"))
})

context("fmi_download")

test_that("mk_time_seq fails when end > start", {
  expect_error(mk_time_seq(today(), today() - days(1), TRUE))
})

test_that("mk_time_works when end == start and hourly = FALSE", {
  tbl1 <- mk_time_seq(today(), today(), FALSE)
  expect_equal(tbl1, tibble(start = today(), end = today()))
})

test_that("mk_time_works when the result does not need to be splitted", {
  tbl1 <- mk_time_seq(today() - days(5), today(), TRUE)
  expect_equal(nrow(tbl1), 1L)
})

test_that("mk_time_works when the result needs to be splitted", {
  # difference of start and end not a multiple of diff
  n <- 3L
  tbl1 <- mk_time_seq(today() - days(7L*n+1L), today(), TRUE)
  expect_equal(nrow(tbl1), n+1L)

  # difference of start and end multiple of diff
  tbl1 <- mk_time_seq(today() - days(7*n), today(), TRUE)
  expect_equal(nrow(tbl1), n)
})

test_that("construct_query works for a simple case with apikey", {
  fmi_apikey <- "test_ak"
  start <- today()
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

test_that("construct_query works for a simple case without apikey", {
  fmi_apikey <- NA_character_
  start <- today()
  end <- start
  station_id <- "test_id"
  hourly <- FALSE
  params <- "test_params"
  query <- construct_query(fmi_apikey, start, end, station_id, params, hourly)

  query_expect <- str_c(
    "http://opendata.fmi.fi/wfs?request=getFeature",
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
  start <- ymd_hms("2019-02-10 00:00:00")
  end <- ymd_hms("2019-03-02T20:00:00Z")
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
  yesterday <- today() - days(1)
  start <- str_c(yesterday, " 00:00:00")
  end <- str_c(yesterday, " 23:00:00")
  query <- construct_query(NA_character_, start, end, "100971", "t2m,r_1h", TRUE)
  res <- map(query, GET)
  df1 <- map_df(res, process_content)
  expect_true(is_tibble(df1))
  expect_equal(nrow(df1), 24)
  expect_equal(ncol(df1), 3)
  expect_equal(median(df1$date), ymd_hms(str_c(yesterday, "11:30:00 UTC")))
})

test_that("simplify_colnames seems to work as expected", {
  tb1 <- simplify_colnames(tibble(tday = 1, vis = 2))
  tb2 <- tibble(temp = 1, visibility = 2)
  expect_identical(tb1, tb2)
})

test_that("the entire function returns something sensible for yesterday", {
  yesterday <- today() - days(1)
  result <- fmi_download(yesterday)
  expect_equal(slice(result, 0),
               tibble(date = as_date(character(0)),
                      rain = double(0),
                      temp = double(0)))
  expect_equal(result$date, yesterday)
})

test_that("report_errors passes the error to R", {
  expect_error(fmi_download(today(), fmi_apikey = "fake_key"), regexp = "apikey")
})
