# -Tests for the helper functions
library(tidyverse)
library(lubridate)

context("helpers")
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
  type <- "weather-daily"
  params <- "test_params"
  query <- construct_query(fmi_apikey, start, end, station_id, params, type)

  query_expect <- str_c(
    "http://data.fmi.fi/fmi-apikey/test_ak/wfs?request=getFeature",
    "&storedquery_id=fmi::observations::weather::daily::simple",
    "&timestep=1440&parameters=test_params&fmisid=test_id",
    "&starttime=", fmi_date_form(start), "&endtime=", fmi_date_form(end))
  expect_equal(query, query_expect)
})

test_that("construct_query works for two simple cases without apikey", {
  fmi_apikey <- NA_character_
  start <- today()
  end <- start
  station_id <- "test_id"
  type1 <- "weather-hourly"
  type2 <- "airquality"
  params <- "test_params"
  query1 <- construct_query(fmi_apikey, start, end, station_id, params, type1)
  query2 <- construct_query(fmi_apikey, start, end, station_id, params, type2)

  query_expect1 <- str_c(
    "http://opendata.fmi.fi/wfs?request=getFeature",
    "&storedquery_id=fmi::observations::weather::simple",
    "&timestep=60&parameters=test_params&fmisid=test_id",
    "&starttime=", fmi_date_form(start), "&endtime=", fmi_date_form(end))
  query_expect2 <- str_c(
    "http://opendata.fmi.fi/wfs?request=getFeature",
    "&storedquery_id=urban::observations::airquality::hourly::simple",
    "&timestep=60&parameters=test_params&fmisid=test_id",
    "&starttime=", fmi_date_form(start), "&endtime=", fmi_date_form(end))

  expect_equal(query1, query_expect1)
  expect_equal(query2, query_expect2)
})

test_that("construct_query works for multi-req daily data", {
  fmi_apikey <- "test"
  start <- today() - years(3)
  end <- today() - days(4)
  station_id <- "test"
  type <- "weather-daily"
  params <- "test"

  query <- construct_query(fmi_apikey, start, end, station_id, params, type)
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
  type <- "weather-hourly"
  params <- "test"

  query <- construct_query(fmi_apikey, start, end, station_id, params, type)
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
  query <- construct_query(NA_character_, start, end, "100971", "t2m,r_1h", "weather-hourly")
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
