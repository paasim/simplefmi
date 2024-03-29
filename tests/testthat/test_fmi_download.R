# -Tests for the functions "fmi_weather" and "fmi_airquality"
library(tidyverse)
library(lubridate)

context("fmi_weather")
test_that("report_errors passes the error to R", {
  expect_error(fmi_weather(today(), fmi_apikey = "fake_key"), regexp = "apikey")
})

test_that("fmi_weather returns something sensible for yesterday", {
  yesterday <- today() - days(1)
  result <- fmi_weather(yesterday)
  expect_equal(nrow(result), 1)
  expect_equal(slice(result, 0),
               tibble(date = as_date(character(0)),
                      rain = double(0),
                      temp = double(0)))
  expect_equal(result$date, yesterday)
})

context("fmi_airquality")
test_that("fmi_airquality returns something sensible for yesterday", {
  yesterday <- floor_date(now("UTC"), "days") - days(1)
  result <- fmi_airquality(yesterday)
  expect_equal(nrow(result), 1)
  tbl0 <- tibble(date = as_datetime(character(0)),
                 airquality = double(0))
  expect_equal(slice(result, 0), tbl0)
  expect_equal(result$date, yesterday)
})


context("fmi_radiation")
test_that("fmi_radiation returns something sensible for yesterday", {
  yesterday <- floor_date(now("UTC"), "days") - days(1)
  result <- fmi_radiation(yesterday)
  expect_equal(nrow(result), 1)
  tbl0 <- tibble(date = as_datetime(character(0)),
                 diffuse = double(0), direct = double(0))
  expect_equal(slice(result, 0), tbl0)
  expect_equal(result$date, yesterday)
})

