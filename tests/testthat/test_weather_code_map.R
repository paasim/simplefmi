# -Tests for the function "weather_code_map"

context("weather_code_map")
test_that("weather_code_map works correctly for some example values", {
  codes <- c(0, 30, 40, 50, 60, 70, 999)
  values <- c("clear", "fog", "precipitation", "drizzle", "rain", "snow", NA_character_)
  expect_identical(weather_code_map(codes), values)
})


