#' weather_code_map
#'
#' Map weather codes (wawa, WMO code 4680) to simplified values. See codes
#' and the respective values at
#' <https://artefacts.ceda.ac.uk/badc_datadocs/surface/code.html>.
#'
#' @param weather_code The original weather code from FMI API.
#'
#' @return A character vector with the simplified values.
#'
#' @export
#'
weather_code_map <- function(weather_code) {
  case_when(weather_code == 0 ~ "clear",
            weather_code %in% 1:3 ~ "cloudy",
            weather_code == 18 ~ "squalls",
            weather_code %in% c(4:5, 10, 20, 30:35) ~ "fog",
            weather_code %in% c(21, 40:48) ~ "precipitation",
            weather_code %in% c(22, 50:58) ~ "drizzle",
            weather_code %in% c(23, 60:68, 80:84) ~ "rain",
            weather_code %in% c(11, 24, 70:78, 85:87) ~ "snow",
            weather_code %in% c(12, 90:96) ~ "snow",
            TRUE ~ NA_character_)
}
