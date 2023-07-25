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
  dplyr::case_when(
    weather_code == 0 ~ "clear",
    weather_code %in% 1:3 ~ "cloudy",
    weather_code == 18 ~ "squalls",
    weather_code %in% c(4:5, 10, 20, 30:35) ~ "fog",
    weather_code %in% c(21, 40:48) ~ "precipitation",
    weather_code %in% c(22, 50:58) ~ "drizzle",
    weather_code %in% c(23, 60:68, 80:84) ~ "rain",
    weather_code %in% c(11, 24, 70:78, 85:87) ~ "snow",
    weather_code %in% c(12, 90:96) ~ "snow",
    TRUE ~ NA_character_
  )
}

#' name_map
#'
#' Simplify (column) names to be more legible. Eg. `t2m` => `temp` or
#' `co_pt1h_avg` => `carbon_monoxide`.
#'
#' @param names Names to be simplified.
#'
#' @return A character vector with the simplified names
name_map <- function(names) {
  name_map <- c(
    "temp" = "t2m",
    "cloud_cover" = "TotalCloudCover",
    "cloud_cover" = "n_man",
    "wind_speed" = "ws_10min",
    "wind_gust" = "wg_10min",
    "wind_dir" = "wd_10min",
    "humidity" = "rh",
    "dew_point" = "td",
    "rain" = "r_1h",
    "rain_intensity" = "ri_10min",
    "snow" = "snow_aws",
    "air_pressure" = "p_sea",
    "visibility" = "vis",
    "rain" = "rrday",
    "temp" = "tday",
    "temp_min" = "tmin",
    "temp_max" = "tmax",
    "temp_ground" = "tg_pt12h_min",
    "weather_code" = "wawa",
    "airquality" = "aqindex_pt1h_avg",
    "carbon_monoxide" = "co_pt1h_avg",
    "nitrogen_dioxide" = "no2_pt1h_avg",
    "nitrogen_monoxide" = "no_pt1h_avg",
    "ozone" = "o3_pt1h_avg",
    "pm_10" = "pm10_pt1h_avg",
    "pm_25" = "pm25_pt1h_avg",
    "black_carbon" = "qbcpm25_pt1h_avg",
    "sulphur_dioxide" = "so2_pt1h_avg",
    "odorous_sulphur_compounds" = "trsc_pt1h_avg",
    "longwave" = "lwin_1min",
    "outgoing_longwave" = "lwout_1min",
    "global" = "glob_1min",
    "direct" = "dir_1min",
    "reflected" = "refl_1min",
    "sunshine" = "sund_1min",
    "diffuse" = "diff_1min",
    "balance" = "net_1min",
    "uv_irradiance" = "uvb_u"
  )
  name_map[name_map %in% names]
}
