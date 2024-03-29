#' fmi_weather
#'
#' Download data from the fmi weather API. If the request is larger than the
#' api allows, it is split into multiple parts. If this results to more than
#' 30 queries, a warning message is printed.
#'
#' The available variables are listed below, more information can be found from
#' <https://en.ilmatieteenlaitos.fi/open-data-manual>.
#'
#' The following variables are available for hourly data:
#' * `t2m`: temperature
#' * `ws_10min`: wind speed
#' * `wg_10min`: gust speed
#' * `wd_10min`: wind direction
#' * `n_man`: cloud cover
#' * `rh`: relative humidity
#' * `td`: dew point
#' * `r_1h`: rain amount
#' * `ri_10min`: rain intensity
#' * `snow_aws`: snow depth
#' * `p_sea`: air pressure
#' * `vis`: visibility
#' * `wawa`: weather code
#'
#' The following variables are available for daily data:
#' * `rrday`: rain
#' * `tday`: temperature
#' * `snow`: snow depth
#' * `tmin`: temperature (min)
#' * `tmax`: temperature (max)
#' * `tg_pt12h_min`: minmum ground temperature
#'
#' @param start Start time as a POSIXt-object obtained with e.g.
#'  lubridate::as_datetime().
#' @param end End time as a POSIXt-object. Defaults to `start`.
#' @param station_id The weather station id. Defaults to `100971`.
#'  (Kaisaniemi weather station). A list of the available stations can be
#'  downloaded with the [get_stations()]-function.
#' @param params Query parameters, a comma separated string, such as
#' `"tday,rrday"`. For more options, see Details.
#' @param hourly If `TRUE`, hourly data is downloaded. Otherwise the daily
#'  data is downloaded.
#' @param simplify_names If `TRUE`, variable names are simplified (eg. `t2m` is
#' converted to `temp`).
#' @param verbose If `TRUE`, prints the progress. Defaults to `FALSE`.
#' @param fmi_apikey An optional fmi-apikey.
#'  See <http://en.ilmatieteenlaitos.fi/open-data-manual>.
#'
#' @return A tibble with date (& time if hourly data is requested) in the first
#'  column and the variables specified in `params` in the other columns.
#'
#' @export
fmi_weather <- function(start,
                        end = start,
                        station_id = "100971",
                        params = ifelse(hourly, "t2m,r_1h", "tday,rrday"),
                        hourly = FALSE,
                        simplify_names = TRUE,
                        verbose = FALSE,
                        fmi_apikey = NA_character_) {
  res <- construct_query(
    fmi_apikey, start, end, station_id, params,
    if (hourly) "weather-hourly" else "weather-daily"
  ) |>
    fmi_get(simplify_names, verbose)

  # remove time information
  if (!hourly) res$date <- lubridate::as_date(res$date)

  res
}


#' fmi_airquality
#'
#' Download data from the fmi (urban) airquality API. If the request is larger
#' than the api allows, it is split into multiple parts. If this results to more
#' than 30 queries, a warning message is printed.
#'
#' The available variables are listed below, more information can be found from
#' <https://en.ilmatieteenlaitos.fi/open-data-manual>.
#'
#' The following variables are available:
#' * `aqindex_pt1h_avg`: air quality index
#' * `co_pt1h_avg`: carbon monoxide
#' * `no2_pt1h_avg`: nitrogen dioxide
#' * `no_pt1h_avg`: nitrogen monoxide
#' * `o3_pt1h_avg`: ozone
#' * `pm10_pt1h_avg`: particulate matter < 10 nm
#' * `pm25_pt1h_avg`: particulate matter < 2.5 nm
#' * `qbcpm25_pt1h_avg`: black carbon pm2.5
#' * `so2_pt1h_avg`: sulphur dioxide
#' * `trsc_pt1h_avg`: odorous sulphur compounds
#'
#' @param start Start time as a POSIXt-object obtained with e.g.
#'  lubridate::as_datetime().
#' @param end End time as a POSIXt-object. Defaults to `start`.
#' @param station_id The weather station id. Defaults to `100742`.
#'  (Helsinki Mannerheimintie air quality station). A list of the available
#'  stations can be downloaded with the [get_stations()]-function.
#' @param params Query parameters, a comma separated string, such as
#' `"pm10_pt1h_avg,pm25_pt1h_avg"`. For more options, see Details.
#' @param simplify_names If `TRUE`, variable names are simplified (eg.
#' `aqindex_pt1h_avg` is converted to `airquality`).
#' @param verbose If `TRUE`, prints the progress. Defaults to `FALSE`.
#' @param fmi_apikey An optional fmi-apikey.
#'  See <http://en.ilmatieteenlaitos.fi/open-data-manual>.
#'
#' @return A tibble with time in the first
#'  column and the variables specified in `params` in the other columns.
#'
#' @export
fmi_airquality <- function(start,
                           end = start,
                           station_id = "100742",
                           params = "aqindex_pt1h_avg",
                           simplify_names = TRUE,
                           verbose = FALSE,
                           fmi_apikey = NA_character_) {
  construct_query(
    fmi_apikey, start, end, station_id, params, "airquality"
  ) |>
    fmi_get(simplify_names, verbose)
}


#' fmi_radiation
#'
#' Download radiation data from the fmi radiation API. If the request is larger
#' than the api allows, it is split into multiple parts. If this results to more
#' than 30 queries, a warning message is printed. *Note*, this might be for
#' the first minute of the hour as it works currently, rather than some kind
#' of averages!
#'
#' The available variables are listed below, more information can be found from
#' <https://en.ilmatieteenlaitos.fi/open-data-manual>.
#'
#' The following variables are available:
#' * `lwin_1min`: long-wave solar radiation (W/m2)
#' * `lwout_1min`: outgoing long-wave solar radiation (W/m2)
#' * `glob_1min`: global radiation (W/m2)
#' * `dir_1min`: direct solar radiation (W/m2)
#' * `refl_1min`: reflected radiation (W/m2)
#' * `sund_1min`: sunshine duration (s)
#' * `diff_1min`: diffuse radiation (W/m2)
#' * `net_1min`: radiation balance (W/m2)
#' * `uvb_u`: ultraviolet irradiance (index)
#'
#' @param start Start time as a POSIXt-object obtained with e.g.
#'  lubridate::as_datetime().
#' @param end End time as a POSIXt-object. Defaults to `start`.
#' @param station_id The station id. Defaults to `101004`.
#'  (Kumpula station). A list of the available
#'  stations can be downloaded with the [get_stations()]-function.
#' @param params Query parameters, a comma separated string, such as
#' `"dir_1min,diff_1min"`. For more options, see Details.
#' @param simplify_names If `TRUE`, variable names are simplified (eg.
#' `GLOB_1MIN` is converted to `global_radiation`).
#' @param verbose If `TRUE`, prints the progress. Defaults to `FALSE`.
#' @param fmi_apikey An optional fmi-apikey.
#'  See <http://en.ilmatieteenlaitos.fi/open-data-manual>.
#'
#' @return A tibble with time in the first
#'  column and the variables specified in `params` in the other columns.
#'
#' @export
fmi_radiation <- function(start,
                          end = start,
                          station_id = "101004",
                          params = "dir_1min,diff_1min",
                          simplify_names = TRUE,
                          verbose = FALSE,
                          fmi_apikey = NA_character_) {
  construct_query(
    fmi_apikey, start, end, station_id, params, "radiation"
  ) |>
    fmi_get(simplify_names, verbose)
}
