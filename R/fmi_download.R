#' fmi_download
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
#' * `TotalCloudCover`: cloud cover
#' * `rh`: humidity
#' * `td`: dew point
#' * `r_1h`: rain
#' * `snow_aws`: snow
#' * `p_sea`: air pressure
#' * `vis`: visibility
#'
#' The following variables are available for daily data:
#' * `rrday`: rain
#' * `tday`: temperature
#' * `tmin`: temperature (min)
#' * `tmax`: temperature (max)
#' * `TG_PT12H_min`: minmum ground temperature
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
fmi_download <- function(start,
                         end = start,
                         station_id = "100971",
                         params = ifelse(hourly, "t2m,r_1h", "tday,rrday"),
                         hourly = FALSE,
                         simplify_names = TRUE,
                         verbose = FALSE,
                         fmi_apikey = NA_character_) {

  queries <- construct_query(fmi_apikey, start, end, station_id, params, hourly)
  if (length(queries) >= 30) {
    warn <- glue("Note that the request is split into ",
                 "{length(queries)} parts due to fmi api limits.")
    warning(warn, call. = FALSE, immediate. = TRUE)
  }

  get_slowly <- slowly(~GET(.x))
  get_verbosely <- function(query, n) {
    glue("Executing query {n}/{length(queries)}") %>% print()
    get_slowly(query)
  }
  res <- if (verbose) imap(queries, get_verbosely) else map(queries, get_slowly)
  report_errors(res)

  res_content <- map_df(res, process_content)

  # remove time information
  if (!hourly) res_content$date <- as_date(res_content$date)

  if (simplify_names) res_content <- simplify_colnames(res_content)

  res_content
}
