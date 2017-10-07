#' fmi_download
#'
#' Download data from the fmi weather API.
#'
#' @param fmi_apikey Your personal fmi-apikey.
#'   See \url{http://en.ilmatieteenlaitos.fi/open-data-manual}.
#' @param start Start time as a POSIXt-object obtained with e.g.
#'   lubridate::as_datetime().
#' @param end End time as a POSIXt-object. Defaults to \code{start}.
#' @param station_id The weather station id. Defaults to \code{100971}
#'   (Kaisaniemi weather station). The available stations can be downloaded
#'   with the \link{get_stations}-function.
#' @param params Query parameters. If not provided, temprature and
#'   precipitation is downloaded. See
#'   \url{http://en.ilmatieteenlaitos.fi/open-data-manual-fmi-wfs-services}.
#' @param hourly If \code{TRUE}, hourly data is downloaded. Otherwise the daily
#'   data is downloaded.
#' @param simplify_names If \code{TRUE}, an attempt is made to simplify some of
#'   the names of the variables. For example, t2m/tday is temp etc.
#'
#' @return A tibble with date (& time if hourly data is requested) in the first
#'   column and the variables specified in \code{params} in the other columns.
#'
#' @export
#'

fmi_download <- function(fmi_apikey,
                         start,
                         end = start,
                         station_id = "100971",
                         params = ifelse(hourly, "t2m,r_1h", "tday,rrday"),
                         hourly = FALSE,
                         simplify_names = TRUE) {

  query <- construct_query(fmi_apikey, start, end, station_id, params, hourly)

  res <- map(query, GET) %T>% report_errors()
  res_content <- map(res, "content") %>%
    map_df(process_content)

  # remove time information
  if (!hourly) res_content$date <- as_date(res_content$date)

  if (simplify_names) res_content <- simplify_colnames(res_content)

  res_content
}
