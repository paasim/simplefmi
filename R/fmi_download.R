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
#'   (Kaisaniemi weather station). See
#'   \url{http://en.ilmatieteenlaitos.fi/observation-stations}.
#' @param params Query parameters. If not provided, temprature and
#'   precipitation is downloaded. See
#'   \url{http://en.ilmatieteenlaitos.fi/open-data-manual-fmi-wfs-services}.
#' @param hourly If \code{TRUE}, hourly data is downloaded. Otherwise the daily
#'   data is downloaded.
#'
#' @return A tibble with the following columns with the time in the first column
#'   and the variables specified in \code{params} in the other columns.
#'
#' @export
#'

fmi_download <- function(fmi_apikey, start, end = start,
                         station_id = "100971", params = NULL, hourly = FALSE) {

  query <- construct_query(fmi_apikey, start, end, station_id, params, hourly)

  res_list <- lapply(query, curl_fetch_memory) %T>%
    report_errors()

  # extract the relevant data from the result
  res <- lapply(res_list, function(r) {
    rawToChar(r$content) %>%
      str_split("\n") %>%
      unlist() %>%
      str_subset("Time|ParameterName|ParameterValue") %>%
      str_replace_all("<BsWfs:|</.*>| ", "") %>%
      str_split(">", simplify = TRUE) %>%
      as_tibble()
  }) %>% bind_rows()

  # make the result tidy
  res <- tibble(time = res$V2[res$V1 == "Time"] %>%
                  str_replace("T", " ") %>%
                  as_datetime(tz = "Europe/Helsinki"),
                var = res$V2[res$V1 == "ParameterName"],
                val = as.numeric(res$V2[res$V1 == "ParameterValue"])) %>%
    spread("var", "val")

  # if custom params are not used, colnames are known and can be simplified
  if (is.null(params)) res <- setNames(res, c("time", "rain", "temp"))

  res
}
