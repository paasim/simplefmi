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

  res_list <- map(query, GET) %T>% report_errors()

  proc_elem <- function(x) set_names(xml_text(x), xml_name(x))
  # parse the content from the xml-result
  res <- map(res_list, ~read_xml(.x) %>%
                xml_find_all(".//BsWfs:BsWfsElement") %>%
                # extract the value from each element and combine them
                map(~xml_find_all(.x, ".//*[position()>1]") %>% # drop location
                      proc_elem() %>% t() %>% as_tibble()) %>%
                bind_rows()) %>%
    bind_rows() %>%
    rename(date = .data$Time) %>% # rename time
    mutate(date = str_replace(.data$date, "T", " ") %>% as_datetime(),
           ParameterValue = as.numeric(.data$ParameterValue)) %>%
    spread("ParameterName", "ParameterValue")

  # remove time information
  if (!hourly) res$date <- as_date(res$date)

  if (simplify_names) {
    simplify_names_list <- set_names(
      c("temp", "wind", "rain", "snow", "visibility",
        "rain", "temp", "temp_min", "temp_max"),
      c("t2m", "ws_10min", "r_1h", "snow_aws", "vis",
        "rrday", "tday", "tmin", "tmax"))
    colnames(res) <- str_replace_all(colnames(res), simplify_names_list)
  }

  res
}
