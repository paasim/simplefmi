#' get_stations
#'
#' Download the available weather stations from
#'  \url{http://en.ilmatieteenlaitos.fi/open-data-manual}.
#'
#' @return A tibble with the station name, id, latitude and longitude.
#'
#' @export
#'

get_stations <- function() {
  GET("http://en.ilmatieteenlaitos.fi/observation-stations") %>%
    read_html() %>%
    html_table() %>%
    pluck(1) %>% # there is only one html table on the page
    as_tibble() %>%
    filter(.data$Groups == "Weather stations") %>% # only weather stations
    select(name = .data$Name, station_id = .data$FMISID,
           lat = .data$Lat, lon = .data$Lon) %>%
    mutate(station_id = as.character(.data$station_id))
}