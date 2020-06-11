#' get_stations
#'
#' Download the available weather stations from
#'  <http://en.ilmatieteenlaitos.fi/open-data-manual>.
#'
#' @return A tibble with the station name, id, latitude and longitude.
#'
#' @export
#'

get_stations <- function() {
  fromJSON("https://cdn.fmi.fi/weather-observations/metadata/all-finnish-observation-stations.en.json") %>%
    pluck("items") %>%
    as_tibble() %>%
    rename("station_id" = "fmisid", "lat" = "y", "lon" = "x")
}
