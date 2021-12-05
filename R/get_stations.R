#' get_stations
#'
#' Download the available stations from
#'  <http://en.ilmatieteenlaitos.fi/open-data-manual>. See especially the
#' variable `groups` which specifies wheter the station provides weather or
#' urban air quality data.
#'
#' @return A tibble with the station name, id, and other information.
#'
#' @export
#'

get_stations <- function() {
  fromJSON("https://cdn.fmi.fi/weather-observations/metadata/all-finnish-observation-stations.en.json") %>%
    pluck("items") %>%
    as_tibble() %>%
    rename("station_id" = "fmisid", "lat" = "y", "lon" = "x")
}
