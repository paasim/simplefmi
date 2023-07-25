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
  "https://cdn.fmi.fi/weather-observations/metadata/all-finnish-observation-stations.en.json" |>
    jsonlite::fromJSON() |>
    purrr::pluck("items") |>
    tibble::as_tibble() |>
    dplyr::rename("station_id" = "fmisid", "lat" = "y", "lon" = "x")
}
