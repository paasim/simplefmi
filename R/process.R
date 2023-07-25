#' fmi_get
#'
#' Downloads the data from FMI API and extracts it from the xml-result. The
#' queries are executed with `purrr::get_slowly`.
#'
#' @param queries List of queries (urls).
#' @param simplify_names if `TRUE`, the column names will be simplified with
#' [name_map()]
#' @param verbose if `TRUE`, progress between queries is printed.
#'
#' @return A list of http results
fmi_get <- function(queries, simplify_names, verbose) {
  if (length(queries) >= 30) {
    warn <- glue::glue(
      "Note that the request is split into {n} parts due to fmi api limits.",
      n = length(queries)
    )
    warning(warn, call. = FALSE, immediate. = TRUE)
  }

  get_slowly <- purrr::slowly(httr::GET)
  get_verbosely <- function(query, n) {
    glue::glue("Executing query {n}/{length(queries)}") |> print()
    get_slowly(query)
  }
  res <- if (verbose) {
    purrr::imap(queries, get_verbosely)
  } else {
    purrr::map(queries, get_slowly)
  }
  report_errors(res)

  res <- purrr::map_df(res, process_content)

  if (simplify_names) {
    rnm <- name_map(colnames(res))
    res <-dplyr::rename(res, dplyr::all_of(rnm))
  }

  res
}

#' construct_query
#'
#' Constructs the queries to get the data. Most importantly, splits it into
#' small enough chunks for the API with [mk_time_seq()].
#'
#' @param apikey An optional fmi-apikey.
#'  See <http://en.ilmatieteenlaitos.fi/open-data-manual>.
#' @param start Start time as a POSIXt-object obtained with e.g.
#'  lubridate::as_datetime().
#' @param end End time as a POSIXt-object. Defaults to `start`.
#' @param station_id The weather station id. Defaults to `100971`.
#'  (Kaisaniemi weather station). A list of the available stations can be
#'  downloaded with the [get_stations()]-function.
#' @param params Query parameters, a comma separated string, such as
#' `"tday,rrday"`. For more options, see Details.
#' @param type Type of the query, must be one of the following:
#' `"weather-daily", "weather-hourly", "airquality", "radiation"`.
#'
#' @return A list queries; one for each time chunk
construct_query <- function(apikey, start, end, station_id, params, type) {
  if (type == "weather-daily") {
    query_id <- "fmi::observations::weather::daily::simple"
    timestep <- 1440
    # possible values of params: rrday, tday, snow, tmin, tmax, tg_pt13h_min
  } else if (type == "weather-hourly") {
    query_id <- "fmi::observations::weather::simple"
    timestep <- 60
    # possible values  t2m, ws_10min, wg_10min, wd_10min, n_man,
    # of params:       rh, td, r_1h, ri_10min, snow_aws, p_sea, vis, wawa
  } else if (type == "airquality") {
    query_id <- "urban::observations::airquality::hourly::simple"
    timestep <- 60
    # possible values  so2_pt1h_avg, no_pt1h_avg, no2_pt1h_avg, o3_pt1h_avg,
    # of params:       trsc_pt1h_avg, co_pt1h_avg, pm10_pt1h_avg, pm25_pt1h_avg,
    #                  qbcpm25_pt1h_avg, aqindex_pt1h_avg
  } else if (type == "radiation") {
    query_id <- "fmi::observations::radiation::simple"
    timestep <- 60
    # possible values  lwin_1min, lwout_1min, glob_1min, dir_1min, refl_1min,
    # of params:       sund_1min, diff_1min, net_1min, uvb_u
  } else {
    stop(glue::glue("unknown type '{type}'"))
  }

  times <- mk_time_seq(start, end, timestep == 60) |> purrr::map(fmi_date_form)

  url <- if (is.na(apikey)) {
    "http://opendata.fmi.fi"
  } else {
    stringr::str_c("http://data.fmi.fi/fmi-apikey/", apikey)
  }

  url_nodate <- glue::glue(
    "{url}/wfs?request=getFeature",
    "&storedquery_id={q}&timestep={t}&parameters={p}", "&fmisid={s}",
    url = url, q = query_id, t = timestep, p = params, s = station_id
  )

  purrr::map2_chr(
    times$start, times$end,
    \(s, e) glue::glue(url_nodate, "&starttime={s}&endtime={e}", s = s, e = e)
  )
}

#' process_content
#'
#' Extract the relevant data from the XML-result.
#'
#' @param res HTTP-result containing the observations as XML.
#'
#' @return The observations formatted in a tibble.
process_content <- function(res) {
  nodes <- httr::content(res, "text", "text/xml", "UTF-8") |>
    xml2::read_xml() |>
    xml2::xml_find_all("./wfs:member/BsWfs:BsWfsElement")

  get <- function(x) {
    xml2::xml_find_all(nodes, stringr::str_c("./BsWfs:", x)) |>
      xml2::xml_text()
  }
  tibble::tibble(
    date = get("Time") |>
      stringr::str_replace("T", " ") |>
      lubridate::as_datetime(),
    key = get("ParameterName"),
    value = get("ParameterValue") |>
      readr::parse_number(c("", "NA", "NaN"))
  ) |>
    tidyr::spread("key", "value")
}
