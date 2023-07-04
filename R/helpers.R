#fmi Construct the query needed for downloading the data
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
    stop(glue("unknown type '{type}'"))
  }

  times <- mk_time_seq(start, end, timestep == 60) |> map(fmi_date_form)

  url <- if (is.na(apikey)) {
    "http://opendata.fmi.fi"
  } else {
    str_c("http://data.fmi.fi/fmi-apikey/", apikey)
  }

  url_nodate <- glue("{url}/wfs?request=getFeature",
                     "&storedquery_id={q}", "&timestep={t}",
                     "&parameters={p}", "&fmisid={s}",
                     url = url, q = query_id, t = timestep,
                     p = params, s = station_id)

  map2_chr(times$start, times$end,
           ~glue(url_nodate, "&starttime={.x}&endtime={.y}"))
}

mk_time_seq <- function(start, end, hourly) {

  if (hourly) {
    start <- as_datetime(start) |> as.POSIXct()
    end <- as_datetime(end) |> as.POSIXct()
    by <- "week"
    diff <- hours(1)
  } else {
    start <- as_date(start)
    end <- as_date(end)
    by <- "year"
    diff <- days(1)
  }

  if (end < start) stop("start date bigger than end date.")
  if (start == end) return(tibble(start = start, end = end))
  end_m1 <- end - diff
  starts <- seq(start, end_m1, by)
  ends <- c(starts[-1] - diff, end)

  tibble(start = starts, end = ends)
}

fmi_date_form <- function(d) format(d, "%Y-%m-%dT%H:%M:%SZ")

process_content <- function(res) {

  nodes <- content(res, "text", "text/xml", "UTF-8") |>
    read_xml() |>
    xml_find_all("./wfs:member/BsWfs:BsWfsElement")

  get <- function(x) xml_find_all(nodes, str_c("./BsWfs:", x)) |> xml_text()
  tibble(date = get("Time") |> str_replace("T", " ") |> as_datetime(),
         key = get("ParameterName"),
         value = get("ParameterValue") |> parse_number(c("", "NA", "NaN"))) |>
    spread("key", "value")
}

simplify_colnames <- function(res) {

  name_map <- c(
    "temp" = "t2m",
    "cloud_cover" = "TotalCloudCover",
    "cloud_cover" = "n_man",
    "wind_speed" = "ws_10min",
    "wind_gust" = "wg_10min",
    "wind_dir" = "wd_10min",
    "humidity" = "rh",
    "dew_point" = "td",
    "rain" = "r_1h",
    "rain_intensity" = "ri_10min",
    "snow" = "snow_aws",
    "air_pressure" = "p_sea",
    "visibility" = "vis",
    "rain" = "rrday",
    "temp" = "tday",
    "temp_min" = "tmin",
    "temp_max" = "tmax",
    "temp_ground" = "tg_pt12h_min",
    "weather_code" = "wawa",

    "airquality" = "aqindex_pt1h_avg",
    "carbon_monoxide" = "co_pt1h_avg",
    "nitrogen_dioxide" = "no2_pt1h_avg",
    "nitrogen_monoxide" = "no_pt1h_avg",
    "ozone" = "o3_pt1h_avg",
    "pm_10" = "pm10_pt1h_avg",
    "pm_25" = "pm25_pt1h_avg",
    "black_carbon" = "qbcpm25_pt1h_avg",
    "sulphur_dioxide" = "so2_pt1h_avg",
    "odorous_sulphur_compounds" = "trsc_pt1h_avg",

    "longwave" = "lwin_1min",
    "outgoing_longwave" = "lwout_1min",
    "global" = "glob_1min",
    "direct" = "dir_1min",
    "reflected" = "refl_1min",
    "sunshine" = "sund_1min",
    "diffuse" = "diff_1min",
    "balance" = "net_1min",
    "uv_irradiance" = "uvb_u"
  )

  rnm <- name_map[name_map %in% colnames(res)]
  rename(res, all_of(rnm))
}

report_errors <- function(res_list) {

  errors <- keep(res_list, http_error)
  if (length(errors) > 0L) {
    str_c("\nQuery returned with error(s), see the first one below:\n",
          content(errors[[1]], "text", "text/xml", "UTF-8")) |>
      stop(call. = FALSE)
  }
}

.onAttach <- function(...) {

  ver <- utils::packageVersion("simplefmi")
  packageStartupMessage("This is simplefmi version ", ver)
}
