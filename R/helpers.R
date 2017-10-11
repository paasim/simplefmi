# Construct the query needed for downloading the data
construct_query <- function(fmi_apikey, start, end, station_id, params, hourly) {

  if (hourly) {
    q_id <- "fmi::observations::weather::simple"
    timestep <- 60
    # possible values:  t2m, ws_10min, wg_10min, wd_10min, rh, td,
    # of params         r_1h, ri_10min, snow_aws, p_sea, vis

    # split the request into smaller queries:
    n_periods <- (((start %--% end) / weeks(1)) %>% floor()) + 1
    starts <- start + weeks(1:n_periods - 1)
    ends <- start + weeks(1:n_periods) - hours(1)
    ends[length(ends)] <- min(ends[length(ends)], end)

  } else {
    q_id <- "fmi::observations::weather::daily::simple"
    timestep <- 1440
    # possible values of params : rrday, tday, snow, tmin, tmax, TG_PT12H_min

    n_periods <- (((start %--% end) / years(1)) %>% floor()) + 1
    starts <- start + years(1:n_periods - 1)
    ends <- start + years(1:n_periods) - days(1)
    ends[length(ends)] <- min(ends[length(ends)], end)
  }

  url_nodate <- str_c("http://data.fmi.fi/fmi-apikey/", fmi_apikey,
                      "/wfs?request=getFeature&storedquery_id=", q_id,
                      "&timestep=", timestep,
                      "&parameters=", params,
                      "&fmisid=", station_id)

  map2_chr(fmi_date_form(starts), fmi_date_form(ends),
           ~str_c(url_nodate, "&starttime=", .x, "&endtime=", .y))
}

fmi_date_form <- function(d) format(d, "%Y-%m-%dT%H:%M:%SZ")

process_content <- function(raw_content) {

  nodes <- read_xml(raw_content) %>%
    xml_find_all("./wfs:member/BsWfs:BsWfsElement")

  get <- function(x) xml_find_all(nodes, str_c("./BsWfs:", x)) %>% xml_text()
  tibble(date = get("Time") %>% str_replace("T", " ") %>% as_datetime(),
         key = get("ParameterName"),
         value = get("ParameterValue") %>% parse_number(c("", "NA", "NaN"))) %>%
    spread("key", "value")
}

simplify_colnames <- function(res) {
  simplify_names_list <- set_names(
    c("temp", "wind", "rain", "snow", "visibility",
      "rain", "temp", "temp_min", "temp_max"),
    c("t2m", "ws_10min", "r_1h", "snow_aws", "vis",
      "rrday", "tday", "tmin", "tmax"))
  set_names(res, str_replace_all(colnames(res), simplify_names_list))
}

report_errors <- function(res_list) {

  errors <- map_lgl(res_list, http_error)
  if (any(errors)) {
    str_c("\nQuery returned with error(s), see the first one below:\n",
          res_list %>% pluck(which(errors)[1], "content") %>% rawToChar()) %>%
      stop()
  }
}
