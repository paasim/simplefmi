# Construct the query needed for downloading the data
construct_query <- function(fmi_apikey, start, end, station_id, params, hourly) {

  if (hourly) {
    q_id <- "fmi::observations::weather::simple"
    timestep <- 60
    # possible values:  t2m, ws_10min, wg_10min, wd_10min, rh, td,
    # of params         r_1h, ri_10min, snow_aws, p_sea, vis

    # split the request into smaller queries:
    n_periods <- ((start %--% end) / weeks(1)) %>% floor() + 1
    starts <- (start + weeks(1:n_periods - 1)) %>% as.list()
    ends <- start + weeks(1:n_periods) - dhours(1)
    ends[length(ends)] <- min(ends[length(ends)], end)

  } else {
    q_id <- "fmi::observations::weather::daily::simple"
    timestep <- 1440
    # possible values of params : rrday, tday, snow, tmin, tmax, TG_PT12H_min

    n_periods <- ((start %--% end) / years(1)) %>% ceiling()
    starts <- start + years(1:n_periods - 1)
    ends <- start + years(1:n_periods) - ddays(1)
    ends[length(ends)] <- min(ends[length(ends)], end)

  }

  form <- function(d) format(d, "%Y-%m-%dT%H:%M:%SZ")
  url_nodate <- str_c("http://data.fmi.fi/fmi-apikey/", fmi_apikey,
                      "/wfs?request=getFeature&storedquery_id=", q_id,
                      "&timestep=", timestep,
                      "&parameters=", params,
                      "&fmisid=", station_id)

  map2_chr(form(starts), form(ends),
           ~str_c(url_nodate, "&starttime=", .x, "&endtime=", .y))
}

# Try to report possible errors
report_errors <- function(res_list) {

  errors <- map_lgl(res_list, http_error)
  # status_codes <- vapply(res, function(x) x$status_code, integer(1))
  # errors <- which(status_codes != 200)
  if (any(errors)) {
    str_c("\nQuery returned with error(s), see the first one below:\n",
          res_list %>% pluck(which(errors)[1], "content") %>% rawToChar()) %>%
      stop()
  }

}
