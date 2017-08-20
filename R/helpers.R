# Construct the query needed for downloading the data
construct_query <- function(fmi_apikey, start, end, station_id, params, hourly) {

  if (hourly) {
    q_id <- "fmi::observations::weather::simple"
    timestep <- 60
    # possible values: t2m, ws_10min, wg_10min, wd_10min, rh, td,
    #                  r_1h, ri_10min, snow_aws, p_sea, vis
    if (is.null(params)) params <- "t2m,r_1h"

    # split the request into smaller queries:
    n_periods <- ((start %--% end) / weeks(1)) %>% floor() + 1
    starts <- start + weeks(1:n_periods - 1)
    ends <- start + weeks(1:n_periods) - hours(1)
    ends[length(ends)] <- min(ends[length(ends)], end)

  } else {
    q_id <- "fmi::observations::weather::daily::simple"
    timestep <- 1440
    # possible values: rrday, tday, snow, tmin, tmax, TG_PT12H_min
    if (is.null(params)) params <- "tday,rrday"

    n_periods <- ((start %--% end) / years(1)) %>% ceiling()
    starts <- start + years(1:n_periods - 1)
    ends <- start + years(1:n_periods) - days(1)
    ends[length(ends)] <- min(ends[length(ends)], end)

  }

  form <- function(d) format(d, "%Y-%m-%dT%H:%M:%SZ")

  lapply(seq_along(starts), function(i) {
    str_c("http://data.fmi.fi/fmi-apikey/", fmi_apikey,
          "/wfs?request=getFeature&storedquery_id=", q_id,
          "&timestep=", timestep,
          "&parameters=", params,
          "&fmisid=", station_id,
          "&starttime=", form(starts[i]),
          "&endtime=", form(ends[i]))
  })
}

# Try to report possible errors
report_errors <- function(res) {

  status_codes <- vapply(res, function(x) x$status_code, integer(1))
  errors <- which(status_codes != 200)
  if (length(errors) > 0) {
    str_c("\nQuery returned with error(s), see the first one below:\n",
          rawToChar(res[[errors[1]]]$headers),
          rawToChar(res[[errors[1]]]$content)) %>%
      stop()
  }

}
