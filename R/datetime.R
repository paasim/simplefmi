#' mk_time_seq
#'
#' Make a sequence of (start,end)-pairs that cover the data.
#'
#' @param start Start time as a POSIXt-object obtained with e.g.
#'  lubridate::as_datetime().
#' @param end End time as a POSIXt-object. Defaults to `start`.
#' @param hourly if `TRUE`, download hourly observations instead of daily.
#'
#' @return A list queries; one for each time chunk
mk_time_seq <- function(start, end, hourly) {
  if (hourly) {
    start <- lubridate::as_datetime(start) |> as.POSIXct()
    end <- lubridate::as_datetime(end) |> as.POSIXct()
    by <- "week"
    diff <- lubridate::hours(1)
  } else {
    start <- lubridate::as_date(start)
    end <- lubridate::as_date(end)
    by <- "year"
    diff <- lubridate::days(1)
  }

  if (end < start) stop("start date bigger than end date.")
  if (start == end) {
    return(tibble::tibble(start = start, end = end))
  }
  end_m1 <- end - diff
  starts <- seq(start, end_m1, by)
  ends <- c(starts[-1] - diff, end)

  tibble::tibble(start = starts, end = ends)
}

fmi_date_form <- function(d) format(d, "%Y-%m-%dT%H:%M:%SZ")
