% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fmi_download.R
\name{fmi_radiation}
\alias{fmi_radiation}
\title{fmi_radiation}
\usage{
fmi_radiation(
  start,
  end = start,
  station_id = "101004",
  params = "dir_1min,diff_1min",
  simplify_names = TRUE,
  verbose = FALSE,
  fmi_apikey = NA_character_
)
}
\arguments{
\item{start}{Start time as a POSIXt-object obtained with e.g.
lubridate::as_datetime().}

\item{end}{End time as a POSIXt-object. Defaults to \code{start}.}

\item{station_id}{The station id. Defaults to \code{101004}.
(Kumpula station). A list of the available
stations can be downloaded with the \code{\link[=get_stations]{get_stations()}}-function.}

\item{params}{Query parameters, a comma separated string, such as
\code{"dir_1min,diff_1min"}. For more options, see Details.}

\item{simplify_names}{If \code{TRUE}, variable names are simplified (eg.
\code{GLOB_1MIN} is converted to \code{global_radiation}).}

\item{verbose}{If \code{TRUE}, prints the progress. Defaults to \code{FALSE}.}

\item{fmi_apikey}{An optional fmi-apikey.
See \url{http://en.ilmatieteenlaitos.fi/open-data-manual}.}
}
\value{
A tibble with time in the first
column and the variables specified in \code{params} in the other columns.
}
\description{
Download radiation data from the fmi radiation API. If the request is larger
than the api allows, it is split into multiple parts. If this results to more
than 30 queries, a warning message is printed. \emph{Note}, this might be for
the first minute of the hour as it works currently, rather than some kind
of averages!
}
\details{
The available variables are listed below, more information can be found from
\url{https://en.ilmatieteenlaitos.fi/open-data-manual}.

The following variables are available:
\itemize{
\item \code{lwin_1min}: long-wave solar radiation (W/m2)
\item \code{lwout_1min}: outgoing long-wave solar radiation (W/m2)
\item \code{glob_1min}: global radiation (W/m2)
\item \code{dir_1min}: direct solar radiation (W/m2)
\item \code{refl_1min}: reflected radiation (W/m2)
\item \code{sund_1min}: sunshine duration (s)
\item \code{diff_1min}: diffuse radiation (W/m2)
\item \code{net_1min}: radiation balance (W/m2)
\item \code{uvb_u}: ultraviolet irradiance (index)
}
}
