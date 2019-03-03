# simplefmi

[![Build Status](https://travis-ci.org/paasim/simplefmi.svg?branch=master)](https://travis-ci.org/paasim/simplefmi)
[![codecov](https://codecov.io/gh/paasim/simplefmi/branch/master/graphs/badge.svg)](https://codecov.io/gh/paasim/simplefmi)

An R package for downloading weather data from the [FMI API](http://en.ilmatieteenlaitos.fi/open-data-manual) in a tidy format for personal use. Inspired by the [fmi](https://github.com/rOpenGov/fmi)-package, but with a lot less functionality.

Installation
------------

    devtools::install_github("paasim/simplefmi")


Usage
-----

    library(simplefmi)
    library(lubridate) # for date-variables

    # get yesterday's hourly rain and temperature data from kaisaniemi
    yesterday <- today() - days(1)
    yesterday_0 <- ymd_hms(stringr::str_c(yesterday, " 00:00:00"))
    yesterday_23 <- ymd_hms(stringr::str_c(yesterday, " 23:00:00"))
    weather <- fmi_download(yesterday_0, yesterday_23, hourly = TRUE)


