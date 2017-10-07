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

    fmi_apikey <- "api-key" # see ?fmi_download for more information.

    # get yesterday's hourly rain and temperature data from kaisaniemi
    weather <- fmi_download(fmi_apikey,
                   start = floor_date(today() - hours(1), "days"),
                   end = today() - hours(1),
                   hourly = TRUE)


