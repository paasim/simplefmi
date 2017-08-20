# simplefmi

[![Build Status](https://travis-ci.org/paasim/simplefmi.svg?branch=master)](https://travis-ci.org/paasim/simplefmi)

A simplified version of the amazing fmi-package for downloading weather data via FMI api in a tidy format.

Installation
------------

    devtools::install_github('paasim/simplefmi)


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


