#' SIMPLEFMI
#'
#' An R package for downloading weather data from the FMI API.
#'
#' @docType package
#' @name simplefmi
#'
#' @importFrom dplyr bind_rows filter mutate rename select
#' @importFrom httr content GET http_error
#' @importFrom lubridate "%--%" as_date as_datetime days hours weeks years
#'  today ymd_hms
#' @importFrom magrittr "%>%" "%T>%"
#' @importFrom purrr keep map map_df map_lgl map2_chr pluck set_names
#' @importFrom readr parse_number
#' @importFrom rlang .data
#' @importFrom rvest html_table
#' @importFrom stringr str_detect str_c str_replace str_replace_all
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr spread
#' @importFrom xml2 read_html read_xml xml_find_all xml_name xml_text
NULL