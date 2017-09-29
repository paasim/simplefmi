#' SIMPLEFMI
#'
#' An R package for downloading weather data from the FMI API.
#'
#' @docType package
#' @name simplefmi
#'
#' @importFrom dplyr bind_rows filter mutate rename select
#' @importFrom httr GET http_error
#' @importFrom lubridate "%--%" as_date as_datetime ddays dhours weeks years
#' @importFrom magrittr "%>%" "%T>%"
#' @importFrom purrr map map_lgl map2_chr pluck set_names
#' @importFrom rlang .data
#' @importFrom rvest html_table
#' @importFrom stringr str_c str_replace str_replace_all
#' @importFrom tibble as_tibble
#' @importFrom tidyr spread
#' @importFrom xml2 read_html read_xml xml_find_all xml_name xml_text
NULL