#' SIMPLEFMI
#'
#' Simplified version of the amazing fmi-package for personal use.
#'
#' @docType package
#' @name simplefmi
#'
#' @importFrom dplyr bind_rows filter mutate select
#' @importFrom httr GET http_error
#' @importFrom lubridate "%--%" as_date as_datetime ddays dhours weeks years
#' @importFrom magrittr "%>%" "%T>%"
#' @importFrom purrr map map_lgl map2_chr pluck set_names
#' @importFrom rlang .data
#' @importFrom stringr str_c str_replace str_replace_all str_split str_subset
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr spread
#' @importFrom xml2 read_xml xml_contents xml_name xml_text
NULL