#' SIMPLEFMI
#'
#' Simplified version of the amazing fmi-package for personal use.
#'
#' @docType package
#' @name simplefmi
#'
#' @importFrom curl curl_fetch_memory
#' @importFrom dplyr bind_rows filter mutate rename
#' @importFrom lubridate "%--%" as_date as_datetime days hours weeks years
#' @importFrom magrittr "%>%" "%T>%"
#' @importFrom stringr str_c str_replace str_replace_all str_split str_subset
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr spread
NULL