#' report_errors
#'
#' Check the http results for error codes. If there is at least one, stop
#' execution and print the result.
#'
#' @param res_list list of http-results.
#'
#' @return Does not return anything but stops execution if one of them returned
#' with an error code.
report_errors <- function(res_list) {
  errors <- purrr::keep(res_list, httr::http_error)
  if (length(errors) > 0L) {
    stringr::str_c(
      "\nQuery returned with error(s), see the first one below:\n",
      httr::content(errors[[1]], "text", "text/xml", "UTF-8")
    ) |>
      stop(call. = FALSE)
  }
}

.onAttach <- function(...) {
  ver <- utils::packageVersion("simplefmi")
  packageStartupMessage("This is simplefmi version ", ver)
}
