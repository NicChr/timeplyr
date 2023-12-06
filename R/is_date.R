#' Utility functions for checking if date or datetime
#'
#' @param x Time variable. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`,
#' `yearmon`, `yearqtr`, `year_month` or `year_quarter`.
#'
#' @returns
#' A [logical] of length 1.
#'
#' @rdname is_date
#' @export
is_date <- function(x){
  inherits(x, "Date")
}
#' @rdname is_date
#' @export
is_datetime <- function(x){
  inherits(x, "POSIXt")
}
#' @rdname is_date
#' @export
is_time <- function(x){
  inherits(x, c("Date", "POSIXt"))
}
#' @rdname is_date
#' @export
is_time_or_num <- function(x){
  inherits(x, time_classes)
}

time_classes <- c("integer", "numeric",
                  "Date", "POSIXt", "yearmon", "yearqtr",
                  "year_month", "year_quarter")

