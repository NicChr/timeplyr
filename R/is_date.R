#' Utility functions for checking if date or datetime
#'
#' @param x date or datetime.
#'
#' @rdname is_date
#' @export
is_date <- function(x){
  inherits(x, "Date")
}
#' @rdname is_date
#' @export
is_datetime <- function(x){
 inherits(x, c("POSIXt", "POSIXct", "POSIXlt"))
}
#' @rdname is_date
#' @export
is_time <- function(x){
  inherits(x, c("Date", "POSIXt", "POSIXct", "POSIXlt"))
}
#' @rdname is_date
#' @export
is_time_or_num <- function(x){
  inherits(x, c("integer", "numeric",
                "Date", "POSIXt", "POSIXct", "POSIXlt")) ||
    isTRUE(typeof(x) %in% c("integer", "double"))
}
