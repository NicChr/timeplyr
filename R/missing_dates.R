#' Check for missing dates between first and last date
#'
#' @param x A date or datetime vector, or a data frame.
#'
#' @returns
#' A date vector if x is a vector, or a list if x is a `data.frame`.
#'
#' @rdname missing_dates
#' @export
missing_dates <- function(x){
  UseMethod("missing_dates")
}
#' @export
missing_dates.default <- function(x){
  check_is_time(x)
  d <- lubridate::floor_date(x, unit = "days")
  d_seq <- time_expandv(d, time_by = list("days" = 1))
  lubridate::as_date(d_seq[!d_seq %in% d])
}
#' @export
missing_dates.data.frame <- function(x){
  time_vars <- which(vapply(x, is_time, FALSE))
  lapply(fselect(x, .cols = time_vars), missing_dates)

}
#' @rdname missing_dates
#' @export
n_missing_dates <- function(x){
  UseMethod("n_missing_dates")
}
#' @export
n_missing_dates.default <- function(x){
  check_is_time(x)
  time_num_gaps(lubridate::floor_date(x, unit = "days"),
                time_by = list("days" = 1))
}
#' @export
n_missing_dates.data.frame <- function(x){
  time_vars <- which(vapply(x, is_time, FALSE))
  lapply(fselect(x, .cols = time_vars), n_missing_dates)
}
