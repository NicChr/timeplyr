#' Check for missing dates between first and last date
#'
#' @param x A Date or Date-Time vector.
#'
#' @returns
#' A `Date` vector.
#'
#' @rdname missing_dates
#' @export
missing_dates <- function(x){
  check_is_time(x)
  d <- lubridate::as_date(collapse::funique(x))
  d_seq <- time_grid(d, new_timespan("days", 1))
  cheapr::setdiff_(d_seq, d)
}
#' @rdname missing_dates
#' @export
n_missing_dates <- function(x){
  check_is_time(x)
  time_num_gaps(
    lubridate::as_date(
      collapse::funique(x)
      ), new_timespan("days", 1)
  )
}
