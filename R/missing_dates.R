#' Check for missing dates between first and last date
#'
#' @param x A date or datetime vector, or a data frame.
#'
#' @returns
#' A `Date` vector.
#'
#' @export
missing_dates <- function(x){
  check_is_time(x)
  d <- lubridate::as_date(collapse::funique(x))
  d_seq <- time_grid(d, new_timespan("days", 1))
  cheapr::setdiff_(d_seq, d)
}
#' @export
n_missing_dates <- function(x){
  check_is_time(x)
  time_num_gaps(
    lubridate::as_date(
      collapse::funique(x)
      ), new_timespan("days", 1)
  )
}
