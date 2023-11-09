#' Time-lagged values
#'
#' @param x Vector.
#' @param k Lag size, must be one of the following:
#' * string, e.g `"day"` or `"2 weeks"`
#' * lubridate duration or period object, e.g. `days(1)` or `ddays(1)`.
#' * named list of length one, e.g. `list("days" = 7)`.
#' * Numeric vector, e.g. `7`.
#' @param time (Optional) time index. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`,
#' `yearmon`, or `yearqtr` vector.
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when lubridate periods are specified or when
#' days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#'
#' @returns
#' A vector of `length(x)` lagged by a specified time unit.
#'
#' @examples
#' library(timeplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' x <- 1:10
#' t <- time_seq(Sys.Date(), len = 10, time_by = "3 days")
#'
#' dplyr::lag(x)
#' time_lag(x)
#' time_lag(x, time = t, k = "3 days")
#'
#' # No values exist at t-1 days
#' time_lag(x, time = t, k = 1)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
time_lag <- function(x, k = 1L,
                     time = seq_along(x),
                     g = NULL,
                     time_type = getOption("timeplyr.time_type", "auto"),
                     roll_month = getOption("timeplyr.roll_month", "preday"), roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  k <- time_by_list(k)
  time_num <- time_by_num(k)
  time_by_unit <- time_by_unit(k)
  k_sign <- time_by_sign(k)
  time_window <- add_names(list(time_num + (k_sign * 1)),
                           time_by_unit)
  # k <- add_names(list(time_by_num(k) + 1), time_by_unit(k))
  sizes <- time_roll_window_size(time,
                                 window = time_window,
                                 g = g,
                                 partial = TRUE,
                                 close_left_boundary = FALSE,
                                 time_type = time_type,
                                 roll_month = roll_month,
                                 roll_dst = roll_dst)
  time_lag <- sizes - (as.integer(1L * k_sign))
  # time_lag <- sizes - 1L
  out <- roll_lag(x, time_lag)
  which_rolled <- which(time_diff(roll_lag(time, time_lag), time, time_by = k) != 1)
  out[which_rolled] <- na_init(out)
  out
}
