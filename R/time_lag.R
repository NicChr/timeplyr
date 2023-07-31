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
#' @export
time_lag <- function(x, k = 1L,
                     time = seq_along(x),
                     g = NULL,
                     time_type = c("auto", "duration", "period"),
                     roll_month = "preday", roll_dst = "pre"){
  k <- time_by_list(k)
  # k <- setnames(list(time_by_num(k) + 1), time_by_unit(k))
  sizes <- time_roll_window_size(time,
                                 window = k,
                                 g = g,
                                 partial = TRUE,
                                 close_left_boundary = TRUE,
                                 time_type = time_type,
                                 roll_month = roll_month,
                                 roll_dst = roll_dst)
  time_lag <- sizes - (as.integer(1L * time_by_sign(k)))
  # time_lag <- sizes - 1L
  out <- roll_lag(x, time_lag)
  which_rolled <- collapse::whichv(
    time_diff(roll_lag(time, time_lag), time, time_by = k),
    1L, invert = TRUE
  )
  out[which_rolled] <- out[NA_integer_]
  out
}
