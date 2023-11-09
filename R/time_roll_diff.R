#' Lagged time differences
#'
#' @description
#' `time_roll_diff` is like `diff()` but always returns a `numeric(length(x))`.
#'
#' @param time Time variable. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`, `yearmon`, or `yearqtr`.
#' @param time_by Time unit. \cr
#' Must be one of the following:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param lag A number indicating the lag size. Negative values are allowed.
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#'
#' @returns
#' A numeric vector the same length as `x.`
#' on the arguments supplied.
#'
#' @seealso [time_elapsed]
#'
#' @details `time_elapsed` is very similar to `time_roll_diff` but
#' is more general in that it supports
#' cumulative time differencing, `NA` filling as well as `NA` skipping.
#'
time_roll_diff <- function(time, time_by = 1, lag = 1L,
                           g = NULL,
                           time_type = getOption("timeplyr.time_type", "auto")){
  check_is_time_or_num(time)
  # lagseq <- lag_seq(time, lag)
  # time_lag <- roll_lag(time, lagseq, check = FALSE)
  time_lag <- flag2(time, n = lag, g = g)
  time_diff(time_lag, time,
            time_by = time_by,
            time_type = time_type)
}
