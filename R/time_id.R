#' Time ID
#'
#' @description Generate a time ID that signifies how many time steps away
#' a time value is from the starting time point or more intuitively,
#' this is the time passed since
#' the first time point.
#'
#' @param x Time variable. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`, `yearmon`, or `yearqtr`.
#' @param time_by Time unit. \cr
#' This signifies the granularity of the time data with which to measure gaps
#' in the sequence.
#' If your data is daily for example, supply `time_by = "days"`.
#' If weekly, supply `time_by = "week"`.
#' Must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param g Object used for grouping x.
#' This can for example be a vector or data frame.
#' `g` is passed directly to `collapse::GRP()`.
#' @param na_skip Should `NA` values be skipped? Default is `TRUE`.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @param shift Value used to shift the time IDs. Typically this is 1 to ensure the
#' IDs start at 1 but can be 0 or even negative if for example
#' your time values are going backwards in time.
#'
#' @returns
#' An integer vector the same length as `x`.
#'
#' @details
#' This is heavily inspired by `collapse::timeid` but differs in 3 ways:
#' * The time steps need not be the greatest common divisor of successive
#' differences
#' * The starting time point may not necessarily
#' be the earliest chronologically and thus `time_id` can generate negative IDs.
#' * `g` can be supplied to calculate IDs by group.
#'
#' `time_id(c(3, 2, 1))` is not the same as `collapse::timeid(c(3, 2, 1))`.
#' In general `time_id(sort(x))`
#' should be equal to  `collapse::timeid(sort(x))`.
#' The time difference GCD is always calculated using all the data and not
#' by-group.
#'
#' @seealso [time_elapsed] [time_seq_id]
#'
#' @export
time_id <- function(x, time_by = NULL, g = NULL, na_skip = TRUE,
                    time_type = c("auto", "duration", "period"),
                    shift = 1L){
  time_by <- time_by_get(x, time_by)
  check_length(shift, 1)
  elapsed <- time_elapsed(x, time_by = time_by, g = g,
                          na_skip = na_skip,
                          time_type = time_type,
                          rolling = FALSE)
  # Make sure we don't lose precision when converting to integer
  out <- trunc(elapsed)
  elapsed_rounded <- round(elapsed)
  elapsed_truncated <- which(double_equal(elapsed, elapsed_rounded) & (elapsed_rounded != out))
  out[elapsed_truncated] <- elapsed_rounded[elapsed_truncated]
  as.integer(out) + shift
}
# To more closely match collapse::timeid one can use the below 3 lines
# time_diff_gcd <- time_diff_gcd(x)
# first_time <- collapse::fmin(x, g = g, na.rm = na_skip,
#                              TRA = "replace_fill")
# out <- time_diff(first_time, x, time_by = time_diff_gcd, time_type = time_type)
