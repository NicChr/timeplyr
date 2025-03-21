#' Time ID
#'
#' @description Generate a time ID that signifies how many time steps away
#' a time value is from the starting time point or more intuitively,
#' this is the time passed since
#' the first time point.
#'
#' @param x Time vector. \cr
#' E.g. a `Date`, `POSIXt`, `numeric` or any time-based vector.
#' @param timespan [timespan].
#' @param g Object used for grouping x.
#' This can for example be a vector or data frame.
#' `g` is passed directly to `collapse::GRP()`.
#' @param na_skip Should `NA` values be skipped? Default is `TRUE`.
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
time_id <- function(x, timespan = granularity(x), g = NULL,
                    na_skip = TRUE, shift = 1L){
  check_is_time_or_num(x)
  timespan <- timespan(timespan)
  check_length(shift, 1)
  out <- time_elapsed(x, timespan, g = g,
                      na_skip = na_skip,
                      rolling = FALSE)
  out <- as.integer(round2(out, digits = 9))
  cheapr::set_add(out, as.integer(shift))
}
# To more closely match collapse::timeid one can use the below 3 lines
# first_time <- collapse::fmin(x, g = g, na.rm = na_skip,
#                              TRA = "replace_fill")
# time_diff(first_time, x, granularity(x))
