#' Fast greatest common divisor of time differences
#'
#' @param x Time variable. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`, `yearmon`, or `yearqtr`.
#' @param time_by Time unit (default is 1). \cr
#' Must be one of the following:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param time_type If "auto", `periods` are used if `x` is a Date and
#' durations are used if `x` is a datetime.
#' Otherwise numeric differences are calculated.
#' @param is_sorted Is `x` sorted? If `FALSE`, the default,
#' `sort(unique(x))` is calculated, otherwise `unique(x)` is used.
#' The order of successive time differences may impact the result.
#' @param tol Tolerance of comparison. The time differences are rounded
#' using `digits = ceiling(abs(log10(tol)))` to try and avoid
#' precision issues.
#'
#' @returns
#' A double vector of length 1 or length 0 if `length(x)` is 0.
#'
#' @examples
#' library(timeplyr)
#' library(lubridate)
#' \dontshow{
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' time_diff_gcd(1:10)
#' time_diff_gcd(seq(0, 1, 0.2))
#'
#' time_diff_gcd(time_seq(today(), today() + 100, time_by = "3 days"))
#' time_diff_gcd(time_seq(now(), len = 10^2, time_by = "125 seconds"))
#'
#' # Monthly gcd using lubridate periods
#' quarter_seq <- time_seq(today(), len = 24, time_by = months(4))
#' time_diff_gcd(quarter_seq, time_by = months(1))
#' time_diff_gcd(quarter_seq, time_by = "months", time_type = "duration")
#'
#' # Detects monthly granularity
#' dplyr::near(time_diff_gcd(as.vector(time(AirPassengers))), 1/12, tol = 1e-06)
#' @export
time_diff_gcd <- function(x, time_by = 1,
                          time_type = c("auto", "duration", "period"),
                          is_sorted = FALSE,
                          tol = sqrt(.Machine$double.eps)){
  if (!is_sorted){
    is_sorted <- is_sorted(x)
  }
  x <- collapse::funique(x, sort = !is_sorted)
  if (length(x) == 1L && is.na(x)){
    return(NA_real_)
  }
  x <- collapse::na_rm(x)
  if (length(x) == 0L){
    return(numeric())
  }
  if (length(x) == 1L){
    return(1)
  }
  tdiff <- time_elapsed(x, rolling = FALSE,
                        time_by = time_by,
                        time_type = time_type,
                        g = NULL,
                        na_skip = FALSE)
  tdiff <- collapse::fdiff.default(tdiff, fill = Inf, n = 1)
  log10_tol <- ceiling(abs(log10(tol)))
  tdiff <- collapse::funique.default(
    round(
      abs(tdiff), digits = log10_tol
    )
  )
  tdiff <- tdiff[double_gt(tdiff, 0, tol = tol)]
  if (length(tdiff) == 1 && tdiff == Inf){
    return(10^(-log10_tol))
  }
  collapse::vgcd(tdiff)
}
