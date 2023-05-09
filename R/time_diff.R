#' Calculate time difference
#'
#' @description This is a neat wrapper around lubridate's
#' date/datetime arithmetic with support for numeric values and
#' any class that supports arithmetic through "base::`-`"
#' If either x or y are date/datetimes and type is "duration" or "period",
#' then lubridate arithmetic is used, otherwise base R arithmetic is used.
#'
#' Some more exotic time units such as quarters, fortnights, etc
#' can be specified.
#'
#' @param x Start date or datetime.
#' @param y End date or datetime.
#' @param by Argument to expand and summarise time series.
#' If `by` is `NULL` then a heuristic will try and estimate the highest
#' order time unit associated with the time variable.
#' If specified, then by must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `by = "days"` or `by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `by = 1`.
#' This is also vectorized where applicable.
#' @param type Time difference type, auto, duration, period.
#' @param as_period Logical. Should time interval be coerced to a period
#' before time difference is calculated? This is useful for calculating
#' for example age in exact years or months.
#' @examples
#' library(timeplyr)
#' library(lubridate)
#' time_diff(today(), today() + days(10),
#'           by = "days")
#' time_diff(today(), today() + days(0:100),
#'           by = "days")
#' time_diff(today(), today() + days(100),
#'           by = list("days" = 1:100))
#' time_diff(1, 1 + 0:100, by = 3)
#' @export
time_diff <- function(x, y, by,
                      type = c("auto", "duration", "period"),
                      as_period = FALSE){
  type <- match.arg(type)
  if (is_time(x) && is_time(y)){
    unit_info <- unit_guess(by)
    units <- unit_info[["unit"]]
    num <- unit_info[["num"]]
    scale <- unit_info[["scale"]]
    num <- num * scale
    # Common but special case where from/to are whole days
    # and type is "auto"
    is_special_case_days <- is_special_case_days(from = x,
                                                 to = y,
                                                 unit = units,
                                                 num = num,
                                                 seq_type = type)
    if (is_special_case_days && !as_period){
      return(as.double(difftime(y, x, units = units)) / num)
    }
    if (type == "auto") type <- guess_seq_type(units)
    if (as_period || type == "period"){
      int <- lubridate::interval(x, y)
      if (as_period){
        int <- lubridate::as.period(int, unit = units)
      }
      unit <- period_unit(units)(abs(num)) # Vectorised lubridate::period
      out <- sign(num) * (int / unit)
    } else {
      unit <- duration_unit(units)(num)
      out <- as.double(unclass(as.POSIXct(y)) - unclass(as.POSIXct(x))) / as.double(unit)
      # out <- int / unit
    }
  } else {
    out <- (y - x) / unlist(unname(by))
  }
  out
}
