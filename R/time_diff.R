#' Calculate time difference
#'
#' @description This is a neat wrapper around lubridate's
#' date/datetime arithmetic with support for numeric values and
#' any class that supports arithmetic through "base::`-`"
#' If either x or y are date/datetimes and time_type is "duration" or "period",
#' then lubridate arithmetic is used, otherwise base R arithmetic is used.
#'
#' Some more exotic time units such as quarters, fortnights, etc
#' can be specified.
#'
#' @param x Start date or datetime.
#' @param y End date or datetime.
#' @param time_by Must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param time_type Time difference type: "auto", "duration" or "period".
#' @param as_period Logical. Should time interval be coerced to a period
#' before time difference is calculated? This is useful for calculating
#' for example age in exact years or months.
#' @param by \bold{Deprecated}. Use `time_by` instead
#' @param type \bold{Deprecated}. Use `time_type` instead.
#' @examples
#' library(timeplyr)
#' library(lubridate)
#' time_diff(today(), today() + days(10),
#'           time_by = "days")
#' time_diff(today(), today() + days(0:100),
#'           time_by = "days")
#' time_diff(today(), today() + days(100),
#'           time_by = list("days" = 1:100))
#' time_diff(1, 1 + 0:100, time_by = 3)
#' @export
time_diff <- function(x, y, time_by,
                      time_type = c("auto", "duration", "period"),
                      as_period = FALSE,
                      by = NULL,
                      type = NULL){
  if (!is.null(by)){
    warning("by is deprecated, use time_by instead")
    time_by <- by
  }
  if (!is.null(type)){
    warning("type is deprecated, use time_type instead")
    time_type <- type
  }
  if (is_time(x) && is_time(y)){
    time_type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
    tby <- time_by_list(time_by)
    units <- names(tby)
    num <- tby[[1L]]
    # Common but special case where from/to are whole days
    # and time_type is "auto"
    is_special_case_days <- is_special_case_days(from = x,
                                                 to = y,
                                                 unit = units,
                                                 num = num,
                                                 time_type = time_type)
    if (is_special_case_days && !as_period){
      if (units == "weeks"){
        num <- num * 7
      }
      return(( as.double(y) - as.double(x) ) / num)
    }
    if (time_type == "auto") time_type <- guess_seq_type(units)
    if (as_period || time_type == "period"){
      int <- lubridate::interval(x, y)
      if (as_period){
        int <- lubridate::as.period(int, unit = units)
      }
      unit <- period_unit(units)(abs(num)) # Vectorised lubridate::period
      out <- sign(num) * (int / unit)
      # out[is.infinite(num)] <- 0
      out[num == 0 & x > y] <- -Inf
      out[num == 0 & x < y] <- Inf
    } else {
      # unit <- duration_unit(units)(num)
      x <- as.double(as_datetime2(x))
      y <- as.double(as_datetime2(y))
      out <- (y - x) / unit_to_seconds(tby)
      # out <- int / unit
    }
  } else {
    by <- unlist(time_by, use.names = FALSE, recursive = FALSE)
    out <- (y - x) / by
  }
  out
}
