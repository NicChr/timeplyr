#' Time differences by any time unit
#'
#' @description
#' The time difference between 2 date or date-time vectors.
#'
#' @param x Start date or datetime.
#' @param y End date or datetime.
#' @param time_by Must be one of the three (Default is 1):
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param time_type Time difference type: "auto", "duration" or "period".
#'
#' @returns
#' A numeric vector recycled to the length of `max(length(x), length(y))`.
#'
#' @details
#' When `time_by` is a numeric vector, e.g `time_by = 1` then
#' base arithmetic using \code{base::`-`} is used, otherwise 'lubridate' style
#' durations and periods are used.\cr
#' Some more exotic time units such as quarters, fortnights, etcetera
#' can be specified. See `.time_units` for more choices.
#'
#' @examples
#' library(timeplyr)
#' library(lubridate)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' time_diff(today(), today() + days(10),
#'           time_by = "days")
#' time_diff(today(), today() + days((0:3) * 7),
#'           time_by = weeks(1))
#' time_diff(today(), today() + days(100),
#'           time_by = list("days" = 1:100))
#' time_diff(1, 1 + 0:100, time_by = 3)
#' \donttest{
#' library(nycflights13)
#' library(bench)
#'
#' # Period differences are much faster
#' # check = FALSE because the results are fractionally different.
#' # lubridate:::adjust_estimate likely has a typo in the first while loop
#'
#' mark(timeplyr = time_diff(flights$time_hour, today(), "weeks", time_type = "period"),
#'      lubridate = interval(flights$time_hour, today()) / weeks(1),
#'      check = FALSE)
#' }
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#' }
#' @export
time_diff <- function(x, y, timespan = 1L){
  span <- timespan(timespan)
  units <- timespan_unit(span)
  num <- timespan_num(span)

  if (!timespan_has_unit(span)){
    set_time_cast(y, x)
    if (!is.numeric(y)){
      y <- unclass(y)
    }
    if (!is.numeric(x)){
      x <- unclass(x)
    }
    out <- divide(y - x, num)
  } else {
    x <- as_datetime2(x)
    y <- as_datetime2(y)
    if (!is_duration_unit(units)){
      # Use distinct start/end pairs (intervals)
      # Instead of all of them because it's usually more efficient
      interval_tbl <- new_df(x = x, y = y, num = num, .recycle = TRUE)
      interval_groups <- collapse::group(interval_tbl, starts = TRUE, group.sizes = TRUE)
      starts <- attr(interval_groups, "starts")
      sizes <- attr(interval_groups, "group.sizes")
      n_groups <- attr(interval_groups, "N.groups")
      # If distinct pairs results in a 2x reduction in data size, then we do that
      distinct_pairs <- isTRUE((df_nrow(interval_tbl) %/% n_groups) >= 2L)
      if ( distinct_pairs ){
        interval_tbl <- df_row_slice(interval_tbl, starts)
      }
      x <- interval_tbl$x
      y <- interval_tbl$y
      num <- interval_tbl$num

      unit <- period_unit(units)(abs(num))
      out <- sign(num) * divide_interval_by_period2(x, y, unit)
      out[which(num == 0 & x > y)] <- -Inf
      out[which(num == 0 & x < y)] <- Inf
      # Expand them back to original length
      if (distinct_pairs){
        out <- out[interval_groups]
      }
    } else {
      x <- unclass(x)
      y <- unclass(y)
      by <- unit_to_seconds(span)
      out <- (y - x) / by
    }
  }
  strip_attrs(out)
}
