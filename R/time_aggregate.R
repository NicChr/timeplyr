#' Aggregate time to a higher unit
#'
#' @description Aggregate time to a higher unit for possibly many groups
#' with respect to a time index.
#'
#' @param x Time vector. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`,
#' `yearmon`, or `yearqtr` vector.
#' @param time_by Time unit. \cr
#' Must be one of the following:
#' * string, e.g `time_by = "day"` or `time_by = "2 weeks"`
#' * lubridate duration or period object, e.g. `days(1)` or `ddays(1)`.
#' * named list of length one, e.g. `list("days" = 7)`.
#' * Numeric vector, e.g. `time_by = 7`.
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param direction Direction with which to aggregate time,
#' "l2r" ("left-to-right") or "r2l" ("right-to-left").
#' If "l2r" (the default), then the minimum time is used as the
#' reference time, otherwise the maximum time is used.
#' @details `time_aggregate` aggregates time using
#' distinct moving time range blocks of a specified time unit.
#'
#' The actual calculation is extremely simple and essentially requires
#' a subtraction, a rounding and an addition.
#'
#'
#' If for example `time_by = "week"` then all dates or datetimes
#' will be shifted backwards (or forwards if direction is "r2l") to the
#' nearest start of the week, where the start of week is based on `min(x)`.
#' This is identical to building a weekly sequence and using this as
#' breakpoints to cut `x`. No time expansion occurs so this is very efficient
#' except when `periods` are used and there is a lot of data.
#' In this case, provided the expansion is not too big,
#' it may be more efficient to cut the data using the period sequence which can
#' be achieved using `time_summarisev`.
#'
#' @seealso [time_summarisev]
#' @returns
#' A time aggregated vector the same class and length as `x`.
#'
#' @examples
#' library(timeplyr)
#' library(nycflights13)
#' library(lubridate)
#' library(dplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' sunique <- function(x) sort(unique(x))
#'
#' hours <- sunique(flights$time_hour)
#' days <- as_date(hours)
#'
#' # Aggregate by week or any time unit easily
#' unique(time_aggregate(hours, "week"))
#' unique(time_aggregate(hours, ddays(14)))
#' unique(time_aggregate(hours, "month"))
#' unique(time_aggregate(days, "month"))
#'
#' # Left aligned
#' unique(time_aggregate(days, "quarter"))
#' # Right aligned
#' unique(time_aggregate(days, "quarter", direction = "r2l"))
#'
#' # Very fast by group aggregation
#' week_by_tailnum <- time_aggregate(flights$time_hour, time_by = ddays(7),
#'                                   g = flights$tailnum)
#' # Confirm this has been done by group as each group will have a
#' # Different aggregate start date
#' flights %>%
#'   mutate(week_by_tailnum) %>%
#'   stat_summarise(week_by_tailnum, .by = tailnum, stat = "min",
#'                  sort = FALSE)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
time_aggregate <- function(x, time_by = NULL, g = NULL,
                           time_type = c("auto", "duration", "period"),
                           roll_month = "preday", roll_dst = "pre",
                           direction = c("l2r", "r2l")){
  check_is_time_or_num(x)
  if (is.null(time_by)){
    return(x)
  }
  direction <- rlang::arg_match0(direction, c("l2r", "r2l"))
  l2r <- direction == "l2r"
  time_by <- time_by_list(time_by)
  num <- time_by_num(time_by)
  units <- time_by_unit(time_by)
  if (l2r){
    index <- gmin(x, g = g, na.rm = TRUE)
  } else {
    index <- gmax(x, g = g, na.rm = TRUE)
  }
  tdiff <- time_diff(index, x, time_by = time_by, time_type = time_type)
  time_to_add <- add_names(list(trunc(tdiff) * num), units)
  time_add2(index, time_by = time_to_add, time_type = time_type,
            roll_month = roll_month, roll_dst = roll_dst)
}
