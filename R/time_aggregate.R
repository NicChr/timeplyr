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
#' @param from Start.
#' @param to End.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @param time_floor Should `from` be floored to the nearest unit specified
#' through the `time_by` argument?
#' This is particularly useful for starting sequences at the
#' beginning of a week or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `time_floor = TRUE`.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param as_interval Should result be a `time_interval`?
#' Default is `TRUE`. \cr
#' This can be controlled globally through `options(timeplyr.use_intervals)`.
#'
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
#' @seealso [time_summarisev] [time_cut]
#'
#' @returns
#' A `time_interval`.
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
#' sunique(time_aggregate(hours, "week"))
#' sunique(time_aggregate(hours, ddays(14)))
#' sunique(time_aggregate(hours, "month"))
#' sunique(time_aggregate(days, "month"))
#'
#' # Left aligned
#' sunique(time_aggregate(days, "quarter"))
#'
#' # Very fast by group aggregation
#' week_by_tailnum <- time_aggregate(flights$time_hour, time_by = ddays(7),
#'                                   g = flights$tailnum)
#' # Confirm this has been done by group as each group will have a
#' # Different aggregate start date
#' flights %>%
#'   stat_summarise(week_by_tailnum, .by = tailnum, stat = "min",
#'                  sort = FALSE)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
time_aggregate <- function(x, time_by = NULL, g = NULL,
                           from = NULL, to = NULL,
                           time_type = getOption("timeplyr.time_type", "auto"),
                           roll_month = getOption("timeplyr.roll_month", "preday"),
                           roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                           time_floor = FALSE,
                           week_start = getOption("lubridate.week.start", 1),
                           as_interval = getOption("timeplyr.use_intervals", FALSE)){
  check_is_time_or_num(x)
  time_by <- time_by_get(x, time_by = time_by)
  ## This uses a breakpoints-cut method which is much more efficient
  ## when there are relatively small numbers of breaks and large data
  if (time_type == "period" &&
      time_by_unit(time_by) %!in_% c("seconds", "minutes", "hours") &&
      is.null(g)){
    return(time_summarisev(
      x, time_by = time_by,
      from = from, to = to,
      time_type = time_type,
      roll_month = roll_month,
      roll_dst = roll_dst,
      time_floor = time_floor,
      week_start = week_start,
      as_interval = as_interval
    ))
  }
  num <- time_by_num(time_by)
  units <- time_by_unit(time_by)
  if (is.null(from)){
    index <- gmin(x, g = g, na.rm = TRUE)
  } else {
    if (length(from) %!in_% c(1, length(x))){
      stop("length of from must be 1 or length(x)")
    }
    index <- time_cast(from, x)
    x[x < index] <- NA
  }
  if (!is.null(to)){
    if (length(to) %!in_% c(1, length(x))){
      stop("length of to must be 1 or length(x)")
    }
    to <- time_cast(to, x)
    x[x > to] <- NA
  }
  if (time_floor){
    from <- time_floor2(from, time_by = time_by, week_start = week_start)
  }
  tdiff <- time_diff(index, x, time_by = time_by, time_type = time_type)
  time_to_add <- add_names(list(trunc2(tdiff) * num), units)
  out <- time_add2(index, time_by = time_to_add, time_type = time_type,
                   roll_month = roll_month, roll_dst = roll_dst)
  if (as_interval){
    out <- time_by_interval(out, time_by = time_by,
                            time_type = time_type,
                            roll_month = roll_month,
                            roll_dst = roll_dst)
  }
  out
}
