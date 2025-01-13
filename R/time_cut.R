# Low-level binning function for right-open intervals only
cut_time <- function(x, breaks, include_oob = TRUE, codes = FALSE){
  cheapr::bin(x, breaks, codes = codes, left_closed = TRUE,
              include_oob = include_oob,
              include_endpoint = FALSE)
}

.time_breaks <- function(x, n = 5, timespan = NULL,
                         from = NULL, to = NULL,
                         time_floor = FALSE,
                         week_start = getOption("lubridate.week.start", 1)){
  check_is_time_or_num(x)
  check_is_num(n)
  stopifnot(n >= 1)
  check_length(n, 1L)
  if (is.null(from)){
    from <- collapse::fmin(x, na.rm = TRUE)
  }
  if (is.null(to)){
    to <- collapse::fmax(x, na.rm = TRUE)
  }
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  if (is.null(timespan)){
    gcd_difference <- gcd_time_diff(x)
    time_rng_diff <- unclass(to) - unclass(from)
    # We shouldn't try to cut up the data using more breaks than this
    max_breaks <- (time_rng_diff %/% gcd_difference) + 1
    if (length(max_breaks) == 0){
      max_breaks <- 0
    }
    if (is_time(x)){
      if (n >= max_breaks){
        interval_width <- gcd_difference
        units_to_try <- rep_len(get_time_unit(x), max(length(interval_width), 1))
      } else {
        date_units <- c("days", "weeks", "months", "years")
        units_to_try <- date_units
        if (is_datetime(x)){
          datetime_units <- setdiff(.duration_units, date_units)
          units_to_try <- c(datetime_units, date_units)
        }
        units_to_try <- rev(units_to_try)
        interval_width <- rep_len(1L, length(units_to_try))
      }
    } else {
      # Calculate range of data
      if (n >= max_breaks){
        interval_width <- gcd_difference
      } else {
        equal_bin_width <- time_rng_diff / min(n, max_breaks, na.rm = TRUE)
        interval_width <- pretty_ceiling(equal_bin_width)
      }
      if (is_whole_number(interval_width)){
        interval_width <- as.integer(interval_width)
      }
      units_to_try <- rep_len(NA_character_, max(length(interval_width), 1))
    }
    i <- 0L
    start <- from
    while(i < length(units_to_try)){
      i <- i + 1L
      tby <- new_timespan(units_to_try[i], interval_width[i])
      if (time_floor){
        start <- time_floor(from, tby, week_start = week_start)
      }
      n_breaks <- time_seq_sizes(start, to, tby)
      if (length(n_breaks) == 0){
        n_breaks <- 0
      }
      if (n_breaks == 0 || n_breaks >= n){
        break
      }
    }
    from <- start
    unit <- units_to_try[i]
    time_by <- unit
    unit_multiplier <- 1L
    scale <- 1L
    num <- interval_width[i]
  } else {
    tby <- timespan(timespan)
    unit <- timespan_unit(tby)
    num <- timespan_num(tby)
    scale <- 1L
    by <- unit
    if (time_floor){
      from <- time_floor(from, tby, week_start = week_start)
    }
    n_breaks <- time_seq_sizes(from, to, tby)
    unit_multiplier <- 1L
  }
  if (n_breaks > n){
    unit_multiplier <- ceiling(n_breaks / n)
  }
  time_increment <- new_timespan(unit, num * scale * unit_multiplier)
  breaks <- time_seq_v(from, to, time_increment)
  list(
    breaks = breaks,
    timespan = time_increment
  )
}

#' Cut dates and datetimes into regularly spaced date or datetime intervals
#'
#' @description
#' Useful functions especially for when plotting time-series.
#' `time_cut` makes approximately `n` groups of equal time range.
#' It prioritises the highest time unit possible, making axes look
#' less cluttered and thus prettier.
#' `time_breaks` returns only the breaks.
#'
#' @details
#' To retrieve regular time breaks that simply spans the range of `x`,
#' use `time_seq()` or `time_aggregate()`.
#' This can also be achieved in `time_cut()` by supplying `n = Inf`.
#'
#' By default `time_cut()` will try to find
#'  the prettiest way of cutting the interval by
#' trying to cut the date/date-times into
#' groups of the highest possible time units,
#' starting at years and ending at milliseconds.
#'
#' When `x` is a numeric vector, `time_cut` will behave similar to `time_cut`
#' except for 3 things:
#' * The intervals are all right-open and of equal width.
#' * The left value of the leftmost interval is always `min(x)`.
#' * Up to `n` breaks are created, i.e `<= n` breaks. This is to prioritise
#'   pretty breaks.
#' @inheritParams time_grid
#' @param n Number of breaks.
#' @param time_floor Logical. Should the initial date/datetime be
#' floored before building the sequence?
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `time_floor = TRUE`.
#'
#' @returns
#' `time_breaks` returns a vector of breaks. \cr
#' `time_cut` returns either a vector or `time_interval`. \cr
#'
#' @examples
#' library(timeplyr)
#' library(fastplyr)
#' library(cheapr)
#' library(lubridate)
#' library(ggplot2)
#' library(dplyr)
#' time_cut(1:10, n = 5)
#' # Easily create custom time breaks
#' df <- nycflights13::flights %>%
#'   f_slice_sample(n = 100) %>%
#'   with_local_seed(.seed = 8192821) %>%
#'   select(time_hour) %>%
#'   fastplyr::f_arrange(time_hour) %>%
#'   mutate(date = as_date(time_hour))
#'
#' # time_cut() and time_breaks() automatically find a
#' # suitable way to cut the data
#' time_cut(df$date)
#' # Works with datetimes as well
#' time_cut(df$time_hour, n = 5) # ~5 breaks
#' time_cut(df$date, timespan = "month")
#' # Just the breaks
#' time_breaks(df$date, n = 5, timespan = "month")
#'
#' cut_dates <- time_cut(df$date)
#' date_breaks <- time_breaks(df$date)
#'
#' # When n = Inf it should be equivalent to using time_cut_width
#' identical(time_cut(df$date, n = Inf, "month"),
#'           time_cut_width(df$date, "month"))
#' # To get exact breaks at regular intervals, use time_grid
#' weekly_breaks <- time_grid(
#'   df$date, "5 weeks",
#'   from = floor_date(min(df$date), "week", week_start = 1)
#' )
#' weekly_labels <- format(weekly_breaks, "%b-%d")
#' df %>%
#'   time_by(date, "week", .name = "date") %>%
#'   f_count() %>%
#'   mutate(date = interval_start(date)) %>%
#'   ggplot(aes(x = date, y = n)) +
#'   geom_bar(stat = "identity") +
#'   scale_x_date(breaks = weekly_breaks,
#'                labels = weekly_labels)
#' @rdname time_cut
#' @export
time_cut <- function(x, n = 5, timespan = NULL,
                     from = NULL, to = NULL,
                     time_floor = FALSE,
                     week_start = getOption("lubridate.week.start", 1)){
  lifecycle::deprecate_soft("1.0.0", "time_cut()", "time_cut_n()")
  time_cut_n(x, n = n, timespan = timespan,
             from = from, to = to,
             time_floor = time_floor,
             week_start = week_start)
}
#' @rdname time_cut
#' @export
time_cut_n <- function(x, n = 5, timespan = NULL,
                     from = NULL, to = NULL,
                     time_floor = FALSE,
                     week_start = getOption("lubridate.week.start", 1)){

  if (!is.null(to)){
    to <- time_cast(to, x)
    x[x >= to] <- NA
  }
  breaks_list <- .time_breaks(x = x, n = n, timespan = timespan,
                              from = from, to = to,
                              time_floor = time_floor,
                              week_start = week_start)
  time_breaks <- breaks_list[["breaks"]]
  timespan <- breaks_list[["timespan"]]
  x <- time_cast(x, time_breaks)
  out <- cut_time(
    x, breaks = time_breaks, codes = FALSE,
    include_oob = TRUE
  )
  time_interval(out, timespan)
}
#' @rdname time_cut
#' @export
time_cut_width <- function(x, timespan = granularity(x),
                           from = NULL, to = NULL){
  check_is_time_or_num(x)
  from_missing <- is.null(from)

  if (from_missing){
    from <- collapse::fmin(x, na.rm = TRUE)
  }
  if (!from_missing){
    from <- time_cast(from, x)
    x[x < from] <- NA
  }
  if (!is.null(to)){
    to <- time_cast(to, x)
    x[x >= to] <- NA
  }
  width <- timespan(timespan)
  num <- timespan_num(width)
  units <- timespan_unit(width)
  tdiff <- time_diff(from, x, width)
  time_to_add <- new_timespan(units, trunc2(tdiff) * num)
  out <- time_add(from, time_to_add)
  time_interval(out, width)
}
#' @rdname time_cut
#' @export
time_breaks <- function(x, n = 5, timespan = NULL,
                        from = NULL, to = NULL,
                        time_floor = FALSE,
                        week_start = getOption("lubridate.week.start", 1)){
  out <- .time_breaks(x, n = n, timespan = timespan,
                      from = from, to = to,
                      time_floor = time_floor,
                      week_start = week_start)
  out[["breaks"]]
}
