#' Cut dates and datetimes into regularly spaced date or datetime intervals
#'
#' @description
#' Useful functions especially for when plotting time-series.
#' `time_cut` makes approximately `n` groups of equal time range.
#' It prioritises the highest time unit possible, making axes look
#' less cluttered and thus prettier. `time_breaks` returns only the breaks.
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
#'
#' `time_cut` is a generalisation of `time_summarisev` such that the
#' below identity should always hold:
#' \preformatted{
#'  identical(time_cut(x, n = Inf, as_factor = FALSE), time_summarisev(x))
#' }
#' Or also:
#' \preformatted{
#'  breaks <- time_breaks(x, n = Inf)
#'  identical(breaks[unclass(time_cut(x, n = Inf))], time_summarisev(x))
#' }
#'
#' @param x Time variable. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`, `yearmon`, or `yearqtr`.
#' @param n Number of breaks.
#' @param time_by Time unit. \cr
#' Must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param from Time series start date.
#' @param to Time series end date.
#' @param time_floor Logical. Should the initial date/datetime be
#' floored before building the sequence?
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `time_floor = TRUE`.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param as_interval Should result be a `time_interval`?
#' Default is `FALSE`. \cr
#' This can be controlled globally through `options(timeplyr.use_intervals)`.
#'
#' @returns
#' `time_breaks` returns a vector of breaks. \cr
#' `time_cut` returns either a vector or `time_interval`. \cr
#'
#' @examples
#' library(timeplyr)
#' library(fastplyr)
#' library(lubridate)
#' library(ggplot2)
#' library(dplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' time_cut(1:10, n = 5)
#' # Easily create custom time breaks
#' df <- nycflights13::flights %>%
#'   f_slice_sample(n = 10, seed = 8192821) %>%
#'   select(time_hour) %>%
#'   fastplyr::f_arrange(time_hour) %>%
#'   mutate(date = as_date(time_hour))
#'
#' # time_cut() and time_breaks() automatically find a
#' # suitable way to cut the data
#' options(timeplyr.use_intervals = TRUE)
#' time_cut(df$date)
#' # Works with datetimes as well
#' time_cut(df$time_hour, n = 5) # <= 5 breaks
#' # Custom formatting
#' options(timeplyr.interval_sub_formatter =
#'           function(x) format(x, format = "%Y %b"))
#' time_cut(df$date, time_by = "month")
#' # Just the breaks
#' time_breaks(df$date, n = 5, time_by = "month")
#'
#' cut_dates <- time_cut(df$date)
#' date_breaks <- time_breaks(df$date)
#'
#' # WHen n = Inf and as_factor = FALSE, it should be equivalent to using
#' # time_aggregate or time_summarisev
#' identical(time_cut(df$date, n = Inf, time_by = "month"),
#'           time_summarisev(df$date, time_by = "month"))
#' identical(time_summarisev(df$date, time_by = "month"),
#'           time_aggregate(df$date, time_by = "month"))
#'
#' # To get exact breaks at regular intervals, use time_expandv
#' weekly_breaks <- time_expandv(df$date,
#'                               time_by = "5 weeks",
#'                               week_start = 1, # Monday
#'                               time_floor = TRUE)
#' weekly_labels <- format(weekly_breaks, "%b-%d")
#' df %>%
#'   time_by(date, time_by = "week", .name = "date") %>%
#'   count() %>%
#'   mutate(date = interval_start(date)) %>%
#'   ggplot(aes(x = date, y = n)) +
#'   geom_bar(stat = "identity") +
#'   scale_x_date(breaks = weekly_breaks,
#'                labels = weekly_labels)
#' reset_timeplyr_options()
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname time_cut
#' @export
time_cut <- function(x, n = 5, time_by = NULL,
                     from = NULL, to = NULL,
                     time_floor = FALSE,
                     week_start = getOption("lubridate.week.start", 1),
                     time_type = getOption("timeplyr.time_type", "auto"),
                     roll_month = getOption("timeplyr.roll_month", "preday"),
                     roll_dst = getOption("timeplyr.roll_dst", "NA"),
                     as_interval = getOption("timeplyr.use_intervals", TRUE)){
  if (is.null(to)){
    to <- collapse::fmax(x, na.rm = TRUE)
  }
  breaks_list <- .time_breaks(x = x, n = n, time_by = time_by,
                              from = from, to = to,
                              time_floor = time_floor,
                              week_start = week_start,
                              time_type = time_type,
                              roll_month = roll_month,
                              roll_dst = roll_dst)
  time_breaks <- breaks_list[["breaks"]]
  x <- time_cast(x, time_breaks)
  to <- time_cast(to, x)
  out <- cut_time(x, breaks = c(unclass(time_breaks), unclass(to)), codes = FALSE)
  if (as_interval){
    out <- time_by_interval(out, time_by = breaks_list[["time_by"]],
                            time_type = time_type,
                            roll_month = roll_month, roll_dst = roll_dst)
  }
  # time_labels <- tseq_levels(x = to, time_breaks, fmt = NULL)
  # levels(out) <- time_labels
  # class(out) <- c("ordered", "factor")
  out
}
#' @rdname time_cut
#' @export
time_breaks <- function(x, n = 5, time_by = NULL,
                        from = NULL, to = NULL,
                        time_floor = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        time_type = getOption("timeplyr.time_type", "auto"),
                        roll_month = getOption("timeplyr.roll_month", "preday"),
                        roll_dst = getOption("timeplyr.roll_dst", "NA")){
  out <- .time_breaks(x, n = n, time_by = time_by,
                      from = from, to = to,
                      time_floor = time_floor,
                      week_start = week_start,
                      time_type = time_type,
                      roll_month = roll_month,
                      roll_dst = roll_dst)
  out[["breaks"]]
}
# time_cut_width <- function(x, time_by = NULL,
#                            from = NULL, as_interval = getOption("timeplyr.use_intervals", TRUE)){
#   check_is_time_or_num(x)
#   time_by <- time_by_get(x, time_by)
#   if (length(from) <= 1 &&
#       time_span_size(x, time_by, from = from) <= 5e05){
#     return(time_summarisev(
#       x, time_by = time_by, from = from,
#       as_interval = as_interval
#     ))
#   }
#   num <- time_by_num(time_by)
#   units <- time_by_unit(time_by)
#   if (is.null(from)){
#     index <- gmin(x, na.rm = TRUE)
#   } else {
#     if (length(from) %!in_% c(1, length(x))){
#       stop("length of from must be 1 or length(x)")
#     }
#     index <- time_cast(from, x)
#     x[which(x < index)] <- NA
#   }
#   tdiff <- time_diff(index, x, time_by = time_by)
#   time_to_add <- add_names(list(trunc2(tdiff) * num), units)
#   out <- time_add2(index, time_by = time_to_add)
#   if (as_interval){
#    out <- time_by_interval(out, time_by = time_by)
#   }
#   out
# }
.time_breaks <- function(x, n = 5, time_by = NULL,
                         from = NULL, to = NULL,
                         time_floor = FALSE,
                         week_start = getOption("lubridate.week.start", 1),
                         time_type = getOption("timeplyr.time_type", "auto"),
                         roll_month = getOption("timeplyr.roll_month", "preday"),
                         roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  check_is_time_or_num(x)
  check_is_num(n)
  stopifnot(n >= 1)
  check_length(n, 1L)
  time_type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
  if (is.null(from)){
    from <- collapse::fmin(x, na.rm = TRUE)
  }
  if (is.null(to)){
    to <- collapse::fmax(x, na.rm = TRUE)
  }
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  if (is.null(time_by)){
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
        time_types <- rep_len("period", length(date_units))
        if (is_datetime(x)){
          datetime_units <- setdiff(.duration_units, date_units)
          units_to_try <- c(datetime_units, date_units)
          time_types <- c(rep_len("duration", length(datetime_units)), time_types)
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
      units_to_try <- rep_len("numeric", max(length(interval_width), 1))
    }
    i <- 0L
    start <- from
    while(i < length(units_to_try)){
      i <- i + 1L
      tby <- add_names(list(interval_width[i]), units_to_try[i])
      if (time_floor){
        start <- time_floor2(from, time_by = tby, week_start = week_start)
      }
      n_breaks <- time_seq_sizes(start, to, time_by = tby, time_type = time_type)
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
    if (time_type == "auto"){
      time_type <- guess_seq_type(unit)
    }
  } else {
    unit_info <- unit_guess(time_by)
    unit <- unit_info[["unit"]]
    scale <- unit_info[["scale"]]
    num <- unit_info[["num"]]
    by <- unit
    tby <- add_names(list(num * 1L * scale), unit)
    if (time_floor){
      from <- time_floor2(from, time_by = tby, week_start = week_start)
    }
    n_breaks <- time_seq_sizes(from, to, time_by = tby, time_type = time_type)
    unit_multiplier <- 1L
  }
  if (n_breaks > n){
    unit_multiplier <- ceiling(n_breaks / n)
  }
  time_increment <- add_names(list(num * scale * unit_multiplier), unit)
  breaks <- time_seq_v(from, to,
                       time_by = time_increment,
                       time_floor = FALSE,
                       week_start = week_start,
                       time_type = time_type,
                       roll_month = roll_month,
                       roll_dst = roll_dst)
  list(
    breaks = breaks,
    time_by = time_increment,
    time_type = time_type
  )
}
