#' Cut dates and datetimes into regularly spaced date or datetime intervals
#'
#' @description  `time_cut()` is very useful for plotting with dates and datetimes
#' and always returns breaks of regular width. \cr
#'
#' @details
#' To specify exact widths, similar to `ggplot2::cut_width()`,
#' supply `time_by` and `n = Inf`. \cr
#' `time_breaks()` is a helper that
#' returns only the time breaks.
#'
#' By default `time_cut()` will try to find
#'  the prettiest way of cutting the interval by
#' trying to cut the date/date-times into
#' groups of the highest possible time units,
#' starting at years and ending at milliseconds.
#'
#' When `x` is a numeric vector, `time_cut` will behave similar to `time_cut`
#' except for 3 things:
#' * The intervals are all right open and equal width, except for the
#' rightmost interval which is closed with width <= the other widths.
#' * The left value of the leftmost interval is always `min(x)`.
#' * Up to `n` breaks are created, i.e `<= n` breaks. This is to prioritise
#'   pretty breaks.
#'
#' `time_cut` is a generalisation of `time_summarisev` such that the
#' below identity should always hold:
#' \preformatted{
#'  identical(time_cut(x, n = Inf, as_factor = FALSE), time_summarisev(x))
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
#' @param fmt (Optional) Date/datetime format for the factor labels.
#' If supplied, this is passed to `format()`.
#' @param time_floor Logical. Should the initial date/datetime be
#' floored before building the sequence?
#' @param n_at_most \bold{Deprecated}. No longer used.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `time_floor = TRUE`.
#' @param as_factor Logical. If `TRUE` the output is an ordered factor.
#' Setting this to `FALSE` is sometimes much faster.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#'
#' @returns
#' `time_breaks` returns a vector of breaks. \cr
#' `time_cut` returns either a `factor` or a vector the same class as `x`.
#' In both cases it is the same length as `x`.
#'
#' @examples
#' library(timeplyr)
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
#'   fslice_sample(n = 10, seed = 8192821) %>%
#'   select(time_hour) %>%
#'   farrange(time_hour) %>%
#'   mutate(date = as_date(time_hour))
#'
#' # time_cut() and time_breaks() automatically find a
#' # suitable way to cut the data
#' time_cut(df$date)
#' # Works with datetimes as well
#' time_cut(df$time_hour, n = 5) # <= 5 breaks
#' # Custom formatting
#' time_cut(df$date, fmt = "%Y %b", time_by = "month")
#' # Just the breaks
#' time_breaks(df$date, n = 5, time_by = "month")
#'
#' cut_dates <- time_cut(df$date)
#' date_breaks <- time_breaks(df$date)
#'
#' # Grouping each interval into the start of its interval
#' identical(date_breaks[group_id(cut_dates)],
#'           time_cut(df$date, as_factor = FALSE))
#'
#' # WHen n = Inf and as_factor = FALSE, it should be equivalent to using
#' # time_aggregate or time_summarisev
#' identical(time_cut(df$date, n = Inf, time_by = "month", as_factor = FALSE),
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
#'   time_count(time = date, time_by = "week") %>%
#'   ggplot(aes(x = date, y = n)) +
#'   geom_bar(stat = "identity") +
#'   scale_x_date(breaks = weekly_breaks,
#'                labels = weekly_labels)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname time_cut
#' @export
time_cut <- function(x, n = 5, time_by = NULL,
                     from = NULL, to = NULL,
                     fmt = NULL,
                     time_floor = FALSE,
                     week_start = getOption("lubridate.week.start", 1),
                     n_at_most = TRUE, as_factor = TRUE,
                     time_type = getOption("timeplyr.time_type", "auto"),
                     roll_month = getOption("timeplyr.roll_month", "preday"),
                     roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  if (is.null(to)){
    to <- collapse::fmax(x, na.rm = TRUE)
  }
  time_breaks <- time_breaks(x = x, n = n, time_by = time_by,
                             from = from, to = to,
                             time_floor = time_floor,
                             week_start = week_start,
                             time_type = time_type,
                             roll_month = roll_month,
                             roll_dst = roll_dst)
  x <- time_cast(x, time_breaks)
  to <- time_cast(to, x)
  out <- cut_time(x,
                  breaks = c(unclass(time_breaks), unclass(to)),
                  codes = as_factor)
  if (as_factor){
    time_labels <- tseq_levels(x = to, time_breaks, fmt = fmt)
    levels(out) <- time_labels
    class(out) <- c("ordered", "factor")
  }
  out
}
#' @rdname time_cut
#' @export
time_breaks <- function(x, n = 5, time_by = NULL,
                        from = NULL, to = NULL,
                        time_floor = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        n_at_most = TRUE,
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
    if (length(x) <= 1){
      gcd_difference <- 1L
    } else {
      gcd_difference <- abs(gcd_diff(x, round = TRUE))
    }
    gcd_difference[is.na(gcd_difference)] <- 1L
    time_rng_diff <- unclass(to) - unclass(from)
    # We shouldn't try to cut up the data using more breaks than this
    max_breaks <- (time_rng_diff %/% gcd_difference) + 1
    if (length(max_breaks) == 0){
      max_breaks <- 0
    }
    if (is_time(x)){
      if (n >= max_breaks){
        interval_width <- gcd_difference
        units_to_try <- rep_len("numeric", max(length(interval_width), 1))
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
  time_seq_v(from, to,
             time_by = add_names(list(num * scale * unit_multiplier), unit),
             time_floor = FALSE,
             week_start = week_start,
             time_type = time_type,
             roll_month = roll_month,
             roll_dst = roll_dst)
}
