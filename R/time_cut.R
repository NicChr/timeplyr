#' Cut dates and datetimes into regularly spaced date or datetime intervals
#'
#' @description  `time_cut()` is very useful for plotting with dates and datetimes
#' and always returns breaks of regular width. \cr
#' To specify exact widths, similar to `ggplot2::cut_width()`,
#' supply `time_by` and `n = Inf`. \cr
#' `time_breaks()` is a helper that
#' returns only the time breaks.
#'
#' By default `time_cut()` will try to find
#'  the prettiest way of cutting the interval by
#' trying to cut the date/datetimes into
#' groups of the highest possible time units,
#' starting at years and ending at milliseconds.
#' If `n_at_most = TRUE` then `<= n` groups are calculated,
#' otherwise `>= n` groups
#' are calculated.
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
#' @param n_at_most Logical. If `TRUE` then n breaks at most are returned,
#' otherwise at least n breaks are returned.
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
#' @returns
#' `time_breaks` returns a vector of breaks. \cr
#' `time_cut` returns either a `factor` or a vector the same class as `x`.
#' In both cases it is the same length as `x`.
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
#' # Easily create custom time breaks
#' df <- nycflights13::flights %>%
#'   fslice_sample(n = 10^3, seed = 8192821) %>%
#'   select(time_hour) %>%
#'   mutate(date = as_date(time_hour))
#'
#' # time_cut() and time_breaks() automatically find a
#' # suitable way to cut the data
#' time_cut(df$date)
#' # Works with datetimes as well
#' time_cut(df$time_hour, n = 5) # <= 5 breaks
#' # Custom formatting
#' time_cut(df$date, fmt = "%Y %b", time_by = "month")
#' time_cut(df$time_hour, fmt = "%Y %b", time_by = "month")
#' # Just the breaks
#' time_breaks(df$date, n = 5, time_by = "month")
#' time_breaks(df$time_hour, n = 5, time_by = "month")
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
  time_breaks <- time_breaks(x = x, n = n, time_by = time_by,
                             from = from, to = to,
                             time_floor = time_floor,
                             week_start = week_start,
                             n_at_most = n_at_most,
                             time_type = time_type,
                             roll_month = roll_month,
                             roll_dst = roll_dst)
  to <- bound_to(to, x)
  out <- cut_time(x,
                  breaks = c(time_as_number(time_breaks),
                             time_as_number(to)),
                  codes = TRUE)
  if (as_factor){
    time_labels <- tseq_levels(x = to, time_breaks, fmt = fmt)
    levels(out) <- time_labels
    class(out) <- c("ordered", "factor")
  } else {
    out <- time_breaks[out]
  }
  out
}
#' @rdname time_cut
#' @export
time_breaks <- function(x, n = 5, time_by = NULL,
                        from = NULL, to = NULL,
                        time_floor = FALSE, week_start = getOption("lubridate.week.start", 1),
                        n_at_most = TRUE,
                        time_type = getOption("timeplyr.time_type", "auto"),
                        roll_month = getOption("timeplyr.roll_month", "preday"), roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  check_is_time_or_num(x)
  check_is_num(n)
  stopifnot(n >= 1)
  check_length(n, 1L)
  time_type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
  from <- bound_from(from, x)
  to <- bound_to(to, x)
  n_unique <- n_unique(x, na.rm = TRUE)
  if (is.null(time_by)){
    if (is_time(x)){
      date_units <- c("days", "weeks", "months", "years")
      units_to_try <- date_units
      time_types <- rep_len("period", length(date_units))
      if (is_datetime(x)){
        datetime_units <- setdiff(.duration_units, date_units)
        units_to_try <- c(datetime_units, date_units)
        time_types <- c(rep_len("duration", length(datetime_units)), time_types)
      }
      units_to_try <- rev(units_to_try)
      unit_nums <- rep_len(1L, length(units_to_try))
    } else {
      # Calculate range of data
      time_rng <- collapse::frange(x, na.rm = TRUE)
      time_rng_diff <- diff(time_rng)
      if (length(time_rng) == 0L || isTRUE(cppdoubles::double_equal(time_rng_diff, 0))){
        unit_nums <- 1
      } else {
        # Multiply gcd by 10 until range of data
        # Another option..
        unit_nums <- 10^(seq.int(log10(time_rng_diff/n),
                                 log10(time_rng_diff), by = 1))
        # Continue multiplying by 5 until range
        unit_nums <- c(unit_nums, 5^(seq.int(logb(max(unit_nums), 5),
                                             logb(time_rng_diff, 5),
                                             by = 1)))
        # Continue multiplying by 2 until range
        unit_nums <- c(unit_nums, 2^(seq.int(round(logb(max(unit_nums), 2), 7),
                                             round(logb(time_rng_diff, 2), 7),
                                             by = 1)))
        # Round the numbers off due to loss of precision
        unit_nums <- round(unit_nums, 6)
        if (time_rng_diff >= 3){
          unit_nums <- ceiling(unit_nums)
        }
        unit_nums <- rev(unique(pretty_ceiling(unit_nums)))
      }
      units_to_try <- rep_len("numeric", length(unit_nums))
    }
    i <- 0
    start <- from
    while(i < length(units_to_try)){
      i <- i + 1
      tby <- add_names(list(unit_nums[i]),
                      units_to_try[i])
      if (time_floor){
        start <- time_floor2(from, time_by = tby, week_start = week_start)
      }
      n_breaks <- time_seq_sizes(start, to, time_by = tby,
                                 time_type = time_type)
      if (n_breaks >= n){
        break
      }
    }
    from <- start
    unit <- units_to_try[i]
    time_by <- unit
    unit_multiplier <- 1
    scale <- 1
    num <- unit_nums[i]
    if (time_type == "auto"){
      time_type <- guess_seq_type(unit)
    }
  } else {
    unit_info <- unit_guess(time_by)
    unit <- unit_info[["unit"]]
    scale <- unit_info[["scale"]]
    num <- unit_info[["num"]]
    by <- unit
    tby <- add_names(list(1 * scale * num),
                    unit)
    if (time_floor){
      from <- time_floor2(from, time_by = tby, week_start = week_start)
    }
    n_breaks <- time_seq_sizes(from, to, time_by = tby,
                               time_type = time_type)
    unit_multiplier <- 1
  }
  if (n_breaks > n){
    unit_multiplier <- (n_breaks / n)
    if (n_at_most){
      unit_multiplier <- ceiling(unit_multiplier)
    } else {
      unit_multiplier <- floor(unit_multiplier)
    }
  }
  time_seq_v(from, to,
                time_by = add_names(list(num * scale * unit_multiplier), unit),
                time_floor = FALSE,
                week_start = week_start,
                time_type = time_type,
                roll_month = roll_month,
                roll_dst = roll_dst)
}
