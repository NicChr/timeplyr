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
#' @param x A date/datetime.
#' @param n The minimum number of breaks.
#' @param time_by Must be one of the three:
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
#' This is only used when `floor_date = TRUE`.
#' @param as_factor Logical. If `TRUE` the output is an ordered factor.
#' Setting this to `FALSE` is sometimes much faster.
#' @param timetype If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param by \bold{Deprecated}. Use `time_by` instead
#' @param floor_date \bold{Deprecated}. Use `time_floor` instead.
#' @param seq_type \bold{Deprecated}. Use `time_type` instead.
#' @examples
#' library(timeplyr)
#' library(lubridate)
#' library(ggplot2)
#' library(dplyr)
#'
#' # Easily create custom time breaks
#' flights <- nycflights13::flights %>%
#'   fslice_sample(n = 10^3, seed = 8192821) %>%
#'   select(time_hour) %>%
#'   mutate(date = as_date(time_hour))
#'
#' # time_cut() and time_breaks() automatically find a
#' # suitable way to cut the data
#' time_cut(flights$date)
#' # Works with datetimes as well
#' time_cut(flights$time_hour, n = 5) # 5 breaks
#' # Custom formatting
#' time_cut(flights$date, fmt = "%Y %b", by = "month")
#' time_cut(flights$time_hour, fmt = "%Y %b", by = "month")
#' # Just the breaks
#' time_breaks(flights$date, n = 5, by = "month")
#' time_breaks(flights$time_hour, n = 5, by = "month")
#'
#' # To get exact breaks at regular intervals, use time_expandv
#' weekly_breaks <- time_expandv(flights$date,
#'                               by = "5 weeks",
#'                               week_start = 1, # Monday
#'                               floor_date = TRUE)
#' weekly_labels <- format(weekly_breaks, "%b-%d")
#' flights %>%
#'   time_count(time = date, by = "week") %>%
#'   ggplot(aes(x = date, y = n)) +
#'   geom_bar(stat = "identity") +
#'   scale_x_date(breaks = weekly_breaks,
#'                labels = weekly_labels)
#' @rdname time_cut
#' @export
time_cut <- function(x, n = 5, time_by = NULL,
                     from = NULL, to = NULL,
                     fmt = NULL,
                     time_floor = FALSE, week_start = getOption("lubridate.week.start", 1),
                     n_at_most = TRUE, as_factor = TRUE,
                     time_type = c("auto", "duration", "period"),
                     roll_month = "preday", roll_dst = "pre",
                     by = NULL,
                     seq_type = NULL,
                     floor_date = NULL){
  time_breaks <- time_breaks(x = x, n = n, time_by = time_by,
                             from = from, to = to,
                             time_floor = time_floor,
                             week_start = week_start,
                             n_at_most = n_at_most,
                             time_type = time_type,
                             roll_month = roll_month, roll_dst = roll_dst,
                             by = by,
                             seq_type = seq_type,
                             floor_date = floor_date)
  x_unique <- collapse::na_rm(collapse::funique(x, sort = TRUE))
  if (length(time_breaks) > length(x_unique)) time_breaks <- x_unique
  from <- bound_from(from, x)
  to <- bound_to(to, x)
  out <- cut_time2(x, c(time_breaks, to + 1))
  time_labels <- tseq_levels(x = to, time_breaks, fmt = fmt)
  if (as_factor){
    out <- ffactor(out,
                   levels = as.character(time_breaks),
                   ordered = TRUE,
                   na.exclude = TRUE)
    if (!isTRUE(all.equal(levels(out), time_labels))){
      out <- factor(out,
                    levels = as.character(time_breaks),
                    labels = time_labels)
    }
  }
  out
}
#' @rdname time_cut
#' @export
time_breaks <- function(x, n = 5, time_by = NULL,
                        from = NULL, to = NULL,
                        time_floor = FALSE, week_start = getOption("lubridate.week.start", 1),
                        n_at_most = TRUE,
                        time_type = c("auto", "duration", "period"),
                        roll_month = "preday", roll_dst = "pre",
                        by = NULL,
                        seq_type = NULL,
                        floor_date = NULL){
  ### Temporary arg switches while deprecating
  if (!is.null(by)){
    warning("by is deprecated, use time_by instead")
    time_by <- by
  }
  if (!is.null(seq_type)){
    warning("seq_type is deprecated, use time_type instead")
    time_type <- seq_type
  }
  if (!is.null(floor_date)){
    warning("floor_date is deprecated, use time_floor instead")
    time_floor <- floor_date
  }
  ###
  stopifnot(is.numeric(n))
  stopifnot(n >= 1)
  stopifnot(length(n) == 1)
  time_type <- match.arg(time_type)
  from <- bound_from(from, x)
  to <- bound_to(to, x)
  n_unique <- n_unique(x, na.rm = TRUE)
  n <- min(n, n_unique)
  if (is.null(time_by)){
    date_units <- c("days", "weeks", "months", "years")
    units_to_try <- date_units
    unit_nums <- rep_len(1L, 4)
    seq_types <- rep_len("period", length(date_units))
    if (is_datetime(x)){
      datetime_units <- setdiff(.duration_units, date_units)
      units_to_try <- c(datetime_units, date_units)
      seq_types <- c(rep_len("duration", length(datetime_units)), seq_types)
      unit_nums <- c(rep_len(1L, length(datetime_units)), unit_nums)
    }
    units_to_try <- rev(units_to_try)
    i <- 0
    while(i <= length(units_to_try)){
      i <- i + 1
      n_breaks <- time_seq_sizes(from, to, time_by = setnames(list(1),
                                                              units_to_try[i]),
                                 time_type = time_type)
      if (n_breaks >= n) break
    }
    unit <- units_to_try[i]
    time_by <- unit
    unit_multiplier <- 1
    scale <- 1
    num <- 1
    if (time_type == "auto") time_type <- guess_seq_type(unit)
  } else {
    unit_info <- unit_guess(time_by)
    unit <- unit_info[["unit"]]
    scale <- unit_info[["scale"]]
    num <- unit_info[["num"]]
    by <- unit
    n_breaks <- time_seq_sizes(from, to, time_by = setnames(list(1 * scale * num),
                                                            unit),
                               time_type = time_type)
    unit_multiplier <- 1
  }
  if (n_breaks > n && n < n_unique){
    unit_multiplier <- (n_breaks / n)
    if (!n_at_most){
      unit_multiplier <- floor(unit_multiplier)
    } else {
      unit_multiplier <- ceiling(unit_multiplier)
    }
  }
  ftseq(from = from, to = to, units = unit, num = num * scale * unit_multiplier,
        time_floor = time_floor, week_start = week_start,
        time_type = time_type,
        roll_month = roll_month, roll_dst = roll_dst)
}
