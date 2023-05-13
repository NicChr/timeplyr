#' Vector date and datetime functions
#'
#' @description These are atomic vector-based functions
#' of the tidy equivalents which all have a "v" suffix to denote this.
#' These are more geared towards programmers and allow for working with date and
#' datetime vectors.
#'
#' @param x Date, datetime or numeric vector.
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
#' @param from Time series start date.
#' @param to Time series end date.
#' @param use.names Should a named vector be returned?
#' @param complete Logical. If `TRUE` implicit gaps in time are filled
#' before counting and after time aggregation (controlled using `by`).
#' The default is `TRUE`.
#' @param unique Should the result be unique or match the length of the vector?
#' Default is `TRUE`.
#' @param sort Should the output be sorted? Default is `TRUE`.
#' @param include_interval Logical. If `TRUE` then the result is a `tibble`
#' with a column "interval" of the form `time_min <= x < time_max`
#' showing the time interval in which the aggregated time points belong to.
#' The rightmost interval will always be closed.
#' @param seq_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations`
#' are used otherwise.
#' @param is_sorted If `TRUE` then x is assumed to be in
#' ascending order and no additional pre-sorting is done.
#' It is recommended to leave this as `FALSE` unless you require the
#' extra speed for programming or looping purposes.
#' @param floor_date Should `from` be floored to the nearest unit specified
#' through the `by`
#' argument? This is particularly useful for starting sequences
#' at the beginning of a week
#' or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `floor_date = TRUE`.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' flights <- nycflights13::flights
#' x <- flights$time_hour
#'
#' n_time_missing(x, by = "hour") # Missing hours
#' # This returns missing dates, even for datetimes
#' # where time_missing() returns missing gaps in a date/datetime sequence
#' missing_dates(x)
#' # Time sequence that spans the data
#' time_span(x, by = "hour")
#' as_datetime(time_span(as.numeric(x), by = 3600), # Also works
#'             tz = tz(x))
#' # No need to specify by as it automatically detects granularity
#' time_span(x)
#' time_span(x, by = "month")
#' time_span(x, by = list("quarters" = 1),
#'              to = today(),
#'              # Floor start of sequence to nearest month
#'              floor_date = TRUE)
#'
#' # Complete missing gaps in time using time_completev
#' y <- time_completev(x, by = "hour")
#' all.equal(y[!y %in% x], time_missing(x, by = "hour"))
#'
#' # Summarise time using time_summarisev
#' time_summarisev(y, by = "quarter")
#' # Set unique = FALSE to return vector same length as input
#' time_summarisev(y, by = "quarter", unique = FALSE)
#' flights %>%
#'   mutate(quarter_start = time_summarisev(time_hour,
#'                                          by = "quarter",
#'                                          unique = FALSE)) %>%
#'   fcount(quarter_start)
#' # You can include the associated time interval
#' time_summarisev(y, by = "quarter", unique = TRUE,
#'                 include_interval = TRUE)
#'
#' # Count time using time_countv
#' time_countv(x, by = list("months" = 3))
#' time_countv(x, by = list("months" = 3), include_interval = TRUE)
#'
#' # No completing of missing gaps in time with complete = FALSE
#' flights %>%
#'   reframe(time = unique(time_hour),
#'           n = time_countv(time_hour, complete = FALSE,
#'                           use.names = FALSE))
#' # With completion
#' flights %>%
#'   reframe(time = time_expandv(time_hour),
#'           n = time_countv(time_hour, complete = TRUE,
#'                           use.names = FALSE))
#' @rdname time_core
#' @export
time_expandv <- function(x, by = NULL, from = NULL, to = NULL,
                      seq_type = c("auto", "duration", "period"),
                      is_sorted = FALSE,
                      floor_date = FALSE,
                      week_start = getOption("lubridate.week.start", 1),
                      roll_month = "preday", roll_dst = "pre"){
  stopifnot(is_time_or_num(x))
  time_by <- time_by_get(x, by = by, is_sorted = is_sorted)
  if (is.null(from) || is.null(to)){
    x_range <- collapse::frange(x, na.rm = TRUE)
  }
  if (is.null(from)){
    from <- x_range[[1L]]
  }
  if (is.null(to)){
    to <- x_range[[2L]]
  }
  # Make sure from/to are datetimes if x is datetime
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  ftseq(from = from, to = to, units = names(time_by), num = time_by[[1L]],
        seq_type = seq_type, floor_date = floor_date,
        week_start = week_start,
        roll_month = roll_month, roll_dst = roll_dst)
}
#' @rdname time_core
#' @export
time_completev <- function(x, by = NULL, from = NULL, to = NULL,
                           sort = TRUE,
                           seq_type = c("auto", "duration", "period"),
                           is_sorted = FALSE,
                           floor_date = FALSE,
                           week_start = getOption("lubridate.week.start", 1),
                           roll_month = "preday", roll_dst = "pre"){
  tseq <- time_span(x, by = by, from = from, to = to,
                    seq_type = seq_type, is_sorted = is_sorted,
                    floor_date = floor_date, week_start = week_start,
                    roll_month = roll_month, roll_dst = roll_dst)
  out <- time_c(x, tseq[!tseq %in% x])
  if (sort) out <- radix_sort(out)
  out
}
#' @rdname time_core
#' @export
time_summarisev <- function(x, by = NULL, from = NULL, to = NULL,
                            sort = FALSE, unique = FALSE,
                            seq_type = c("auto", "duration", "period"),
                            is_sorted = FALSE,
                            floor_date = FALSE,
                            week_start = getOption("lubridate.week.start", 1),
                            roll_month = "preday", roll_dst = "pre",
                            include_interval = FALSE){
  # # We need to bound from and to based on range of x
  from <- bound_from(from, x)
  to <- bound_to(to, x)
  # Time sequence
  time_breaks <- time_span(x, by = by, from = from, to = to,
                           seq_type = seq_type, is_sorted = is_sorted,
                           floor_date = floor_date, week_start = week_start,
                           roll_month = roll_month, roll_dst = roll_dst)
  # time_breaks <- time_cast(time_breaks, x)
  # Cut time
  time_break_ind <- fcut_ind(x, c(time_breaks, to + 1))
  # Time breaks subset on cut indices
  out <- time_breaks[time_break_ind]

  if (include_interval){
    time_int <- tseq_interval(x = to, time_breaks)
    time_int <- time_int[time_break_ind]
    out <- list("x" = out,
                "interval" = time_int)
    out <- list_to_tibble(out)
    # Unique and sorting
    if (unique){
      out <- fdistinct(out, .cols = "x", .keep_all = TRUE)
      # out <- gunique(out, g = out[["x"]])
    }
    if (sort){
      out <- farrange(out, .cols = "x")
    }
    if (!is_interval(time_int)){
      attr(out[["interval"]], "start") <- out[["x"]]
    }
  } else {
    if (unique){
      out <- collapse::funique(out, sort = sort)
    } else {
      if (sort) out <- radix_sort(out)
    }
  }
  out
}
#' @rdname time_core
#' @export
time_countv <- function(x, by = NULL, from = NULL, to = NULL,
                        sort = !is_sorted, unique = TRUE,
                        use.names = TRUE, complete = TRUE,
                        seq_type = c("auto", "duration", "period"),
                        is_sorted = FALSE,
                        floor_date = FALSE,
                        include_interval = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        roll_month = "preday", roll_dst = "pre"){
  stopifnot(is_time_or_num(x))
  x_na <- which(is.na(x))
  time_by <- time_by_get(x, by = by, is_sorted = is_sorted)
  if (is.null(from)){
    .from <- collapse::fmin(x, na.rm = TRUE, use.g.names = FALSE)
  } else {
    .from <- time_cast(from, x)
  }
  if (is.null(to)){
    .to <- collapse::fmax(x, na.rm = TRUE, use.g.names = FALSE)
  } else {
    .to <- time_cast(to, x)
  }
  if (!is.null(from) || !is.null(to)){
    x <- x[data.table::between(x, .from, .to, incbounds = TRUE, NAbounds = NA)]
  }
  # Time sequence
  time_breaks <- ftseq(from = .from, to = .to,
                       units = names(time_by),
                       num = time_by[[1L]],
                       seq_type = seq_type, floor_date = floor_date,
                       week_start = week_start,
                       roll_month = roll_month, roll_dst = roll_dst)
  # time_breaks <- time_cast(time_breaks, x)
  out_len <- length(x)
  # Aggregate time/cut time
  time_break_ind <- fcut_ind(x, c(time_breaks, to + 1))
  # Time breaks subset on cut indices
  x <- time_breaks[time_break_ind]

  # (Optionally) complete time data
  time_missed <- x[0L]
  if (complete){
    time_missed <- time_breaks[!time_breaks %in% x]
    if (length(time_missed) > 0L){
      x <- c(x, time_missed) # Complete time sequence
    }
  }
  # Count time
  # Don't count completed sequence items, only original..
  cnt_grps <- GRP2(if (complete) x[seq_len(out_len)] else x,
                   sort = FALSE,
                   call = FALSE, return.groups = FALSE,
                   na.last = TRUE, decreasing = FALSE,
                   return.order = FALSE)
  out <- integer(out_len + length(time_missed))
  # Replace allocated integer with counts
  setv(out, seq_len(out_len), collapse::GRPN(cnt_grps, expand = TRUE),
       vind1 = TRUE)
  # if (use.names && !include_interval) out <- setnames(out, x)
  if (include_interval){
    time_seq_int <- tseq_interval(x = .to, time_breaks)
    time_int <- time_seq_int[time_break_ind]
    if (complete && length(time_missed) > 0L){
      time_int <- c(time_int, time_seq_int[which(attr(time_seq_int, "start") %in%
                                                   time_cast(time_missed, attr(time_seq_int, "start")))])
    }
    out <- list("x" = x,
                "interval" = time_int,
                "n" = out)
    out <- list_to_tibble(out)
    if (unique){
      out <- fdistinct(out, .cols = "x", .keep_all = TRUE)
    }
    if (sort){
      if (sort){
        out <- farrange(out, .cols = "x")
      }
    }
    if (!is_interval(out[["interval"]])){
      attr(out[["interval"]], "start") <- out[["x"]]
    }
  } else {
    if (unique || sort){
      dt <- data.table::data.table(x, out)
      if (unique){
        dt <- collapse::funique(dt, cols = "x", sort = sort)
      } else if (!unique && sort){
        data.table::setorderv(dt, cols = "x", na.last = TRUE)
      }
      out <- dt[["out"]]
      if (use.names){
        out <- setnames(out, dt[["x"]])
      }
    } else {
      if (use.names){
        out <- setnames(out, x)
      }
    }
  }
  out
}
#' @rdname time_core
#' @export
time_span <- time_expandv
