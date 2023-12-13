#' Vector date and datetime functions
#'
#' @description These are atomic vector-based functions
#' of the tidy equivalents which all have a "v" suffix to denote this.
#' These are more geared towards programmers and allow for working with date and
#' datetime vectors.
#'
#' @param x Time variable. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`, `yearmon`, `yearqtr`,
#' `year_month` or `year_quarter`.
#' @param time_by Time unit. \cr
#' Must be one of the following:
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
#' @param unique Should the result be unique or match the length of the vector?
#' Default is `TRUE`.
#' @param sort Should the output be sorted? Default is `TRUE`.
#' @param include_interval Logical. If `TRUE` then the result is a `tibble`
#' with a column "interval" of the form `time_min <= x < time_max`
#' showing the time interval in which the aggregated time points belong to.
#' The rightmost interval will always be closed.
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
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param complete Logical. If `TRUE` implicit gaps in time are filled
#' before counting and after time aggregation (controlled using `time_by`).
#' The default is `FALSE`.
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame.
#' @param use.g.names Should the result include group names?
#' Default is `TRUE`.
#'
#' @returns
#' Vectors (typically the same class as `x`) of varying lengths depending
#' on the arguments supplied.
#' `time_countv()` returns a `tibble`.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' library(nycflights13)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' x <- unique(flights$time_hour)
#'
#' # Number of missing hours
#' time_num_gaps(x)
#'
#' # Same as above
#' time_span_size(x) - length(unique(x))
#'
#' # Time sequence that spans the data
#' time_span(x) # Automatically detects hour granularity
#' time_span(x, time_by = "month")
#' time_span(x, time_by = list("quarters" = 1),
#'              to = today(),
#'              # Floor start of sequence to nearest month
#'              time_floor = TRUE)
#'
#' # Complete missing gaps in time using time_completev
#' y <- time_completev(x, time_by = "hour")
#' identical(y[!y %in% x], time_gaps(x))
#'
#' # Summarise time using time_summarisev
#' time_summarisev(y, time_by = "quarter")
#' time_summarisev(y, time_by = "quarter", unique = TRUE)
#' flights %>%
#'   fcount(quarter_start = time_summarisev(time_hour, "quarter"))
#' # Alternatively
#' time_countv(x, time_by = "quarter")
#' # If you want the above as an atomic vector just use tibble::deframe
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname time_core
#' @export
time_expandv <- function(x, time_by = NULL, from = NULL, to = NULL,
                         g = NULL, use.g.names = TRUE,
                         time_type = getOption("timeplyr.time_type", "auto"),
                         time_floor = FALSE,
                         week_start = getOption("lubridate.week.start", 1),
                         roll_month = getOption("timeplyr.roll_month", "preday"),
                         roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  check_is_time_or_num(x)
  check_length_lte(from, 1)
  check_length_lte(to, 1)
  time_by <- time_by_get(x, time_by = time_by)
  if (time_by_length(time_by) > 1L){
    stop("time_by must be a time unit containing a single numeric increment")
  }
  g <- GRP2(g)
  check_data_GRP_size(x, g)
  has_groups <- !is.null(g)
  if (is.null(from)){
    from <- collapse::fmin(x, g = g, use.g.names = FALSE, na.rm = TRUE)
  }
  if (is.null(to)){
    to <- collapse::fmax(x, g = g, use.g.names = FALSE, na.rm = TRUE)
  }
  # Make sure from/to are datetimes if x is datetime
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  if (time_floor){
    from <- time_floor2(from, time_by, week_start = week_start)
  }
  seq_sizes <- time_seq_sizes(from, to, time_by, time_type = time_type)
  # if (collapse::allNA(seq_sizes)){
  #   return(rep_len(na_init(x), length(from)))
  # }
  out <- time_seq_v2(seq_sizes,
                     from = from,
                     time_by = time_by,
                     time_type = time_type,
                     time_floor = FALSE,
                     week_start = week_start,
                     roll_month = roll_month,
                     roll_dst = roll_dst)
  if (has_groups && use.g.names){
    group_names <- GRP_names(g)
    if (!is.null(group_names)){
      names(out) <- rep.int(group_names, times = seq_sizes)
    }
  }
  out
}
#' @rdname time_core
#' @export
time_span <- time_expandv
#' @rdname time_core
#' @export
time_completev <- function(x, time_by = NULL, from = NULL, to = NULL,
                           sort = TRUE,
                           time_type = getOption("timeplyr.time_type", "auto"),
                           time_floor = FALSE,
                           week_start = getOption("lubridate.week.start", 1),
                           roll_month = getOption("timeplyr.roll_month", "preday"),
                           roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  time_full <- time_expandv(x, time_by = time_by,
                            from = from, to = to,
                            time_type = time_type,
                            time_floor = time_floor,
                            week_start = week_start,
                            roll_month = roll_month,
                            roll_dst = roll_dst)
  out <- time_cast(x, time_full)
  # out <- c(x, time_full[cpp_which(time_full %in% x, invert = TRUE)])
  gaps <- time_full[collapse::whichNA(collapse::fmatch(time_full, out, overid = 2L))]
  if (length(gaps) > 0){
    out <- c(out, gaps)
  }
  if (sort){
    out <- conditional_sort(out)
  }
  out
}
#' @rdname time_core
#' @export
time_summarisev <- function(x, time_by = NULL, from = NULL, to = NULL,
                            sort = FALSE, unique = FALSE,
                            time_type = getOption("timeplyr.time_type", "auto"),
                            time_floor = FALSE,
                            week_start = getOption("lubridate.week.start", 1),
                            roll_month = getOption("timeplyr.roll_month", "preday"),
                            roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                            include_interval = FALSE){
  check_is_time_or_num(x)
  check_length_lte(from, 1)
  check_length_lte(to, 1)
  if (is.null(from)){
    from <- collapse::fmin(x, na.rm = TRUE)
  }
  if (is.null(to)){
    to <- collapse::fmax(x, na.rm = TRUE)
  }
  # set_time_cast(from, to)
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  if (isTRUE(from > to)){
    stop("from must be <= to")
  }
  # Time sequence
  time_breaks <- time_expandv(x, time_by = time_by,
                              from = from, to = to,
                              time_type = time_type,
                              time_floor = time_floor,
                              week_start = week_start,
                              roll_month = roll_month,
                              roll_dst = roll_dst)
  x <- time_cast(x, time_breaks)
  to <- time_cast(to, x)
  # Cut time
  time_bins <- c(unclass(time_breaks), unclass(to))
  if (include_interval){
    time_break_ind <- cut_time(x, breaks = time_bins, codes = TRUE)
    # Time breaks subset on cut indices
    out <- time_breaks[time_break_ind]
    time_int <- tseq_interval(x = to, time_breaks)
    time_int <- time_int[time_break_ind]
    out <- new_tbl(x = out, interval = time_int)
    # Unique and sorting
    if (unique){
      out <- fdistinct(out, .cols = "x", .keep_all = TRUE, sort = sort)
    }
    if (sort && !unique){
      out <- farrange(out, .cols = "x")
    }
    if (!is_interval(time_int)){
      attr(out[["interval"]], "start") <- out[["x"]]
    }
  } else {
    out <- cut_time(x, breaks = time_bins, codes = FALSE)
    if (unique){
      out <- collapse::funique(out, sort = sort)
    } else {
      if (sort){
        out <- radix_sort(out)
      }
    }
  }
  out
}
#' @rdname time_core
#' @export
time_countv <- function(x, time_by = NULL, from = NULL, to = NULL,
                        sort = TRUE, unique = TRUE,
                        complete = FALSE,
                        time_type = getOption("timeplyr.time_type", "auto"),
                        include_interval = FALSE,
                        time_floor = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        roll_month = getOption("timeplyr.roll_month", "preday"),
                        roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  check_is_time_or_num(x)
  time_by <- time_by_get(x, time_by = time_by)
  if (is.null(from)){
    from <- collapse::fmin(x, na.rm = TRUE, use.g.names = FALSE)
  }
  if (is.null(to)){
    to <- collapse::fmax(x, na.rm = TRUE, use.g.names = FALSE)
  }
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  # Time sequence
  time_breaks <- time_seq_v(from = from, to = to,
                            time_by = time_by,
                            time_type = time_type,
                            time_floor = time_floor,
                            week_start = week_start,
                            roll_month = roll_month,
                            roll_dst = roll_dst)
  x <- time_cast(x, time_breaks)
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  out_len <- length(x)
  # Aggregate time/cut time
  time_break_ind <- cut_time(x, breaks = c(time_breaks, to), codes = TRUE)
  # Time breaks subset on cut indices
  x <- time_breaks[time_break_ind]
  # Counts
  out <- group_sizes(x, expand = TRUE)
  # (Optionally) complete time data
  if (complete){
    time_missed <- time_breaks[collapse::whichNA(collapse::fmatch(time_breaks, x, overid = 2L))]
    if (length(time_missed) > 0L){
      x <- c(x, time_missed) # Complete time sequence
    }
    out <- c(out, integer(length(time_missed)))
  }
  if (include_interval){
    time_seq_int <- tseq_interval(x = to, time_breaks)
    time_int <- time_seq_int[time_break_ind]
    if (complete && length(time_missed) > 0L){
      time_int <- c(time_int, time_seq_int[cpp_which(attr(time_seq_int, "start") %in%
                                                       time_cast(time_missed, attr(time_seq_int, "start")))])
    }
    out <- new_tbl(x = x, interval = time_int, n = out)
    if (unique){
      out <- fdistinct(out, .cols = "x", .keep_all = TRUE, sort = sort)
    }
    if (sort && !unique){
      out <- farrange(out, .cols = "x")
    }
    if (!is_interval(out[["interval"]])){
      attr(out[["interval"]], "start") <- out[["x"]]
    }
  } else {
    if (unique || sort){
      dt <- new_dt(x = x, n = out, .copy = TRUE)
      if (unique){
        dt <- collapse::funique(dt, cols = "x", sort = FALSE)
      }
      if (sort){
        setorderv2(dt, cols = "x")
      }
      out <- df_as_tibble(dt)
    }
  }
  if (!is_df(out)){
    out <- new_tbl(x = x, n = out)
  }
  out
}
#' @rdname time_core
#' @export
time_span_size <- function(x, time_by = NULL, from = NULL, to = NULL,
                           g = NULL, use.g.names = TRUE,
                           time_type = getOption("timeplyr.time_type", "auto"),
                           time_floor = FALSE,
                           week_start = getOption("lubridate.week.start", 1)){
  check_is_time_or_num(x)
  check_length_lte(from, 1)
  check_length_lte(to, 1)
  time_by <- time_by_get(x, time_by = time_by)
  if (time_by_length(time_by) > 1L){
    stop("time_by must be a time unit containing a single numeric increment")
  }
  g <- GRP2(g)
  check_data_GRP_size(x, g)
  has_groups <- is.null(g)
  if (is.null(from)){
    from <- collapse::fmin(x, g = g, use.g.names = FALSE, na.rm = TRUE)
  }
  if (is.null(to)){
    to <- collapse::fmax(x, g = g, use.g.names = FALSE, na.rm = TRUE)
  }
  # Make sure from/to are datetimes if x is datetime
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  if (time_floor){
    from <- time_floor2(from, time_by = time_by, week_start = week_start)
  }
  out <- time_seq_sizes(from = from, to = to,
                        time_by = time_by,
                        time_type = time_type)
  if (has_groups && use.g.names){
    group_names <- GRP_names(g)
    if (!is.null(group_names)){
      names(out) <- group_names
    }
  }
  out
}
