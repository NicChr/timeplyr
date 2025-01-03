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
#' @param as_interval Should result be a `time_interval`?
#' Default is `FALSE`. \cr
#' This can be controlled globally through `options(timeplyr.use_intervals)`.
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
#' time_grid_size(x) - length(unique(x))
#'
#' # Time sequence that spans the data
#' length(time_span(x)) # Automatically detects hour granularity
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
#' quarters <- time_summarisev(y, time_by = "quarter")
#' interval_count(quarters)
#'
#' # Unique quarters
#' time_summarisev(y, time_by = "quarter", unique = TRUE)
#'
#' flights %>%
#'   fastplyr::f_count(quarter = time_summarisev(time_hour, "quarter"))
#' # Alternatively
#' time_countv(flights$time_hour, time_by = "quarter")
#' # If you want the above as an atomic vector just use tibble::deframe
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#' }
#' @rdname time_core
#' @export
time_grid <- function(x, timespan = granularity(x),
                      from = NULL, to = NULL){
  check_is_time_or_num(x)
  check_length_lte(from, 1)
  check_length_lte(to, 1)
  timespan <- timespan(timespan)

  if (is.null(from)){
    rng <- collapse::frange(x, na.rm = TRUE)
    from <- rng[1]
    to <- rng[2]
  } else {
    if (is.null(from)){
      from <- collapse::fmin(x, na.rm = TRUE)
    }
    if (is.null(to)){
      to <- collapse::fmax(x, na.rm = TRUE)
    }
  }
  # Make sure from/to are datetimes if x is datetime
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  seq_sizes <- time_seq_sizes(from, to, timespan)
  time_seq_v2(seq_sizes,
              from = from,
              time_by = timespan)
}
#' @rdname time_core
#' @export
time_complete <- function(x, timespan = granularity(x), sort = TRUE){
  time_full <- time_grid(x, timespan)
  out <- time_cast(x, time_full)
  gaps <- cheapr::setdiff_(time_full, out)
  if (length(gaps) > 0){
    out <- c(out, gaps)
  }
  if (sort){
    out <- sort(out)
  }
  out
}
#' @rdname time_core
#' @export
time_grid_size <- function(x, timespan = granularity(x),
                           from = NULL, to = NULL){
  check_is_time_or_num(x)
  check_length_lte(from, 1)
  check_length_lte(to, 1)
  timespan <- timespan(timespan)
  if (is.null(from)){
    rng <- collapse::frange(x, na.rm = TRUE)
    from <- rng[1]
    to <- rng[2]
  } else {
    if (is.null(from)){
      from <- collapse::fmin(x, use.g.names = FALSE, na.rm = TRUE)
    }
    if (is.null(to)){
      to <- collapse::fmax(x, use.g.names = FALSE, na.rm = TRUE)
    }
  }
  # Make sure from/to are datetimes if x is datetime
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  time_seq_sizes(from, to, timespan)
}
time_expandv <- function(x, timespan = granularity(x),
                         from = NULL, to = NULL,
                         g = NULL, use.g.names = TRUE){
  check_is_time_or_num(x)
  check_length_lte(from, 1)
  check_length_lte(to, 1)
  timespan <- timespan(timespan)
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
  seq_sizes <- time_seq_sizes(from, to, timespan)
  out <- time_seq_v2(seq_sizes, from = from, timespan)
  if (has_groups && use.g.names){
    group_names <- GRP_names(g)
    if (!is.null(group_names)){
      names(out) <- rep.int(group_names, times = seq_sizes)
    }
  }
  out
}
time_expanded_sizes <- function(x, timespan = granularity(x),
                                from = NULL, to = NULL,
                                g = NULL, use.g.names = TRUE){
  check_is_time_or_num(x)
  check_length_lte(from, 1)
  check_length_lte(to, 1)
  timespan <- timespan(timespan)
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
  out <- time_seq_sizes(from = from, to = to, timespan)
  if (has_groups && use.g.names){
    group_names <- GRP_names(g)
    if (!is.null(group_names)){
      names(out) <- group_names
    }
  }
  out
}
