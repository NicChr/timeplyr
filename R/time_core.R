#' Vector date and datetime functions
#'
#' @description These are atomic vector-based functions
#' of the tidy equivalents which all have a "v" suffix to denote this.
#' These are more geared towards programmers and allow for working with date and
#' datetime vectors.
#'
#' @param x Time vector. \cr
#' E.g. a `Date`, `POSIXt`, `numeric` or any time-based vector.
#' @param timespan [timespan].
#' @param from Start time.
#' @param to End time.
#'
#' @returns
#' Vectors (typically the same class as `x`) of varying lengths depending
#' on the arguments supplied.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' library(nycflights13)
#' x <- unique(flights$time_hour)
#'
#' # Number of missing hours
#' time_num_gaps(x)
#'
#' # Same as above
#' time_grid_size(x) - length(unique(x))
#'
#' # Time sequence that spans the data
#' length(time_grid(x)) # Automatically detects hour granularity
#' time_grid(x, "month")
#' time_grid(x, from = floor_date(min(x), "month"), to = today(),
#'           timespan = timespan("month"))
#'
#' # Complete missing gaps in time using time_complete
#' y <- time_complete(x, "hour")
#' identical(y[!y %in% x], time_gaps(x))
#'
#' # Summarise time into higher intervals
#' quarters <- time_cut_width(y, "quarter")
#' interval_count(quarters)
#'
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
              timespan)
}
#' @rdname time_core
#' @export
time_complete_missing <- function(x, timespan = granularity(x)){
  time_full <- time_grid(x, timespan)
  out <- time_cast(x, time_full)
  gaps <- cheapr::setdiff_(time_full, out)
  if (length(gaps) > 0){
    out <- c(out, gaps)
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
