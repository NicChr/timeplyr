#' Vector date and datetime functions
#'
#' @description These are atomic vector-based functions
#' of the tidy equivalents which all have a "v" suffix to denote this.
#' These are more geared towards programmers and allow for working with date and
#' datetime vectors.
#'
#' @param x Date, datetime or numeric vector.
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
#' @param is_sorted If `TRUE` then x is assumed to be in
#' ascending order and no additional pre-sorting is done.
#' It is recommended to leave this as `FALSE` unless you require the
#' extra speed for programming or looping purposes.
#' @param time_floor Should `from` be floored to the nearest unit specified
#' through the `time_by` argument?
#' This is particularly useful for starting sequences at the
#' beginning of a week or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `floor_date = TRUE`.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param use.names Should a named vector be returned for `time_countv()`?
#' @param complete Logical. If `TRUE` implicit gaps in time are filled
#' before counting and after time aggregation (controlled using `time_by`).
#' The default is `TRUE`.
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame.
#' @param use.g.names Should the result include group names?
#' Default is `TRUE`.
#' @param by \bold{Deprecated}. Use `time_by` instead
#' @param floor_date \bold{Deprecated}. Use `time_floor` instead.
#' @param seq_type \bold{Deprecated}. Use `time_type` instead.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' flights <- nycflights13::flights
#' x <- flights$time_hour
#'
#' # n_time_missing(x, time_by = "hour") # Missing hours
#' # This returns missing dates, even for datetimes
#' # where time_missing() returns missing gaps in a date/datetime sequence
#' missing_dates(x)
#' # Time sequence that spans the data
#' time_span(x, time_by = "hour")
#' as_datetime(time_span(as.numeric(x), time_by = 3600), # Also works
#'             tz = tz(x))
#' # No need to specify by as it automatically detects granularity
#' time_span(x)
#' time_span(x, time_by = "month")
#' time_span(x, time_by = list("quarters" = 1),
#'              to = today(),
#'              # Floor start of sequence to nearest month
#'              floor_date = TRUE)
#'
#' # Complete missing gaps in time using time_completev
#' y <- time_completev(x, time_by = "hour")
#' all.equal(y[!y %in% x], time_missing(x, time_by = "hour"))
#'
#' # Summarise time using time_summarisev
#' time_summarisev(y, time_by = "quarter")
#' # Set unique = FALSE to return vector same length as input
#' time_summarisev(y, time_by = "quarter", unique = FALSE)
#' flights %>%
#'   mutate(quarter_start = time_summarisev(time_hour,
#'                                          time_by = "quarter",
#'                                          unique = FALSE)) %>%
#'   fcount(quarter_start)
#' # You can include the associated time interval
#' time_summarisev(y, time_by = "quarter", unique = TRUE,
#'                 include_interval = TRUE)
#'
#' # Count time using time_countv
#' time_countv(x, time_by = list("months" = 3))
#' time_countv(x, time_by = list("months" = 3), include_interval = TRUE)
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
time_expandv <- function(x, time_by = NULL, from = NULL, to = NULL,
                         g = NULL, use.g.names = TRUE,
                         time_type = c("auto", "duration", "period"),
                         time_floor = FALSE,
                         week_start = getOption("lubridate.week.start", 1),
                         roll_month = "preday", roll_dst = "pre",
                         is_sorted = FALSE,
                         by = NULL,
                         seq_type = NULL,
                         floor_date = NULL){
  stopifnot(is_time_or_num(x))
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
  if (length(from) > 1L){
    stop("from must be of length 1")
  }
  if (length(to) > 1L){
    stop("to must be of length 1")
  }
  time_by <- time_by_get(x, time_by = time_by,
                         is_sorted = FALSE)
  if (length(time_by[[1L]]) > 1L){
    stop("time_by must be a time unit containing a single numeric increment")
  }
  if (!is.null(g)){
    g <- GRP2(g)
    if (GRP_data_size(g) != length(x)){
      stop("g must have the same size as x")
    }
  }
  if (is.null(from)){
    from <- collapse::fmin(x, g = g, use.g.names = FALSE)
  }
  if (is.null(to)){
    to <- collapse::fmax(x, g = g, use.g.names = FALSE)
  }
  # Make sure from/to are datetimes if x is datetime
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  if (time_floor){
    from <- time_floor2(from, time_by, week_start = week_start)
  }
  seq_sizes <- time_seq_sizes(from, to, time_by, time_type = time_type)
  if (isTRUE(log10(sum(seq_sizes)) >= 8)){
    message("The final size exceeds 8m rows, this may take a while")
  }
  out <- time_seq_v2(seq_sizes, from = from, time_by = time_by,
                     time_type = time_type,
                     time_floor = FALSE,
                     week_start = week_start,
                     roll_month = roll_month, roll_dst = roll_dst)
  if (use.g.names && !is.null(g)){
    names(out) <- rep.int(GRP_names(g), times = seq_sizes)
  }
  out
}
#' @rdname time_core
#' @export
time_completev <- function(x, time_by = NULL, from = NULL, to = NULL,
                           sort = TRUE,
                           time_type = c("auto", "duration", "period"),
                           is_sorted = FALSE,
                           time_floor = FALSE,
                           week_start = getOption("lubridate.week.start", 1),
                           roll_month = "preday", roll_dst = "pre",
                           by = NULL,
                           seq_type = NULL,
                           floor_date = NULL){
  tseq <- time_expandv(x, time_by = time_by, from = from, to = to,
                       time_type = time_type, is_sorted = is_sorted,
                       time_floor = time_floor, week_start = week_start,
                       roll_month = roll_month, roll_dst = roll_dst,
                       by = by,
                       floor_date = floor_date,
                       seq_type = seq_type)
  out <- time_c(x, tseq[!tseq %in% x])
  if (sort){
    out <- radix_sort(out)
  }
  out
}
#' @rdname time_core
#' @export
time_summarisev <- function(x, time_by = NULL, from = NULL, to = NULL,
                            sort = FALSE, unique = FALSE,
                            time_type = c("auto", "duration", "period"),
                            is_sorted = FALSE,
                            time_floor = FALSE,
                            week_start = getOption("lubridate.week.start", 1),
                            roll_month = "preday", roll_dst = "pre",
                            include_interval = FALSE,
                            by = NULL,
                            seq_type = NULL,
                            floor_date = NULL){
  if (is.null(from) || is.null(to)){
    x_range <- collapse::frange(x, na.rm = TRUE)
  }
  if (is.null(from)){
    from <- x_range[1L]
  } else {
    from <- time_cast(from, x)
  }
  if (is.null(to)){
    to <- x_range[2L]
  } else {
    to <- time_cast(to, x)
  }
  # Time sequence
  time_breaks <- time_expandv(x, time_by = time_by, from = from, to = to,
                              time_type = time_type, is_sorted = is_sorted,
                              time_floor = time_floor, week_start = week_start,
                              roll_month = roll_month, roll_dst = roll_dst,
                              by = by,
                              seq_type = seq_type,
                              floor_date = floor_date)
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
time_countv <- function(x, time_by = NULL, from = NULL, to = NULL,
                        sort = !is_sorted, unique = TRUE,
                        use.names = TRUE, complete = TRUE,
                        time_type = c("auto", "duration", "period"),
                        is_sorted = FALSE,
                        include_interval = FALSE,
                        time_floor = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        roll_month = "preday", roll_dst = "pre",
                        by = NULL,
                        seq_type = NULL,
                        floor_date = NULL){
  stopifnot(is_time_or_num(x))
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
  x_na <- which(is.na(x))
  time_by <- time_by_get(x, time_by = time_by, is_sorted = is_sorted)
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
  time_breaks <- time_seq_v(from = .from, to = .to,
                            time_by = time_by,
                            time_type = time_type,
                            time_floor = time_floor,
                            week_start = week_start,
                            roll_month = roll_month, roll_dst = roll_dst)
  # time_breaks <- time_cast(time_breaks, x)
  out_len <- length(x)
  # Aggregate time/cut time
  time_break_ind <- fcut_ind(x, c(time_breaks, .to + 1))
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
