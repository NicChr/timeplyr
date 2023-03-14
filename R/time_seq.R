#' Time based version of `base::seq()` to generate regular time sequences
#' with additional lubridate functionality for period calculations.
#'
#' @description This works like `seq()`,
#' but using `timechange` for the period calculations and
#' `base::seq.POSIXT()` for the duration calculations.
#' In many ways it is improved over `seq.POSIXt()` as
#' dates and/or datetimes can be supplied with no errors to
#' the start and end points.
#' Examples like,\cr
#' `time_seq(now(), length.out = 10, by = "0.5 days", seq_type = "dur")`
#' and \cr
#' `time_seq(today(), length.out = 10, by = "0.5 days", seq_type = "dur")`\cr
#' produce more expected results compared to \cr
#' `seq(now(), length.out = 10, by = "0.5 days")` or \cr
#' `seq(today(), length.out = 10, by = "0.5 days")`.\cr
#'
#' `time_seq_len()` is a vectorized convenience
#' function to calculate the length/size of
#' a regular time sequence, given a start, end and unit specification.\cr
#'
#' `time_seq_v()` is a vectorized version of `time_seq()`.\cr
#'
#' With periods, impossible dates and datetimes can be
#' rolled forward or backward
#' through the `roll_month` and `roll_dst` arguments.
#'
#' @param from Start date/datetime of sequence.
#' @param to End date/datetime of sequence.
#' @param by Unit increment of the sequence.
#' @param length.out Length of the sequence.
#' @param floor_date Should `from` be floored to
#' the nearest unit specified through the `by`
#' argument? This is particularly useful for
#' starting sequences at the beginning of a week
#' or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday, 7 means Sunday (default).
#' This is only used when `floor_date = TRUE`.
#' @param seq_type If "auto", `periods` are used for
#' the time expansion when days, weeks,
#' months or years are specified, and `durations`
#' are used otherwise. If `durations`
#' are used the output is always of class `POSIXt`.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param tz Timezone of returned time sequence.
#' @param as_period Logical. Should time interval be coerced to a period
#' before time difference is calculated? This is useful for calculating
#' for example age in exact years or months.
#' @param units Time units.
#' @param num Number of time units.
#'
#' @examples
#' library(timeplyr)
#' library(lubridate)
#'
#' # Dates
#' today <- today()
#' now <- now()
#' time_seq(today, today + years(1), by = "day")
#' time_seq(today, today + years(1), by = list("days" = 1)) # Alternative
#' time_seq(today, today + years(1), by = "week")
#' time_seq(today, today + years(1), by = "fortnight")
#' time_seq(today, today + years(1), by = "year")
#' time_seq(today, today + years(10), by = "year")
#' time_seq(today, today + years(100), by = "decade")
#'
#' # Datetimes
#' time_seq(now, now + years(1), by = "12 hours")
#' time_seq(now, now + years(1), by = "day")
#' time_seq(now, now + years(1), by = "week")
#' time_seq(now, now + years(1), by = "fortnight")
#' time_seq(now, now + years(1), by = "year")
#' time_seq(now, now + years(10), by = "year")
#' time_seq(now, today + years(100), by = "decade")
#'
#' # You can seamlessly mix dates and datetimes with no errors.
#' time_seq(now, today + days(3), by = "day")
#' time_seq(now, today + days(3), by = "hour")
#' time_seq(today, now + days(3), by = "day")
#' time_seq(today, now + days(3), by = "hour")
#'
#' # Choose between durations or periods
#'
#' start <- dmy(31012020)
#' # If seq_type is left as is,
#' # periods are used for days, weeks, months and years.
#' time_seq(start, by = "month", length.out = 12,
#'          seq_type = "period")
#' time_seq(start, by = "month", length.out = 12,
#'          seq_type = "duration")
#' # Notice how strange base R version is.
#' seq(start, by = "month", length.out = 12)
#'
#' # Roll forward or backward impossible dates
#'
#' leap <- dmy(29022020) # Leap day
#' end <- dmy(01032021)
#' # 3 different options
#' time_seq(leap, to = end, by = "year", seq_type = "per",
#'          roll_month = "NA")
#' time_seq(leap, to = end, by = "year", seq_type = "per",
#'          roll_month = "postday")
#' time_seq(leap, to = end, by = "year", seq_type = "per",
#'          roll_month = "preday")
#' @rdname time_seq
#' @export
time_seq <- function(from, to, by, length.out = NULL,
                     floor_date = FALSE,
                     week_start = getOption("lubridate.week.start", 1),
                     seq_type = c("auto", "duration", "period"),
                     roll_month = "preday", roll_dst = "pre",
                     tz = if (!missing(from)) lubridate::tz(from) else
                       lubridate::tz(to)){
  missing_from <- missing(from)
  missing_to <- missing(to)
  missing_by <- missing(by)
  missing_len <- is.null(length.out)
  seq_type <- match.arg(seq_type)
  if (!missing_len) stopifnot(sign(length.out) >= 0)
  n_args <- (4 - sum(c(missing_from, missing_to, missing_by, missing_len)))
  if (n_args < 3){
      stop("Please supply 3 from either from, to, length.out and by")
  }
  if (n_args == 4){
    warning("from, to, by and length.out have all been specified,
            the result may be unpredictable.")
  }
  if (!missing_from) stopifnot(length(from) <= 1)
  if (!missing_to) stopifnot(length(to) <= 1)
  if (!missing_by) stopifnot(length(by) <= 1)
  if (!missing_len) stopifnot(length(length.out) <= 1)
  # Basically if from and/or to are time do x else y
  if ( (
    (!missing_from && (is_time(from) || is.character(from))) ||
    ( !missing_to &&  (is_time(to) || is.character(to)))
  )){
    from_and_to <- !missing_from && !missing_to
    from_and_length <- !missing_from && !missing_len
    to_and_length <- !missing_to && !missing_len
    if (from_and_to){
      if (length(from) == 0 && length(to) == 1) {
        stop("from must be of length 1")
      }
      if (length(from) == 1 && length(to) == 0) {
        stop("to must be of length 1")
      }
    }
    # Unit parsing
    if (!missing_by){
      unit_info <- unit_guess(by)
      by_n <- unit_info[["num"]] * unit_info[["scale"]]
      by_unit <- unit_info[["unit"]]
      # Guess seq type
      if (seq_type == "auto") seq_type <- guess_seq_type(by_unit)
    }
    seq_type <- match.arg(seq_type)
    # Help the user by converting very common date formats
    if (!missing_from) from <- convert_common_dates(from)
    if (!missing_to) to <- convert_common_dates(to)

    # Make from and to most granular data type between them
    if (from_and_to && is_date(from) && is_datetime(to)){
      from <- lubridate::as_datetime(from, tz = tz)
    }
    if (from_and_to && is_date(to) && is_datetime(from)){
      to <- lubridate::as_datetime(to, tz = tz)
    }
    # From, to, length, no by
    if (from_and_to && missing_by && !missing_len){
      time_unit <- time_by(from, to, length = length.out, seq_type = seq_type)
      # Calculate by info from lubridate class object
      unit_info <- time_unit_info(time_unit)
      by_n <- unname(unit_info)[[1L]]
      by_unit <- paste0(names(unit_info), "s")
      # From, to, by, no length
    } else if (from_and_to && !missing_by && missing_len){
      time_unit <- time_unit(by_unit, type = seq_type)(by_n)
      length.out <- time_seq_len(from, to, by = by, seq_type = seq_type)

    } else if (missing_from){
      time_unit <- time_unit(by_unit, type = seq_type)(by_n)
    } else {
      time_unit <- time_unit(by_unit, type = seq_type)(by_n)
    }
    # Guess seq type
    if (seq_type == "auto") seq_type <- guess_seq_type(by_unit)
    ### After this we will always have both length and by
    if (missing_from){
      from <- to - (time_unit * length.out) + time_unit
    }
    if (seq_type == "duration"){
      out <- duration_seq(from = from,
                           length = length.out,
                           duration = duration_unit(by_unit)(by_n))
    } else {
        out <- period_seq(from = from,
                             length = length.out,
                             unit = substr(by_unit, 1L, nchar(by_unit) -1L),
                          num = by_n,
                          roll_month = roll_month,
                          roll_dst = roll_dst)
    }
    if (lubridate::tz(from) != tz){
      out <- lubridate::with_tz(out, tzone = tz)
    }
  } else {
    args <- as.list(match.call())[-1]
    args <- args[names(args) %in% c("from", "to", "by", "length.out")]
    if (!missing_from && floor_date){
      args[["from"]] <- time_floor(from, by = by)
    }
    out <- do.call(seq, args, envir = parent.frame())
  }
  out
}
#' @rdname time_seq
#' @export
time_seq_len <- function(from, to, by,
                         seq_type = c("auto", "duration", "period"),
                         as_period = FALSE){
  unit_info <- unit_guess(by)
  units <- unit_info[["unit"]]
  num <- unit_info[["num"]]
  scale <- unit_info[["scale"]]
  num <- num * scale
  time_diff <- abs(time_diff(from, to, by = setnames(list(num), units),
                         type = seq_type,
                         as_period = as_period))
  # Handles a special case when by is zero or exceeds integer limit
  if (length(time_diff) > 0L && (
    any(num == 0) || max(time_diff) > .Machine$integer.max
    )){
    trunc(time_diff) + 1
  } else {
    as.integer(time_diff) + 1L
  }
}
# This is like time_seq but without the string parsing
# This always take a start and end-point as well.
ftseq <- function(from, to, units, num = 1,
                  floor_date = FALSE,
                  week_start = getOption("lubridate.week.start", 1),
                  seq_type = c("auto", "duration", "period"),
                  roll_month = "preday", roll_dst = "pre",
                  tz = lubridate::tz(from)){
  stopifnot(length(num) == 1L)
  stopifnot(length(units) == 1L)
  if (is_time(from) && is_time(to)){
    seq_type <- match.arg(seq_type)
    if (seq_type == "auto") seq_type <- guess_seq_type(units)
    if (seq_type == "duration"){
      time_unit <- duration_unit(units)(x = num)
      num_seconds <- as.double(time_unit)
      if (!is_datetime(from)) from <- lubridate::as_datetime(from, tz = tz)
      if (floor_date) from <- lubridate::floor_date(from, unit = units,
                                                    week_start = week_start)
      # if (!is_datetime(to)) to <- lubridate::as_datetime(to, tz = tz)
      if (to < from) num_seconds <- -abs(num_seconds)
      out_length <- time_seq_len(from, to, by = setnames(list(num), units),
                            seq_type = "duration", as_period = FALSE)
      out <- duration_seq(from, length = out_length, duration = num_seconds)
    } else {
      unit <- substr(units, 1L, nchar(units) -1L)
      time_unit <- lubridate::period(num = num, units = units)
      int <- lubridate::interval(from, to)
      out_length <- abs((int %/% time_unit)) + 1
      if (floor_date) from <- lubridate::floor_date(from, unit = units,
                                                    week_start = week_start)
      if (to < from) num <- -abs(num)
      out <- period_seq(from, out_length, unit,
                        num = num)
    }
    if (length(tz) > 0 && lubridate::tz(out) != tz){
      out <- lubridate::with_tz(out, tzone = tz)
    }
  } else {
    if (floor_date) from <- time_floor(from, by = num)
    out <- seq(from = from, to = to, by = num)
  }
  out
}
# This is purely for speed purposes
duration_seq <- function(from, length, duration){
  if (!is_datetime(from)) from <- lubridate::as_datetime(from,
                                                         tz =
                                                           lubridate::tz(from))
  seq.POSIXt(from = from,
             length.out = length, by = as.double(duration))
}
# This will always calculate an increasing or decreasing sequence
# of a specified length and unit increment
period_seq <- function(from, length, unit, num = 1,
                       roll_month = "preday", roll_dst = "pre"){
  int_seq <- seq_len(length) - 1L
  if (length == 0L) from <- from[0L]
  timechange::time_add(from, periods = setnames(list(num * int_seq),
                                                       unit),
                       roll_month = roll_month, roll_dst = roll_dst)
}
# Duration sequence vectorised over from, to and num
duration_seq_v <- function(from, to, units, num = 1){
  units <- match.arg(units, .duration_units)
  tz <- lubridate::tz(from)
  if (!is_datetime(from)) from <- lubridate::as_datetime(from, tz = tz)
  duration <- duration_unit(units)(num)
  num_seconds <- as.double(duration)
  seq_len <- time_seq_len(from, to, setnames(list(num),
                                             units),
                          seq_type = "duration")
  time_seq <- sequence2(seq_len,
                        from = as.double(from),
                        by = num_seconds)
  time_cast(time_seq, from)
}
# Period sequence vectorised over from, to and num
period_seq_v3 <- function(from, to, units, num = 1,
                         roll_month = "preday", roll_dst = "pre"){
  units <- match.arg(units, .period_units)
  seq_len <- time_seq_len(from, to, by = setnames(list(num), units),
                          seq_type = "period")
  out_len <- sum(seq_len)
  unit <- substr(units, 1L, nchar(units) -1L)
  g_len <- length(seq_len)
  g_seq <- seq_len(g_len)
  # # Recycle
  from <- rep_len(from, g_len)
  num <- rep_len(num, g_len)
  # # Expand
  g <- rep(g_seq, times = seq_len)
  num <- rep(num, times = seq_len)
  # Arithmetic
  g_add <- collapse::fcumsum(rep_len(1, out_len),
                             check.o = FALSE,
                             na.rm = FALSE,
                             g = g) - 1
  num <- (g_add * num)
  # Split these by group
  by <- collapse::gsplit(num, g = g, use.g.names = FALSE)
  period_dt <- data.table::data.table(g = g_seq, from = from, by = by,
                                      key = "g")
  period_dt[, list("time" = time_add(get("from"),
                                     periods = setnames(get("by"), unit),
                                     roll_month = roll_month,
                                     roll_dst = roll_dst)),
            keyby = "g"][["time"]]
}
# This is the same as above but optimized for duplicate sequences
period_seq_v <- function(from, to, units, num = 1,
                         roll_month = "preday", roll_dst = "pre"){
  units <- match.arg(units, .period_units)
  seq_len <- time_seq_len(from, to, by = setnames(list(num), units),
                          seq_type = "period")
  out_len <- sum(seq_len)
  unit <- substr(units, 1L, nchar(units) -1L)
  if (length(from) == 0L || length(to) == 0L || length(seq_len) == 0L){
   return(from[0L])
  }
  period_df <- data.table::as.data.table(mget(c("from", "num", "seq_len")))
  period_df[, ("row_id") := seq_len(.N)]
  # We want to eliminate unecessary grouped calculations
  # To do so we need to collapse identical groups and just repeat their sequences based on number of duplicates
  period_df[, ("g") := group_id(period_df,
                                .by = all_of(c("from", "num", "seq_len")),
                                sort = FALSE)]
  period_df[, ("n") := frequencies(get("g"))]

  # It's important the result is properly ordered
  # So let's store the correct order before collapsing
  data.table::setorderv(period_df, cols = "g")
  out_order <- radix_order(rep(period_df[["row_id"]], period_df[["seq_len"]]))

  # Collapse the data frame into unique combinations of length, from, and num
  period_df <- collapse::funique(period_df, cols = "g")
  # Add key for optimised aggregation
  data.table::setkeyv(period_df, cols = "g")
  g_len <- nrow2(period_df)
  g_seq <- period_df[["g"]]
  # # Expand
  g <- rep(g_seq, times = period_df[["seq_len"]])
  num <- rep(period_df[["num"]], times = period_df[["seq_len"]])
  # Arithmetic
  g_add <- collapse::fcumsum(rep_len(1, sum(period_df[["seq_len"]])),
                             check.o = FALSE,
                             na.rm = FALSE,
                             g = g) - 1
  num <- (g_add * num)
  # Split these by group
  by <- collapse::gsplit(num, g = g, use.g.names = FALSE)
  # Repeat these by the group counts
  which_n_gt_1 <- which(period_df[["n"]] > 1)
  by[which_n_gt_1] <- purrr::map2(by[which_n_gt_1],
                                  period_df[["n"]][which_n_gt_1],
                                  function(x, y) rep(x, y))
  period_df[, ("by") := by]
  out <- period_df[, list("time" = time_add(get("from"),
                                     periods = setnames(get("by"), unit),
                                     roll_month = roll_month,
                                     roll_dst = roll_dst)),
            keyby = "g"][["time"]]
  out[out_order]
}
# # Period sequence vectorised over from, to and num
period_seq_v2 <- function(from, to, units, num = 1,
                         roll_month = "preday", roll_dst = "pre"){
  units <- match.arg(units, .period_units)
  seq_len <- time_seq_len(from, to, by = setnames(list(num), units),
                          seq_type = "period")
  unit <- substr(units, 1L, nchar(units) -1L)
  time_seq <- Vectorize(period_seq,
                        vectorize.args = c("from", "length", "num"),
                        SIMPLIFY = FALSE,
                        USE.NAMES = FALSE)(from,
                                           length = seq_len,
                                           unit = unit,
                                           num = num,
                                           roll_month = roll_month,
                                           roll_dst = roll_dst)
  # Concatenate the lists into vector
  purrr::list_c(time_seq)
}
# Base sequence vectorized over from and to
seq_v <- function(from, to, by = 1){
  seq_len <- time_seq_len(from, to,
                           by = by)
  sequence2(seq_len,
            from = from,
            by = by)
}
# Like sequence() but slower and works with decimal numbers
# Weirdly enough sequence() seems less precise than this?
# example:
# x <- Sys.time()
# seq.POSIXt(x, x + dseconds(112), by = 1) - time_cast(sequence(113, from = as.double(x), by =1 ), x)
# seq.POSIXt(x, x + dseconds(112), by = 1) - time_cast(sequence2(113, from = as.double(x), by =1 ), x)
sequence2 <- function(nvec, from, by = 1){
  out_len <- sum(nvec)
  g_len <- length(nvec)
  # Recycle
  by <- rep_len(by, g_len)
  from <- rep_len(from, g_len)
  # Expand
  by <- rep(by, times = nvec)
  from <- rep(from, times = nvec)
  # Arithmetic
  if (out_len <= .Machine$integer.max){
    g_add <- sequence(nvec, from = 1L, by = 1L) - 1L
  } else {
    g <- rep(seq_len(g_len), times = nvec)
    g_add <- collapse::fcumsum(rep_len(1, out_len),
                               check.o = FALSE,
                               na.rm = FALSE,
                               g = g) - 1
  }
  from + (g_add * by)
}
# Alternative base R only extension of sequence() that handles decimals
# sequence3 <- function(nvec, from, by = 1){
#   out_len <- sum(nvec)
#   g_len <- length(nvec)
#   # Recycle
#   by <- rep_len(by, g_len)
#   from <- rep_len(from, g_len)
#   # Expand
#   by <- rep(by, times = nvec)
#   from <- rep(from, times = nvec)
#   # Arithmetic
#   g_add <- sequence(nvec, from = 1L, by = 1L) - 1L
#   from + (g_add * by)
# }
# # Like sequence() but excepts end-point argument
# sequence4 <- function(from, to, by = 1){
#   seq_len <- time_seq_len(from, to,
#                           by = by)
#
#     by <- recycle_args(by, length = length(seq_len))[[1L]]
#     from <- recycle_args(from, length = length(seq_len))[[1L]]
#     g <- rep(seq_len(length(by)), times = seq_len)
#     by <- rep(by, times = seq_len)
#     from <- rep(from, times = seq_len)
#     g_add <- collapse::fcumsum(rep_len(1, length(g)),
#                                g = g) - 1
#     from + (g_add * by)
# }
# Vectorised time sequence function
# It is vectorized over from, to, units and num
#' @rdname time_seq
#' @export
time_seq_v <- function(from, to, units, num = 1,
                       seq_type = c("auto", "duration", "period"),
                       roll_month = "preday", roll_dst = "pre"){
  if (is_time(from) && is_time(to)){
    seq_type <- match.arg(seq_type)
    if (seq_type == "auto") seq_type <- guess_seq_type(units)
    if (seq_type == "period"){
      out <- period_seq_v(from, to, units = units, num = num,
                          roll_month = roll_month, roll_dst = roll_dst)

    } else {
      out <- duration_seq_v(from, to, units = units, num = num)
    }
  } else {
    out <- seq_v(from, to, by = num)
  }
  out
}


