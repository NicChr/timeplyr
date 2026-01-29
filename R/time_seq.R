#' Time based version of `base::seq()`
#'
#' @details This works like `seq()`,
#' but using `timechange` for the period calculations and
#' `base::seq.POSIXT()` for the duration calculations.
#' In many ways it is improved over `seq` as
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
#' For a vectorized implementation with multiple start/end times,
#' use `time_seq_v()`/`time_seq_v2()`
#'
#' `time_seq_sizes()` is a convenience
#' function to calculate time sequence lengths, given start/end times.
#'
#' @param from Start time.
#' @param to End time.
#' @param timespan [timespan].
#' @param time_by A [timespan]. This argument may be renamed in the future.
#' @param length.out Length of the sequence.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param sizes Time sequence sizes.
#'
#' @returns
#' `time_seq` returns a time sequence. \cr
#' `time_seq_sizes` returns an integer vector of sequence sizes. \cr
#' `time_seq_v` returns time sequences. \cr
#' `time_seq_v2` also returns time sequences.
#'
#' @examples
#' library(timeplyr)
#' library(lubridate)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # Dates
#' today <- today()
#' now <- now()
#'
#' time_seq(today, today %m+% months(1), time = "day")
#' time_seq(today, length.out = 10, time = "day")
#' time_seq(today, length.out = 10, time = "hour")
#'
#' time_seq(today, today %m+% months(1), time = timespan("days", 1)) # Alternative
#' time_seq(today, today + years(1), time = "week")
#' time_seq(today, today + years(1), time = "fortnight")
#' time_seq(today, today + years(1), time = "year")
#' time_seq(today, today + years(10), time = "year")
#' time_seq(today, today + years(100), time = "decade")
#'
#' # Datetimes
#' time_seq(now, now + weeks(1), time = "12 hours")
#' time_seq(now, now + weeks(1), time = "day")
#' time_seq(now, now + years(1), time = "week")
#' time_seq(now, now + years(1), time = "fortnight")
#' time_seq(now, now + years(1), time = "year")
#' time_seq(now, now + years(10), time = "year")
#' time_seq(now, today + years(100), time = "decade")
#'
#' # You can seamlessly mix dates and datetimes with no errors.
#' time_seq(now, today + days(3), time = "day")
#' time_seq(now, today + days(3), time = "hour")
#' time_seq(today, now + days(3), time = "day")
#' time_seq(today, now + days(3), time = "hour")
#'
#' # Choose between durations or periods
#'
#' start <- dmy(31012020)
#' # If time_type is left as is,
#' # periods are used for days, weeks, months and years.
#' time_seq(start, time = months(1), length.out = 12)
#' time_seq(start, time = dmonths(1), length.out = 12)
#' # Notice how strange base R version is.
#' seq(start, by = "month", length.out = 12)
#'
#' # Roll forward or backward impossible dates
#'
#' leap <- dmy(29022020) # Leap day
#' end <- dmy(01032021)
#' # 3 different options
#' time_seq(leap, to = end, time = "year",
#'          roll_month = "NA")
#' time_seq(leap, to = end, time = "year",
#'          roll_month = "postday")
#' time_seq(leap, to = end, time = "year",
#'          roll_month = getOption("timeplyr.roll_month", "xlast"))
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#' }
#' @rdname time_seq
#' @export
time_seq <- function(from = NULL, to = NULL, time_by = NULL, length.out = NULL,
                     roll_month = getOption("timeplyr.roll_month", "xlast"),
                     roll_dst = getOption("timeplyr.roll_dst", c("NA", "xfirst"))){
  missing_from <- is.null(from)
  missing_to <- is.null(to)
  missing_by <- is.null(time_by)
  missing_len <- is.null(length.out)
  n_args <- (4L - sum(c(missing_from, missing_to, missing_by, missing_len)))
  if (n_args < 3){
    cli::cli_abort(
      "Please supply 3 args from either {.arg from}, {.arg to}, {.arg length.out} and
                     {.arg time_by}"
    )
  }
  if (n_args == 4){
    cli::cli_warn(
      "{.arg from}, {.arg to}, {.arg time_by} and {.arg length.out} have all been specified,
            the result may be unpredictable"
    )
  }
  from_and_to <- !missing_from && !missing_to
  from_and_length <- !missing_from && !missing_len
  to_and_length <- !missing_to && !missing_len

  if (from_and_to){
    # Make from and to most granular data type between them
    set_time_cast(from, to)
  }

  # Recycle

  recycled_args <- cheapr::recycle(
    from = from, to = to,
    time_by = scalar_if_else(is.null(time_by), NULL, timespan(time_by)),
    length.out = length.out
  )
  from <- recycled_args[["from"]]
  to <- recycled_args[["to"]]
  time_by <- recycled_args[["time_by"]]
  length.out <- recycled_args[["length.out"]]

  # From, to, length, no time_by
  if (from_and_to && missing_by && !missing_len){
    time_by <- time_by_calc(from, to, length = length.out)
  }
  if (from_and_to && !missing_by && missing_len){
    wrong_dir <- (unclass(from) > unclass(to) & time_by > 0) |
      (unclass(from) < unclass(to) & time_by < 0)

    # Correct the direction when user supplies impossible non-zero increment
    if (any(wrong_dir, na.rm = TRUE)){
      switch_locs <- which(wrong_dir)
      time_by[switch_locs] <- -time_by[switch_locs]
    }
    length.out <- time_seq_sizes(from, to, time_by)
  }
  ### After this we will always have both length and time_by
  if (missing_from){
    from <- time_add(
      to, -(time_by * length.out) + time_by,
      roll_month = roll_month, roll_dst = roll_dst
    )
  }
  time_seq_v2(length.out, from = from, time_by,
              roll_dst = roll_dst,
              roll_month = roll_month)
}
#' @rdname time_seq
#' @export
time_seq_sizes <- function(from, to, timespan){
  timespan <- timespan(timespan)
  set_time_cast(from, to)

  size <- time_diff(from, to, timespan)
  num <- timespan_num(timespan)
  size[cheapr::which_(from == to & num == 0L)] <- 0L

  size_rng <- collapse::frange(size, na.rm = TRUE)
  if (any(size_rng < 0, na.rm = TRUE)){
    cli::cli_abort(
      "At least 1 sequence length is negative, please check the supplied timespan increments"
    )
  }

  # Account for floating point errors
  size <- trunc(size + 1e-10) + 1
  size_rng <- trunc(size_rng + 1e-10) + 1

  if (all(size_rng <= .Machine[["integer.max"]], na.rm = TRUE)){
    size <- as.integer(size)
  }

  size
}
#' @rdname time_seq
#' @export
time_seq_v <- function(from, to, timespan,
                       roll_month = getOption("timeplyr.roll_month", "xlast"),
                       roll_dst = getOption("timeplyr.roll_dst", c("NA", "xfirst"))){
  if (length(to) == 0L){
    return(from[0])
  }
  set_time_cast(from, to)
  seq_sizes <- time_seq_sizes(from = from, to = to, timespan)
  time_seq_v2(seq_sizes, from = from, timespan,
              roll_month = roll_month,
              roll_dst = roll_dst)
}
# Alternate version of time_seq_v that accepts a sizes arg instead of to,
# Like base::sequence()
#' @rdname time_seq
#' @export
time_seq_v2 <- function(sizes, from, timespan,
                        roll_month = getOption("timeplyr.roll_month", "xlast"),
                        roll_dst = getOption("timeplyr.roll_dst", c("NA", "xfirst"))){

  if (length(from) == 0L || length(sizes) == 0L){
    return(from[0L])
  }

  timespan <- timespan(timespan)
  units <- timespan_unit(timespan)
  num <- timespan_num(timespan)

  if (is.na(units)){
    out <- sequences(sizes, from = from, by = num)
  } else {
    is_special_case_days <- units %in% c("days", "weeks") &&
      is_date(from) &&
      is_whole_number(num)
    if (is_special_case_days){
      if (units == "weeks"){
        num <- num * 7L
      }
      out <- date_seq_v2(sizes, from = from, by = num)
    } else if (is_duration_unit(units)){
      out <- duration_seq_v2(sizes, from = from, units = units, num = num)
    } else {
      out <- period_seq_v2(
        sizes, from = from, units = units, num = num,
        roll_month = roll_month, roll_dst = roll_dst
      )
    }
  }
  out
}

# Duration sequence vectorised over from, to and num
duration_seq_v <- function(from, to, units, num = 1){
  seq_sizes <- time_seq_sizes(from, to, new_timespan(units, num))
  duration_seq_v2(seq_sizes, from = from, units = units, num = num)
}
# Alternate version of duration_seq_v with sizes arg instead of to
# If you have the sequence sizes pre-calculated, you can use this
duration_seq_v2 <- function(sizes, from, units, num = 1){
  units <- rlang::arg_match0(units, .duration_units)
  from <- as_datetime2(from)
  timespan <- new_timespan(units, num)
  num_seconds <- unit_to_seconds(timespan)
  time_seq <- cheapr::sequence_(sizes,
                                from = unclass(from),
                                by = num_seconds)
  .POSIXct(time_seq, lubridate::tz(from))
}
# Date sequence vectorised over from, to and by
date_seq_v <- function(from, to, by = 1L){
  size <- cheapr::seq_size(from, to, by)
  date_seq_v2(size, from = from, by = by)
}
# Alternate version of date_seq_v with sizes arg instead of to
# If you have the sequence sizes pre-calculated, you can use this
date_seq_v2 <- function(sizes, from, by = 1L){
  out <- sequences(sizes, from = unclass(from), by = by)
  class(out) <- "Date"
  out
}
# Vectorised period sequence
period_seq_v <- function(from, to, units, num = 1,
                         roll_month = getOption("timeplyr.roll_month", "xlast"),
                         roll_dst = getOption("timeplyr.roll_dst", c("NA", "xfirst"))){
  seq_sizes <- time_seq_sizes(from, to, new_timespan(units, num))
  period_seq_v2(sizes = seq_sizes,
                from = from, units = units,
                num = num,
                roll_month = roll_month,
                roll_dst = roll_dst)
}
# Alternate version of period_seq_v with sizes arg instead of to
# If you have the sequence sizes pre-calculated, you can use this
period_seq_v2 <- function(sizes, from, units, num = 1L,
                          roll_month = getOption("timeplyr.roll_month", "xlast"),
                          roll_dst = getOption("timeplyr.roll_dst", c("NA", "xfirst"))){

  units <- rlang::arg_match0(units, .period_units)
  n_seqs <- length(sizes)

  # Vectorised time period addition

  if (length(num) != 1){
    num <- cheapr::cheapr_rep(cheapr::cheapr_rep_len(num, n_seqs), sizes)
  }
  if (length(from) != 1){
    from <- cheapr::cheapr_rep(cheapr::cheapr_rep_len(from, n_seqs), sizes)
  }
  if (is.integer(num)){
    add <- sequence(sizes, from = 0L, by = 1L) * num
  } else {
    add <- cheapr::sequence_(sizes, from = 0, by = 1) * num
  }
  time_add(
    from, new_timespan(units, add),
    roll_month = roll_month, roll_dst = roll_dst
  )
}
