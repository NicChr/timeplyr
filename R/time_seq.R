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
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # Dates
#' today <- today()
#' now <- now()
#'
#' time_seq(today, today + months(1), time = "day")
#' time_seq(today, length.out = 10, time = "day")
#' time_seq(today, length.out = 10, time = "hour")
#'
#' time_seq(today, today + months(1), time = timespan("days", 1)) # Alternative
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
#'          roll_month = getOption("timeplyr.roll_month", "preday"))
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#' }
#' @rdname time_seq
#' @export
time_seq <- function(from, to, time_by, length.out = NULL,
                     roll_month = getOption("timeplyr.roll_month", "preday"),
                     roll_dst = getOption("timeplyr.roll_dst", "NA")){
  missing_from <- missing(from)
  missing_to <- missing(to)
  missing_by <- missing(time_by)
  missing_len <- is.null(length.out)
  if (!missing_len && !sign(length.out) >= 0){
    stop("length.out must be positive")
  }
  n_args <- (4 - sum(c(missing_from, missing_to, missing_by, missing_len)))
  if (n_args < 3){
      stop("Please supply 3 from either from, to, length.out and time_by")
  }
  if (n_args == 4){
    warning("from, to, time_by and length.out have all been specified,
            the result may be unpredictable.")
  }
  if (!missing_from && length(from) > 1L) stop("from must be of length 1")
  if (!missing_to && length(to) > 1L) stop("to must be of length 1")
  if (!missing_by && length(time_by) > 1L) stop("time_by must be of length 1")
  if (!missing_len && length(length.out) > 1L) stop("length.out must be of length 1")
  from_and_to <- !missing_from && !missing_to
  from_and_length <- !missing_from && !missing_len
  to_and_length <- !missing_to && !missing_len
  if (from_and_to){
    if (length(from) == 1L && length(to) == 0L){
      stop("to must be of length 1")
    }
    # Make from and to most granular data type between them
    set_time_cast(from, to)
  }
  # Unit parsing
  if (!missing_by){
    unit_info <- timespan(time_by)
    by_n <- timespan_num(unit_info)
    by_unit <- timespan_unit(unit_info)
    tby <- unit_info
    has_unit <- timespan_has_unit(unit_info)
  } else {
    has_unit <- FALSE
  }
    # From, to, length, no time_by
    if (from_and_to && missing_by && !missing_len){
      time_unit <- time_by_calc(from, to, length = length.out)
      # Calculate time_by info from lubridate class object
      unit_info <- timespan(time_unit)
      by_n <- timespan_num(unit_info)
      by_unit <- timespan_unit(unit_info)
      tby <- new_timespan(by_unit, by_n)
      # From, to, time_by, no length
    }
    if (from_and_to && !missing_by && missing_len){
      length.out <- time_seq_sizes(from, to, tby)
    }
    ### After this we will always have both length and time_by
    if (missing_from){
      from <- time_add(
        to, timespan(by_unit, -(by_n * length.out) + by_n),
        roll_month = roll_month, roll_dst = roll_dst
      )
    }
    if (!missing_to && length(from) > 0L && length(to) > 0L && to < from){
      by_n <- -abs(by_n)
      tby <- timespan(timespan_unit(tby), by_n)
    }
  time_seq_v2(length.out, from = from, tby,
              roll_dst = roll_dst,
              roll_month = roll_month)
}
#' @rdname time_seq
#' @export
time_seq_sizes <- function(from, to, timespan){
  timespan <- timespan(timespan)
  set_time_cast(from, to)
  tdiff <- time_diff(from, to, timespan)
  tdiff[which(from == to)] <- 0L
  tdiff_rng <- collapse::frange(tdiff, na.rm = TRUE)
  if (isTRUE(any(tdiff_rng < 0))){
    stop("At least 1 sequence length is negative, please check the time_by unit increments")
  }
  if (length(tdiff) == 0 || all(is_integerable(abs(tdiff_rng) + 1), na.rm = TRUE)){
    if (is.integer(tdiff)){
      tdiff + 1L
    } else {
      as.integer(tdiff + 1e-10) + 1L
    }
  } else {
    trunc(tdiff + 1e-10) + 1
  }
}
#' @rdname time_seq
#' @export
time_seq_v <- function(from, to, timespan,
                       roll_month = getOption("timeplyr.roll_month", "preday"),
                       roll_dst = getOption("timeplyr.roll_dst", "NA")){
  timespan <- timespan(timespan)
  units <- timespan_unit(timespan)
  num <- timespan_num(timespan)
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
                        roll_month = getOption("timeplyr.roll_month", "preday"),
                        roll_dst = getOption("timeplyr.roll_dst", "NA")){
  timespan <- timespan(timespan)
  units <- timespan_unit(timespan)
  num <- timespan_num(timespan)

  if (!timespan_has_unit(timespan)){
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
                         roll_month = getOption("timeplyr.roll_month", "preday"),
                         roll_dst = getOption("timeplyr.roll_dst", "NA")){
  if (length(to) == 0L){
    return(from[0])
  }
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
                          roll_month = getOption("timeplyr.roll_month", "preday"),
                          roll_dst = getOption("timeplyr.roll_dst", "NA")){
  units <- rlang::arg_match0(units, .period_units)
  n_seqs <- length(sizes)

  if (length(from) == 0L || n_seqs == 0L){
    return(from[0L])
  }

  # Vectorised time period addition

  if (length(num) != 1){
    num <- rep2(rep_len2(num, n_seqs), sizes)
  }
  if (length(from) != 1){
    from <- rep2(rep_len2(from, n_seqs), sizes)
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
