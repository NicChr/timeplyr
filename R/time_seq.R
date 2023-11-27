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
#'
#' @param from Start date/datetime of sequence.
#' @param to End date/datetime of sequence.
#' @param time_by Time unit increment. \cr
#' Must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param length.out Length of the sequence.
#' @param time_floor Should `from` be floored to
#' the nearest unit specified through the `time_by`
#' argument? This is particularly useful for
#' starting sequences at the beginning of a week
#' or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `time_floor = TRUE`.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks,
#' months or years are specified, and `durations`
#' are used otherwise. If `durations`
#' are used the output is always of class `POSIXt`.
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
#' @seealso [seq_id] [time_seq_id]
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
#' time_seq(today, today + years(1), time_by = "day")
#' time_seq(today, length.out = 10, time_by = "day")
#' time_seq(today, length.out = 10, time_by = "hour")
#'
#' time_seq(today, today + years(1), time_by = list("days" = 1)) # Alternative
#' time_seq(today, today + years(1), time_by = "week")
#' time_seq(today, today + years(1), time_by = "fortnight")
#' time_seq(today, today + years(1), time_by = "year")
#' time_seq(today, today + years(10), time_by = "year")
#' time_seq(today, today + years(100), time_by = "decade")
#'
#' # Datetimes
#' time_seq(now, now + years(1), time_by = "12 hours")
#' time_seq(now, now + years(1), time_by = "day")
#' time_seq(now, now + years(1), time_by = "week")
#' time_seq(now, now + years(1), time_by = "fortnight")
#' time_seq(now, now + years(1), time_by = "year")
#' time_seq(now, now + years(10), time_by = "year")
#' time_seq(now, today + years(100), time_by = "decade")
#'
#' # You can seamlessly mix dates and datetimes with no errors.
#' time_seq(now, today + days(3), time_by = "day")
#' time_seq(now, today + days(3), time_by = "hour")
#' time_seq(today, now + days(3), time_by = "day")
#' time_seq(today, now + days(3), time_by = "hour")
#'
#' # Choose between durations or periods
#'
#' start <- dmy(31012020)
#' # If time_type is left as is,
#' # periods are used for days, weeks, months and years.
#' time_seq(start, time_by = "month", length.out = 12,
#'          time_type = "period")
#' time_seq(start, time_by = "month", length.out = 12,
#'          time_type = "duration")
#' # Notice how strange base R version is.
#' seq(start, by = "month", length.out = 12)
#'
#' # Roll forward or backward impossible dates
#'
#' leap <- dmy(29022020) # Leap day
#' end <- dmy(01032021)
#' # 3 different options
#' time_seq(leap, to = end, time_by = "year",
#'          roll_month = "NA")
#' time_seq(leap, to = end, time_by = "year",
#'          roll_month = "postday")
#' time_seq(leap, to = end, time_by = "year",
#'          roll_month = getOption("timeplyr.roll_month", "preday"))
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname time_seq
#' @export
time_seq <- function(from, to, time_by, length.out = NULL,
                     time_type = getOption("timeplyr.time_type", "auto"),
                     week_start = getOption("lubridate.week.start", 1),
                     time_floor = FALSE,
                     roll_month = getOption("timeplyr.roll_month", "preday"),
                     roll_dst = getOption("timeplyr.roll_dst", "boundary")){
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
    unit_info <- unit_guess(time_by)
    by_n <- unit_info[["num"]] * unit_info[["scale"]]
    by_unit <- unit_info[["unit"]]
    tby <- add_names(list(by_n), by_unit)
    time_by_is_num <- time_by_is_num(tby)
  } else {
    time_by_is_num <- FALSE
  }
  if (time_by_is_num || (missing_by &&
                         (!missing_from && !is_time(from) ||
                          !missing_to && !is_time(to)))){
    args <- as.list(match.call())[-1]
    args <- args[names(args) %in% c("from", "to", "time_by", "length.out")]
    if (!missing_from){
     args[["from"]] <- from
    }
    if (!missing_to){
      args[["to"]] <- to
    }
    if (!missing_from && time_floor){
      args[["from"]] <- time_floor(from, time_by)
    }
    if (!missing_by){
      time_by <- unlist(unname(time_by), recursive = FALSE, use.names = FALSE)
      if (from_and_to && length(from) > 0L && length(to) > 0L && to < from){
        time_by <- -abs(time_by)
      }
      args[["time_by"]] <- time_by
    }
    names(args)[names(args) == "time_by"] <- "by"
    out <- do.call(seq, args, envir = parent.frame())

  } else {
    time_type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
    input_time_type <- time_type
    # Guess seq type
    if (time_type == "auto" && !missing_by){
      time_type <- guess_seq_type(by_unit)
    }
    # From, to, length, no time_by
    if (from_and_to && missing_by && !missing_len){
      time_unit <- time_by_calc(from, to, length = length.out, time_type = time_type)
      # Calculate time_by info from lubridate class object
      unit_info <- time_unit_info(time_unit)
      by_n <- unname(unit_info)[[1L]]
      by_unit <- paste0(names(unit_info), "s")
      tby <- add_names(list(by_n), by_unit)
      if (time_floor){
        warning("Ambiguous how to floor from. Please supply the time_by argument.")
      }
      # From, to, time_by, no length
    }
    if (from_and_to && !missing_by && missing_len){
      if (time_floor){
        from <- time_floor2(from, time_by = tby,
                            week_start = week_start)
      }
      length.out <- time_seq_sizes(from, to, time_by = tby,
                                   time_type = input_time_type)
    }
    # Guess seq type
    if (time_type == "auto") time_type <- guess_seq_type(by_unit)
    ### After this we will always have both length and time_by
    if (missing_from){
      from <- time_add2(to, add_names(list(-(by_n * length.out) + by_n),
                                     by_unit),
                        time_type = time_type,
                        roll_month = roll_month, roll_dst = roll_dst)
      if (time_floor){
        from <- time_floor2(from, time_by = tby,
                            week_start = week_start)
      }
    }
    if (missing_to){
      if (time_floor){
        from <- time_floor2(from, time_by = tby,
                            week_start = week_start)
      }
    }
    if (!missing_to && length(from) > 0L && length(to) > 0L && to < from){
      by_n <- -abs(by_n)
      tby[[1L]] <- by_n
    }
    is_special_case_days <- {
      input_time_type == "auto" &&
        by_unit %in% c("days", "weeks") &&
        is_date(from) &&
        is_whole_number(by_n)
    }
    if (is_special_case_days){
      if (by_unit == "weeks"){
        by_n <- by_n * 7L
      }
      out <- seq.int(from = from,
                     length.out = length.out,
                     by = as.double(by_n))
      class(out) <- "Date"
    } else if (time_type == "duration"){
      out <- duration_seq(from = as_datetime2(from),
                          length = length.out,
                          duration = duration_unit(by_unit)(by_n))
    } else {
      out <- period_seq(from = from,
                        length = length.out,
                        unit = plural_unit_to_single(by_unit),
                        num = by_n,
                        roll_month = roll_month,
                        roll_dst = roll_dst)
    }
  }
  out
}
#' @rdname time_seq
#' @export
time_seq_sizes <- function(from, to, time_by,
                           time_type = getOption("timeplyr.time_type", "auto")){
  time_by <- time_by_list(time_by)
  set_time_cast(from, to)
  tdiff <- time_diff(from, to, time_by = time_by,
                     time_type = time_type)
  tdiff[cpp_which(from == to)] <- 0L
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
time_seq_v <- function(from, to, time_by,
                       time_type = getOption("timeplyr.time_type", "auto"),
                       roll_month = getOption("timeplyr.roll_month", "preday"),
                       roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                       time_floor = FALSE,
                       week_start = getOption("lubridate.week.start", 1)){
  time_by <- time_by_list(time_by)
  units <- time_by_unit(time_by)
  num <- time_by_num(time_by)
  set_time_cast(from, to)
  if (time_floor){
    from <- time_floor2(from, time_by = time_by, week_start = week_start)
  }
  seq_sizes <- time_seq_sizes(from = from, to = to,
                              time_by = time_by,
                              time_type = time_type)
  time_seq_v2(seq_sizes, from = from,
              time_by = time_by,
              time_type = time_type,
              time_floor = FALSE,
              roll_month = roll_month,
              roll_dst = roll_dst)
}
# Alternate version of time_seq_v that accepts a sizes arg instead of to,
# Like base::sequence()
#' @rdname time_seq
#' @export
time_seq_v2 <- function(sizes, from, time_by,
                        time_type = getOption("timeplyr.time_type", "auto"),
                        time_floor = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        roll_month = getOption("timeplyr.roll_month", "preday"),
                        roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  time_by <- time_by_list(time_by)
  units <- time_by_unit(time_by)
  num <- time_by_num(time_by)
  if (time_floor){
    from <- time_floor2(from, time_by, week_start = week_start)
  }
  if (time_by_is_num(time_by)){
    out <- sequence2(sizes, from = from, by = num)
  } else {
    time_type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
    is_special_case_days <- time_type == "auto" &&
      units %in% c("days", "weeks") &&
      is_date(from) &&
      is_whole_number(num)
    if (time_type == "auto") time_type <- guess_seq_type(units)
    if (is_special_case_days){
      out <- date_seq_v2(sizes, from = from, units = units, num = num)
    } else if (time_type == "period"){
      out <- period_seq_v2(sizes, from = from, units = units, num = num,
                           roll_month = roll_month, roll_dst = roll_dst)

    } else {
      out <- duration_seq_v2(sizes, from = from, units = units, num = num)
    }
  }
  out
}
# faster seq.Date() and handles zero length from differently
# by must be numeric
date_seq <- function(from, to, by = 1L){
  if (length(from) == 0L) return(from)
  check_is_date(from)
  check_is_date(to)
  out <- seq.int(from = unclass(from),
                 to = unclass(to),
                 by = by)
  class(out) <- "Date"
  out
}
# datetime sequence using from + length + by
duration_seq <- function(from, length, duration){
  if (length(from) == 0L){
    check_is_datetime(from)
    return(from)
  }
  if (length < 0 || length == Inf){
    stop("length must be a non-negative integer")
  }
  seq.POSIXt(from = from,
             length.out = length, by = unclass(duration))
}
# datetime sequence using from + to + by
duration_seq2 <- function(from, to, duration){
  if (length(from) == 0L){
    check_is_datetime(from)
    return(from)
  }
  if (length(to) == 0L){
    check_is_datetime(to)
    return(to)
  }
  seq.POSIXt(from = from, to = to, by = unclass(duration))
}
# This will always calculate an increasing or decreasing sequence
# of a specified length and unit increment
period_seq <- function(from, length, unit, num = 1,
                       roll_month = getOption("timeplyr.roll_month", "preday"),
                       roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  if (length(from) == 0L){
    length <- 0L
  }
  if (length < 0 || length == Inf){
    stop("length must be a non-negative integer")
  }
  int_seq <- seq_len(length) - 1L
  if (length == 0L){
    from <- from[0L]
  }
  time_add(from, periods = add_names(list(num * int_seq),
                                                       unit),
                       roll_month = roll_month, roll_dst = roll_dst)
}
# Duration sequence vectorised over from, to and num
duration_seq_v <- function(from, to, units, num = 1){
  time_by <- add_names(list(num), units)
  seq_sizes <- time_seq_sizes(from, to, time_by = time_by,
                              time_type = "duration")
  duration_seq_v2(seq_sizes, from = from, units = units, num = num)
}
# Alternate version of duration_seq_v with sizes arg instead of to
# If you have the sequence sizes pre-calculated, you can use this
duration_seq_v2 <- function(sizes, from, units, num = 1){
  units <- rlang::arg_match0(units, .duration_units)
  from <- as_datetime2(from)
  time_by <- add_names(list(num), units)
  num_seconds <- unit_to_seconds(time_by)
  time_seq <- double_sequence(sizes,
                              from = time_as_number(from),
                              by = num_seconds)
  .POSIXct(time_seq, lubridate::tz(from))
  # time_cast(time_seq, from)
}
# Date sequence vectorised over from, to and num
date_seq_v <- function(from, to, units = c("days", "weeks"), num = 1L){
  check_is_date(to)
  seq_sizes <- time_seq_sizes(from, to, time_by = add_names(list(num), units))
  date_seq_v2(seq_sizes,
              from = from, units = units, num = num)
}
# Alternate version of date_seq_v with sizes arg instead of to
# If you have the sequence sizes pre-calculated, you can use this
date_seq_v2 <- function(sizes, from, units = c("days", "weeks"), num = 1L){
  units <- rlang::arg_match0(units, c("days", "weeks"))
  check_is_date(from)
  if (units == "weeks"){
    units <- "days"
    num <- num * 7L
  }
  out <- sequence2(sizes, from = unclass(from), by = num)
  class(out) <- "Date"
  out
}
# (Semi) Vectorised period sequence
# Duplicate from/to/by values are grouped together and
# their sequences are repeated at the end.
period_seq_v <- function(from, to, units, num = 1,
                         roll_month = getOption("timeplyr.roll_month", "preday"),
                         roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  units <- rlang::arg_match0(units, .period_units)
  if (length(to) == 0L){
    return(vec_head(from, n = 0L))
  }
  seq_sizes <- time_seq_sizes(from, to, time_by = add_names(list(num), units),
                              time_type = "period")
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
                          roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  units <- rlang::arg_match0(units, .period_units)
  out_len <- sum(sizes)
  unit <- plural_unit_to_single(units)
  if (length(from) == 0L || length(sizes) == 0L){
    return(from[0L])
  }
  # Following timechange rules.
  convert_back_to_date <- is_date(from) &&
    unit %in% c("day", "week", "month", "year")
  period_df <- recycle_args(from, num, sizes, use.names = TRUE)
  data.table::setDT(period_df)
  period_df[, ("row_id") := seq_len(.N)]
  # We want to eliminate unnecessary grouped calculations
  # To do so we need to collapse identical groups and just repeat their sequences based on number of duplicates
  grps <- df_to_GRP(period_df, .cols = c("from", "num", "sizes"),
                    order = TRUE,
                    return.groups = FALSE)
  period_df[, ("g") := GRP_group_id(grps)]
  period_df[, ("n") := GRP_group_sizes(grps)[GRP_group_id(grps)]]

  # It's important the result is properly ordered
  # So let's store the correct order before collapsing
  period_df <- df_row_slice(period_df, GRP_order(grps))
  out_order <- radix_order(rep.int(period_df[["row_id"]],
                                   period_df[["sizes"]]))

  # Collapse the data frame into unique combinations of length, from, and num
  period_df <- collapse::funique(period_df, cols = "g")
  # Setting up vector arithmetic
  g <- rep.int(period_df[["g"]], times = period_df[["sizes"]])
  num <- sequence2(period_df[["sizes"]], from = 1L, by = period_df[["num"]]) - 1L
  # Split these by group
  by <- collapse::gsplit(num, g = g, use.g.names = FALSE)
  # Repeat these by the group counts
  group_counts <- period_df[["n"]]
  which_n_gt_1 <- cpp_which(group_counts > 1L)
  for (ind in which_n_gt_1){
    by[ind][[1L]] <- rep.int(.subset2(by, ind),
                             .subset2(group_counts, ind))
  }
  out_sizes <- as.integer(period_df[["sizes"]] * group_counts)
  # Counter to keep track of which indices to replace
  init <- 0L
  from <- period_df[["from"]]
  from <- as_datetime2(from)
  # Setnames on the list for timechange::time_add
  by <- add_names(by, rep_len(unit, length(by)))
  out <- vector("list", df_nrow(period_df))
  for (i in df_seq_along(period_df)){
    out[[i]] <- C_time_add(from[i], .subset(by, i), roll_month, roll_dst)
  }
  out <- unlist(out, recursive = FALSE, use.names = FALSE)
  out <- time_cast(out, from)
  if (convert_back_to_date){
    out <- lubridate::as_date(out)
  }
  out[out_order]
}
# Period sequence vectorised over from, to and num
# period_seq_v3 <- function(from, to, units, num = 1,
#                          roll_month = getOption("timeplyr.roll_month", "preday"), roll_dst = getOption("timeplyr.roll_dst", "boundary")){
#   units <- match.arg(units, .period_units)
#   seq_len <- time_seq_len(from, to, by = add_names(list(num), units),
#                           seq_type = "period")
#   out_len <- sum(seq_len)
#   unit <- substr(units, 1L, nchar(units) -1L)
#   g_len <- length(seq_len)
#   g_seq <- seq_len(g_len)
#   # # Recycle
#   from <- rep_len(from, g_len)
#   num <- rep_len(num, g_len)
#   # # Expand
#   g <- rep(g_seq, times = seq_len)
#   num <- rep(num, times = seq_len)
#   # Arithmetic
#   g_add <- collapse::fcumsum(seq_ones(out_len),
#                              check.o = FALSE,
#                              na.rm = FALSE,
#                              g = g) - 1
#   num <- (g_add * num)
#   # Split these by group
#   by <- collapse::gsplit(num, g = g, use.g.names = FALSE)
#   period_dt <- data.table::data.table(g = g_seq, from = from, by = by,
#                                       key = "g")
#   period_dt[, list("time" = time_add(get("from"),
#                                      periods = add_names(as.list(get("by")), unit),
#                                      roll_month = roll_month,
#                                      roll_dst = roll_dst)),
#             keyby = "g"][["time"]]
# }
