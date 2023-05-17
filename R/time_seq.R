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
#' `time_seq_v()` is a vectorized version of `time_seq()`.\cr
#'
#' `time_seq_len()` is a vectorized convenience
#' function to calculate the length/size of
#' a regular time sequence, given a start, end and unit specification.\cr
#'
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
#' means Monday (default), 7 means Sunday.
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
#'
#' @examples
#' library(timeplyr)
#' library(lubridate)
#'
#' # Dates
#' today <- today()
#' now <- now()
#'
#' time_seq(today, today + years(1), by = "day")
#' time_seq(today, length.out = 10, by = "day")
#' time_seq(today, length.out = 10, by = "hour")
#'
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
  input_seq_type <- seq_type
  if (!missing_len) stopifnot(sign(length.out) >= 0)
  n_args <- (4 - sum(c(missing_from, missing_to, missing_by, missing_len)))
  if (n_args < 3){
      stop("Please supply 3 from either from, to, length.out and by")
  }
  if (n_args == 4){
    warning("from, to, by and length.out have all been specified,
            the result may be unpredictable.")
  }
  if (!missing_from) stopifnot(length(from) <= 1L)
  if (!missing_to) stopifnot(length(to) <= 1L)
  if (!missing_by) stopifnot(length(by) <= 1L)
  if (!missing_len) stopifnot(length(length.out) <= 1L)
  from_and_to <- !missing_from && !missing_to
  from_and_length <- !missing_from && !missing_len
  to_and_length <- !missing_to && !missing_len
  if (from_and_to){
    if (length(from) == 1L && length(to) == 0L){
      stop("to must be of length 1")
    }
  }
  # Basically if from and/or to are time do x else y
  if ( (
    (!missing_from && (is_time(from) || is.character(from))) ||
    (!missing_to && (is_time(to) || is.character(to)))
  )){
    # Unit parsing
    if (!missing_by){
      unit_info <- unit_guess(by)
      by_n <- unit_info[["num"]] * unit_info[["scale"]]
      by_unit <- unit_info[["unit"]]
      # Guess seq type
      if (seq_type == "auto") seq_type <- guess_seq_type(by_unit)
    }
    # seq_type <- match.arg(seq_type)
    # Help the user by converting very common date formats
    if (!missing_from) from <- convert_common_dates(from)
    if (!missing_to) to <- convert_common_dates(to)
    # Make from and to most granular data type between them
    if (from_and_to){
      set_time_cast(from, to)
    }
    # From, to, length, no by
    if (from_and_to && missing_by && !missing_len){
      time_unit <- time_by(from, to, length = length.out, seq_type = seq_type)
      # Calculate by info from lubridate class object
      unit_info <- time_unit_info(time_unit)
      by_n <- unname(unit_info)[[1L]]
      by_unit <- paste0(names(unit_info), "s")
      if (floor_date){
        warning("Ambiguous how to floor from. Please supply the by argument.")
      }
      # From, to, by, no length
    } else if (from_and_to && !missing_by && missing_len){
      time_unit <- time_unit(by_unit, type = seq_type)(by_n)
      if (floor_date) from <- time_floor(from, by = by_unit,
                                         week_start = week_start)
      length.out <- time_seq_len(from, to, by = by, seq_type = input_seq_type)

    } else if (missing_from){
      time_unit <- time_unit(by_unit, type = seq_type)(by_n)
    } else {
      time_unit <- time_unit(by_unit, type = seq_type)(by_n)
    }
    # Guess seq type
    if (seq_type == "auto") seq_type <- guess_seq_type(by_unit)
    ### After this we will always have both length and by
    if (missing_from){
      from <- time_add2(to, setnames(list(-(by_n * length.out) + by_n),
                                     by_unit),
                        type = seq_type,
                        roll_month = roll_month, roll_dst = roll_dst)
      if (floor_date){
        from <- time_floor(from, by = by_unit,
                           week_start = week_start)
      }
    }
    if (missing_to){
      if (floor_date){
        from <- time_floor(from, by = by_unit,
                           week_start = week_start)
      }
    }
    if (!missing_to && length(from) > 0L && length(to) > 0L && to < from){
      by_n <- -abs(by_n)
    }
    # # Special case where everything is based on days
    # special_case_days <- is_special_case_days(from = from, to = lubridate::Date(0),
    #                                           unit = by_unit, num = by_n, seq_type = input_seq_type)
    # if (special_case_days){
    #   if (by_unit == "weeks"){
    #     by_unit <- "days"
    #     by_n <- by_n * 7
    #   }
    #   out <- date_seq(from = from, length.out = length.out, by = by_n)
    # } else
    if (seq_type == "duration"){
      from <- lubridate::as_datetime(from)
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
    if (!missing_by){
      by <- unlist(unname(by))
      if (from_and_to && length(from) > 0L && length(to) > 0L && to < from){
        by <- -abs(by)
      }
      args[["by"]] <- by
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
  tby <- time_by_list(by)
  tdiff <- abs(time_diff(from, to, by = tby,
                         type = seq_type,
                         as_period = as_period))
  # Accounting for when from - to / by = 0 / 0
  tdiff[is.nan(tdiff)] <- 0
  # tdiff[tby[[1L]] == 0 & from == to] <- 0
  if (length(tdiff) == 0L || (collapse::fmax(tdiff) + 1) <= .Machine$integer.max){
    as.integer(tdiff) + 1L
  } else {
    tdiff + 1
  }
}
#' @rdname time_seq
#' @export
time_seq_v <- function(from, to, by,
                       seq_type = c("auto", "duration", "period"),
                       roll_month = "preday", roll_dst = "pre"){
  unit_info <- unit_guess(by)
  units <- unit_info[["unit"]]
  num <- unit_info[["num"]]
  scale <- unit_info[["scale"]]
  num <- num * scale
  if (is_time(from) && is_time(to)){
    seq_type <- match.arg(seq_type)
    set_time_cast(from, to)
    is_special_case_days <- is_special_case_days(from = from,
                                                 to = to,
                                                 unit = units,
                                                 num = num,
                                                 seq_type = seq_type)
    if (seq_type == "auto") seq_type <- guess_seq_type(units)
    if (is_special_case_days){
      out <- date_seq_v(from, to, units = units, num = num)
    } else if (seq_type == "period"){
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
# faster seq.Date() and handles zero length from differently
# by must be numeric
date_seq <- function(from, to, by = 1L){
  if (length(from) == 0L) return(from)
  out <- seq.int(from = as.double(from),
                 to = as.double(to),
                 by = by)
  attr(out, "class") <- "Date"
  out
  # seq.Date(from, ...)
}
# ftseq differs from time_seq() in that it doesn't accept missing arguments,
# (except) for non time from/to, always has a start and end point,
# replaces by with unit and num arguments, and doesn't change the
# sign of unit increment. This is more suited for loops.
ftseq <- function(from, to, units, num = 1,
                  floor_date = FALSE,
                  week_start = getOption("lubridate.week.start", 1),
                  seq_type = c("auto", "duration", "period"),
                  roll_month = "preday", roll_dst = "pre"){
  if (length(num) != 1L) stop("num must be of length 1.")
  if (length(units) != 1L) stop("num must be of length 1.")
    seq_type <- rlang::arg_match0(seq_type, c("auto", "duration", "period"))
    is_special_case_days <- is_special_case_days(from = from,
                                                 to = to,
                                                 unit = units,
                                                 num = num,
                                                 seq_type = seq_type)
    if (seq_type == "auto") seq_type <- guess_seq_type(units)
    if (is_special_case_days){
      if (floor_date){
        from <- time_floor(from, by = setnames(list(1), units),
                                         week_start = week_start)
      }
      if (units == "weeks"){
        units <- "days"
        num <- num * 7
      }
      out <- date_seq(from, to = to, by = num)
      # out <- date_seq(from, length = out_length, by = round(num_seconds / 86400))
    } else {
      tby <- setnames(list(num), units)
      if (seq_type == "duration"){
        from <- as_datetime2(from)
        to <- as_datetime2(to)
        if (floor_date){
          from <- time_floor(from, by = setnames(list(1), units),
                             week_start = week_start)
        }
        num_seconds <- unit_to_seconds(tby)
        out <- duration_seq2(from = from, to = to, duration = num_seconds)
      } else if (seq_type == "period"){
        if (floor_date){
          from <- time_floor(from, by = setnames(list(1), units),
                             week_start = week_start)
        }
        out_length <- time_seq_len(from, to, by = tby,
                                   seq_type = "period",
                                   as_period = FALSE)
        unit <- substr(units, 1L, nchar(units) -1L)
        out <- period_seq(from, out_length, unit,
                          num = num,
                          roll_month = roll_month, roll_dst = roll_dst)
      } else {
        if (floor_date){
          from <- time_floor(from, by = num,
                             week_start = week_start)
        }
        out <- seq(from = from, to = to, by = num)
      }
    }
  out
}
# datetime sequence using from + length + by
duration_seq <- function(from, length, duration){
  if (length(from) == 0L){
    if (!is_datetime(from)) stop("from must be a datetime")
    return(from)
  }
  seq.POSIXt(from = from,
             length.out = length, by = as.double(duration))
}
# datetime sequence using from + to + by
duration_seq2 <- function(from, to, duration){
  if (length(from) == 0L){
    if (!is_datetime(from)){
      stop("to must be a datetime")
    }
    return(from)
  }
  if (length(to) == 0L){
    if (!is_datetime(to)){
      stop("to must be a datetime")
    }
    return(to)
  }
  seq.POSIXt(from = from, to = to, by = as.double(duration))
}
# This will always calculate an increasing or decreasing sequence
# of a specified length and unit increment
period_seq <- function(from, length, unit, num = 1,
                       roll_month = "preday", roll_dst = "pre"){
  if (length(from) == 0L){
    length <- 0L
  }
  int_seq <- seq_len(length) - 1L
  if (length == 0L){
    from <- from[0L]
  }
  time_add(from, periods = setnames(list(num * int_seq),
                                                       unit),
                       roll_month = roll_month, roll_dst = roll_dst)
}
# Duration sequence vectorised over from, to and num
duration_seq_v <- function(from, to, units, num = 1){
  units <- match.arg(units, .duration_units)
  from <- time_cast(from, lubridate::POSIXct(0, tz = lubridate::tz(from)))
  duration <- duration_unit(units)(num)
  num_seconds <- as.double(duration)
  seq_len <- time_seq_len(from, to, setnames(list(num),
                                             units),
                          seq_type = "duration")
  time_seq <- sequence3(seq_len,
                        from = as.double(from),
                        by = num_seconds)
  time_cast(time_seq, from)
}
# Date sequence vectorised over from, to and num
date_seq_v <- function(from, to, units = c("days", "weeks"), num = 1){
  units <- match.arg(units)
  stopifnot(is_date(from))
  stopifnot(is_date(to))
  if (units == "weeks"){
    units <- "days"
    num <- num * 7
  }
  seq_len <- time_seq_len(from, to, by = setnames(list(num), units))
  time_seq <- sequence3(seq_len,
                        from = as.double(from),
                        by = num)
  attr(time_seq, "class") <- "Date"
  time_seq
  # lubridate::as_date(time_seq)
}
# Period sequence vectorised over from, to and num
# period_seq_v3 <- function(from, to, units, num = 1,
#                          roll_month = "preday", roll_dst = "pre"){
#   units <- match.arg(units, .period_units)
#   seq_len <- time_seq_len(from, to, by = setnames(list(num), units),
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
#                                      periods = setnames(as.list(get("by")), unit),
#                                      roll_month = roll_month,
#                                      roll_dst = roll_dst)),
#             keyby = "g"][["time"]]
# }
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
  period_df <- recycle_args(from, num, seq_len, use.names = TRUE)
  data.table::setDT(period_df)
  # period_df <- data.table::as.data.table(mget(c("from", "num", "seq_len")))
  period_df[, ("row_id") := seq_len(.N)]
  # We want to eliminate unnecessary grouped calculations
  # To do so we need to collapse identical groups and just repeat their sequences based on number of duplicates
  period_df[, ("g") := group_id.default(.SD, order = FALSE),
            .SDcols = c("from", "num", "seq_len")]
  period_df[, ("n") := collapse::GRPN(get("g"), expand = TRUE)]

  # It's important the result is properly ordered
  # So let's store the correct order before collapsing
  data.table::setorderv(period_df, cols = "g")
  out_order <- radix_order(rep.int(period_df[["row_id"]],
                                   period_df[["seq_len"]]))

  # Collapse the data frame into unique combinations of length, from, and num
  period_df <- collapse::funique(period_df, cols = "g")
  # Add key for optimised aggregation
  # data.table::setkeyv(period_df, cols = "g")

  # Setting up vector arithmetic
  g <- rep.int(period_df[["g"]], times = period_df[["seq_len"]])
  num <- sequence3(period_df[["seq_len"]], from = 1, by = period_df[["num"]]) - 1
  # Split these by group
  by <- collapse::gsplit(num, g = g, use.g.names = FALSE)
  # Repeat these by the group counts
  group_counts <- period_df[["n"]]
  which_n_gt_1 <- which(group_counts > 1)
  by[which_n_gt_1] <- purrr::map2(by[which_n_gt_1],
                                  group_counts[which_n_gt_1],
                                  function(x, y) rep.int(x, y))
  # Alternate method, works similarly
  # period_df[, ("by") := by]
  # out <- period_df[, list("time" = time_add(get("from"),
  #                                    periods = setnames(as.list(get("by")), unit),
  #                                    roll_month = roll_month,
  #                                    roll_dst = roll_dst)),
  #           keyby = "g"][["time"]]
  out_sizes <- as.integer(period_df[["seq_len"]] * group_counts)
  # Counter to keep track of which indices to replace
  init <- 0L
  from <- period_df[["from"]]
  # Initialise output
  out <- rep_len(from[0L],
                sum(out_sizes))
  # Setnames on the list for timechange::time_add
  by <- setnames(by, rep_len(unit, length(by)))
  for (i in df_seq_along(period_df)){
    setv(out, seq_len(out_sizes[[i]]) + init,
         time_add(from[[i]],
                  periods = by[i],
                  roll_month = roll_month,
                  roll_dst = roll_dst),
         vind1 = TRUE)
    init <- init + out_sizes[[i]]
  }
  out[out_order]
}
