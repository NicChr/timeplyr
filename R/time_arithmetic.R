

# Period arithmetic -------------------------------------------------------

# Taken from timechange to be used in a tight period sequence loop
# All credits go to the authors of timechange
C_time_add <- get("C_time_add", asNamespace("timechange"), inherits = FALSE)

timespan_as_timechange_period <- function(x){
  `names<-`(list(timespan_num(x)), plural_unit_to_single(timespan_unit(x)))
}

# timechange::time_add used for date-times
# std::chrono (C++20) used for dates which is much faster

period_add <- function(x, add, ...){
  check_is_timespan(add)
  if (!is_whole_number(timespan_num(add))){
    cli::cli_abort("{.arg add} must be a {.cls timespan} of whole numbers")
  }
  UseMethod("period_add")
}
#' @export
period_add.default <- function(x, add, ...){
  timechange::time_add(
    x, periods = timespan_as_timechange_period(add), ...
  )
}
#' @export
period_add.Date <- function(x, add, roll_month = getOption("timeplyr.roll_month", "preday"), ...){
  num <- timespan_num(add)
  unit <- timespan_unit(add)
  roll_choice <- match(roll_month, c("preday", "postday", "NA"))

  if (is.na(roll_choice)){
    # Helpful error msg
    rlang::arg_match0(roll_month, c("preday", "postday", "NA"))
    stop("error") # This should never be reached but is a fallback in case above doesn't error
  }

  switch(
    unit,
    years = {
      cpp_add_months(x, num * 12L, roll_choice)
    },
    months = {
      cpp_add_months(x, num, roll_choice)
    },
    weeks = {
      `class<-`(unclass(x) + (num * 7L), class(x))
    },
    days = {
      `class<-`(unclass(x) + num, class(x))
    },
    rlang::arg_match0(unit, .period_units)
  )
}

#' Add/subtract timespans to dates and date-times
#'
#' @description
#' A very fast method of adding time units to dates and date-times.
#'
#' @param x Time vector. \cr
#' E.g. a `Date`, `POSIXt`, `numeric` or any time-based vector.
#' @param timespan [timespan].
#' @param roll_month See `?timechange::time_add`.
#' @param roll_dst See `?timechange::time_add`.
#'
#' @details
#' The methods are continuously being improved over time.
#' Date arithmetic should be very fast regardless of the timespan supplied.
#' Date-time arithmetic, specifically
#' when supplied days, weeks, months and years, is being improved.
#'
#' @returns
#' A date, date-time, or other time-based vector.
#' @export
time_add <- function(x, timespan,
                     roll_month = getOption("timeplyr.roll_month", "preday"),
                     roll_dst = getOption("timeplyr.roll_dst", "NA")){

  span <- timespan(timespan)
  num <- timespan_num(span)
  unit <- timespan_unit(span)

  if (is.na(unit)){
    x + num
  } else {
    # If timespan is less than a day
    if (is_duration_unit(unit)){
      as_datetime2(x) + unit_to_seconds(span)
    } else {
      period_add(x, span, roll_month = roll_month, roll_dst = roll_dst)
    }
  }
}
#' @export
time_subtract <- function(x, timespan,
                          roll_month = getOption("timeplyr.roll_month", "preday"),
                          roll_dst = getOption("timeplyr.roll_dst", "NA")){
  time_add(x, -timespan, roll_month = roll_month, roll_dst = roll_dst)
}
time_floor <- function(x, time_by, week_start = getOption("lubridate.week.start", 1)){
  span <- timespan(time_by)
  num <- timespan_num(span)
  unit <- timespan_unit(span)

  if (is_time(x)){
    timechange::time_floor(x, unit = paste(num, unit), week_start = week_start)
  } else {
    floor(x / num) * num
  }
}

diff_months <- function(x, y, fractional = FALSE, n = 1L, ...){
  set_time_cast(x, y)
  UseMethod("diff_months")
}
#' @export
diff_months.default <- function(x, y, fractional = FALSE, n = 1L, ...){

  x <- as.POSIXlt(x)
  y <- as.POSIXlt(y)
  start <- unclass(x)
  end <- unclass(y)
  sy <- start[["year"]]
  sm <- start[["mon"]]
  ey <- end[["year"]]
  em <- end[["mon"]]
  smd <- start[["mday"]]
  emd <- end[["mday"]]

  out <- (12L * (ey - sy)) + (em - sm)

  l2r <- y >= x
  pos <- cheapr::val_find(l2r, TRUE)
  neg <- cheapr::val_find(l2r, FALSE)

  # Adjust for full months
  if (length(pos) > 0){
    out[pos] <- out[pos] - (emd < smd)[pos]
  }
  if (length(neg) > 0){
    out[neg] <- out[neg] + (emd > smd)[neg]
  }

  out <- as.integer(divide(out, n))

  # Fractional through the month
  if (fractional){
    start_dt <- as.POSIXct(x)
    end_dt <- as.POSIXct(y)

    int_end1 <- C_time_add(start_dt, list(month = out * n), "preday", "NA")
    if (length(n) != 1){
      n <- rep_len2(n, length(out))
    }
    int_end2 <- C_time_add(int_end1, list(month = n), "preday", "NA")
    if (length(neg) > 0L){
      int_end2[neg] <- C_time_add(int_end1, list(month = -n), "preday", "NA")
    }
    fraction <- strip_attrs(
      (unclass(end_dt) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
    )
    out <- out + fraction
  }
  out
}
#' @export
diff_months.Date <- function(x, y, fractional = FALSE, n = 1L, ...){

  start <- data.table::as.IDate(x)
  end <- data.table::as.IDate(y)
  sy <- data.table::year(start)
  sm <- data.table::month(start)
  ey <- data.table::year(end)
  em <- data.table::month(end)
  smd <- data.table::mday(start)
  emd <- data.table::mday(end)

  out <- (12L * (ey - sy)) + (em - sm)

  l2r <- y >= x
  pos <- cheapr::val_find(l2r, TRUE)
  neg <- cheapr::val_find(l2r, FALSE)

  # Adjust for full months
  if (length(pos) > 0){
    out[pos] <- out[pos] - (emd < smd)[pos]
  }
  if (length(neg) > 0){
    out[neg] <- out[neg] + (emd > smd)[neg]
  }

  out <- as.integer(divide(out, n))

  # Fractional through the month
  if (fractional){
    int_end1 <- cpp_add_months(x, new_timespan("months", out * n), 1L)

    months <- new_timespan("months", n)
    int_end2 <- cpp_add_months(int_end1, months, 1L)
    if (length(neg) > 0L){
      int_end2[neg] <- cpp_add_months(int_end1, -months, 1L)[neg]
    }
    fraction <- strip_attrs(
      (unclass(y) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
    )
    out <- out + fraction
  }
  out
  # if (fractional){
  #   zero <- cheapr::val_find(sign, 0L)
  #   int_end1 <- cpp_add_months(x, new_timespan("months", out), 1L)
  #
  #   months <- new_timespan("months", n)
  #   int_end2 <- cpp_add_months(int_end1, months, 1L)
  #   if (length(neg) > 0L){
  #     int_end2[neg] <- cpp_add_months(int_end1, -months, 1L)[neg]
  #   }
  #   fraction <- strip_attrs(
  #     (unclass(y) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
  #   )
  # } else {
  #   fraction <- 0L
  # }
  # divide(out, n) + fraction
}
#' @export
diff_months.POSIXct <- function(x, y, fractional = FALSE, n = 1L, ...){

  start <- unclass(as.POSIXlt(x))
  end <- unclass(as.POSIXlt(y))
  sy <- start[["year"]]
  sm <- start[["mon"]]
  ey <- end[["year"]]
  em <- end[["mon"]]
  smd <- start[["mday"]]
  emd <- end[["mday"]]

  out <- (12L * (ey - sy)) + (em - sm)

  l2r <- y >= x
  pos <- cheapr::val_find(l2r, TRUE)
  neg <- cheapr::val_find(l2r, FALSE)

  # Adjust for full months
  if (length(pos) > 0){
    out[pos] <- out[pos] - (emd < smd)[pos]
  }
  if (length(neg) > 0){
    out[neg] <- out[neg] + (emd > smd)[neg]
  }

  out <- as.integer(divide(out, n))

  # Fractional through the month
  if (fractional){
    int_end1 <- C_time_add(x, list(month = out * n), "preday", "NA")
    if (length(n) != 1){
      n <- rep_len2(n, length(out))
    }
    int_end2 <- C_time_add(int_end1, list(month = n), "preday", "NA")
    if (length(neg) > 0L){
      int_end2[neg] <- C_time_add(int_end1, list(month = -n), "preday", "NA")
    }
    fraction <- strip_attrs(
      (unclass(y) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
    )
    out <- out + fraction
  }
  out
}
# diff_months.default <- function(x, y, fractional = FALSE, n = 1L, ...){
#
#   x <- as.POSIXlt(x)
#   y <- as.POSIXlt(y)
#   start <- unclass(x)
#   end <- unclass(y)
#   sy <- start[["year"]]
#   sm <- start[["mon"]]
#   ey <- end[["year"]]
#   em <- end[["mon"]]
#   smd <- start[["mday"]]
#   emd <- end[["mday"]]
#
#   out <- (12L * (ey - sy)) + (em - sm)
#
#   sign <- cheapr::int_sign(out)
#
#   # Adjust for full months
#   pos <- cheapr::val_find(sign, 1L)
#   neg <- cheapr::val_find(sign, -1L)
#   out[pos] <- out[pos] - (emd < smd)[pos]
#   out[neg] <- out[neg] + (emd > smd)[neg]
#
#   out <- as.integer(divide(out, n))
#
#   # Fractional through the month
#   if (fractional){
#     zero <- cheapr::val_find(sign, 0L)
#     start_dt <- as.POSIXct(x)
#     end_dt <- as.POSIXct(y)
#
#     int_end1 <- C_time_add(start_dt, list(month = out * n), "preday", "NA")
#     if (length(n) != 1){
#       n <- rep_len2(n, length(out))
#     }
#     int_end2 <- C_time_add(int_end1, list(month = n), "preday", "NA")
#     if (length(neg) > 0L){
#       int_end2[neg] <- C_time_add(int_end1, list(month = -n), "preday", "NA")
#     }
#     fraction <- strip_attrs(
#       (unclass(end_dt) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
#     )
#     out <- out + fraction
#   }
#   out
# }
# diff_months.Date <- function(x, y, fractional = FALSE, n = 1L, ...){
#
#   start <- data.table::as.IDate(x)
#   end <- data.table::as.IDate(y)
#   sy <- data.table::year(start)
#   sm <- data.table::month(start)
#   ey <- data.table::year(end)
#   em <- data.table::month(end)
#   smd <- data.table::mday(start)
#   emd <- data.table::mday(end)
#
#   out <- (12L * (ey - sy)) + (em - sm)
#
#   sign <- cheapr::int_sign(out)
#
#   # Adjust for full months
#   pos <- cheapr::val_find(sign, 1L)
#   neg <- cheapr::val_find(sign, -1L)
#   out[pos] <- out[pos] - (emd < smd)[pos]
#   out[neg] <- out[neg] + (emd > smd)[neg]
#
#   out <- as.integer(divide(out, n))
#
#   # Fractional through the month
#   if (fractional){
#     zero <- cheapr::val_find(sign, 0L)
#     int_end1 <- cpp_add_months(x, new_timespan("months", out * n), 1L)
#
#     months <- new_timespan("months", n)
#     int_end2 <- cpp_add_months(int_end1, months, 1L)
#     if (length(neg) > 0L){
#       int_end2[neg] <- cpp_add_months(int_end1, -months, 1L)[neg]
#     }
#     fraction <- strip_attrs(
#       (unclass(y) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
#     )
#     out <- out + fraction
#   }
#   out
#   # if (fractional){
#   #   zero <- cheapr::val_find(sign, 0L)
#   #   int_end1 <- cpp_add_months(x, new_timespan("months", out), 1L)
#   #
#   #   months <- new_timespan("months", n)
#   #   int_end2 <- cpp_add_months(int_end1, months, 1L)
#   #   if (length(neg) > 0L){
#   #     int_end2[neg] <- cpp_add_months(int_end1, -months, 1L)[neg]
#   #   }
#   #   fraction <- strip_attrs(
#   #     (unclass(y) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
#   #   )
#   # } else {
#   #   fraction <- 0L
#   # }
#   # divide(out, n) + fraction
# }
# diff_months.POSIXct <- function(x, y, fractional = FALSE, n = 1L, ...){
#
#   start <- unclass(as.POSIXlt(x))
#   end <- unclass(as.POSIXlt(y))
#   sy <- start[["year"]]
#   sm <- start[["mon"]]
#   ey <- end[["year"]]
#   em <- end[["mon"]]
#   smd <- start[["mday"]]
#   emd <- end[["mday"]]
#
#   out <- (12L * (ey - sy)) + (em - sm)
#
#   sign <- cheapr::int_sign(out)
#
#   # Adjust for full months
#   pos <- cheapr::val_find(sign, 1L)
#   neg <- cheapr::val_find(sign, -1L)
#   out[pos] <- out[pos] - (emd < smd)[pos]
#   out[neg] <- out[neg] + (emd > smd)[neg]
#
#   out <- as.integer(divide(out, n))
#
#   # Fractional through the month
#   if (fractional){
#     zero <- cheapr::val_find(sign, 0L)
#     int_end1 <- C_time_add(x, list(month = out * n), "preday", "NA")
#     if (length(n) != 1){
#       n <- rep_len2(n, length(out))
#     }
#     int_end2 <- C_time_add(int_end1, list(month = n), "preday", "NA")
#     if (length(neg) > 0L){
#       int_end2[neg] <- C_time_add(int_end1, list(month = -n), "preday", "NA")
#     }
#     fraction <- strip_attrs(
#       (unclass(y) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
#     )
#     out <- out + fraction
#   }
#   out
# }
# diff_years <- function(x, y, fractional = FALSE){
#
#   start <- data.table::as.IDate(x)
#   end <- data.table::as.IDate(y)
#   sy <- data.table::year(start)
#   ey <- data.table::year(end)
#   syd <- data.table::yday(start)
#   eyd <- data.table::yday(end)
#
#   out <- ey - sy
#
#   sign <- cheapr::int_sign(out)
#
#   # Adjust for full months
#   pos <- cheapr::val_find(sign, 1L)
#   neg <- cheapr::val_find(sign, -1L)
#   out[pos] <- out[pos] - (eyd < syd)[pos]
#   out[neg] <- out[neg] + (eyd > syd)[neg]
#
#   # Fractional through the year
#   if (fractional){
#     zero <- cheapr::val_find(sign, 0L)
#     start_dt <- as_datetime2(x)
#     end_dt <- as_datetime2(y)
#     int_end1 <- C_time_add(start_dt, list(year = out), "preday", "NA")
#     int_end2 <- int_end1 # Initialise
#     int_end2[pos] <- C_time_add(int_end1[pos], list(year = 1), "preday", "NA")
#     int_end2[neg] <- C_time_add(int_end1[neg], list(year = -1), "preday", "NA")
#     int_end2[zero] <- C_time_add(int_end1[zero], list(year = 1), "preday", "NA")
#     fraction <- strip_attrs(
#       (unclass(end_dt) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
#     )
#     out <- out + fraction
#   }
#
#   out
# }

period_diff <- function(x, y, timespan){
  check_is_timespan(timespan)
  unit <- timespan_unit(timespan)
  num <- timespan_num(timespan)
  if (!is_whole_number(num)){
    cli::cli_abort("{.arg timespan} must be a {.cls timespan} of whole numbers")
  }

  # Reduce to unique x-y pairs

  interval_tbl <- cheapr::new_df(x = x, y = y, num = num, .recycle = TRUE)
  interval_groups <- collapse::group(interval_tbl, starts = TRUE, group.sizes = TRUE)
  starts <- attr(interval_groups, "starts")
  sizes <- attr(interval_groups, "group.sizes")
  n_groups <- attr(interval_groups, "N.groups")
  # If distinct pairs results in a 2x reduction in data size, then we do that
  distinct_pairs <- isTRUE((df_nrow(interval_tbl) %/% n_groups) >= 2L)
  if ( distinct_pairs ){
    interval_tbl <- cheapr::sset(interval_tbl, starts)
  }
  x <- interval_tbl$x
  y <- interval_tbl$y
  num <- interval_tbl$num

  span <- new_timespan(unit, num)

  out <- switch(
    unit,
    years = {
      diff_months(x, y, fractional = TRUE, n = num * 12L)
    },
    months = {
      diff_months(x, y, fractional = TRUE, n = num)
    },
    weeks = {

    },
    days = {

    },
    rlang::arg_match0(unit, .period_units)
  )

  if (distinct_pairs){
    out <- out[interval_groups]
  }

  out

}
