

# Period arithmetic -------------------------------------------------------

# Taken from timechange to be used in a tight period sequence loop
# All credits go to the authors of timechange
C_time_add <- get("C_time_add", asNamespace("timechange"), inherits = FALSE)

timespan_as_timechange_period <- function(x){
  `names<-`(list(timespan_num(x)), plural_unit_to_single(timespan_unit(x)))
}

# timechange::time_add used for date-times
# std::chrono (C++20) can also be used for adding months
# (see earlier commits for method)

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
    as.POSIXct(x), periods = timespan_as_timechange_period(add), ...
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
    stop("Internal error") # This should never be reached but is a fallback in case above doesn't error
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
                     roll_dst = getOption("timeplyr.roll_dst", c("NA", "pre"))){

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
time_subtract <- function(x, timespan,
                          roll_month = getOption("timeplyr.roll_month", "preday"),
                          roll_dst = getOption("timeplyr.roll_dst", c("NA", "pre"))){
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

# Extract the "clock-time" as the number of seconds of the day since midnight
# Useful for comparing clock-times
clock_seconds <- function(x){
  posixlt <- unclass(as.POSIXlt(x))
  hour <- posixlt[["hour"]]
  min <- posixlt[["min"]]
  sec <- as.integer(posixlt[["sec"]])

  # The below seconds capture the clock time, not the number of seconds that
  # have truly passed since midnight
  # So if the clocks go back an hour for DST, both these metrics
  # measure the same number of seconds
  sec + (min * 60L) + (hour * 3600L)
}

# Internal function to calculate differences in months
# arg `n` is used for differences in blocks of multiple months
diff_months <- function(x, y, n = 1L, fractional = FALSE, ...){
  set_time_cast(x, y)
  UseMethod("diff_months", x)
}
#' @export
diff_months.default <- function(x, y, n = 1L, fractional = FALSE, ...){

  x <- as.POSIXct(x)
  y <- as.POSIXct(y)
  diff_months(x, y, fractional = fractional, n = n, ...)
}
#' @export
diff_months.Date <- function(x, y, n = 1L, fractional = FALSE, ...){

  start <- data.table::as.IDate(x)
  end <- data.table::as.IDate(y)
  sy <- data.table::year(start)
  sm <- data.table::month(start)
  ey <- data.table::year(end)
  em <- data.table::month(end)
  smd <- data.table::mday(start)
  emd <- data.table::mday(end)

  out <- (12L * (ey - sy)) + (em - sm)

  l2r <- end >= start

  out <- cheapr::cheapr_if_else(l2r, out - (emd < smd), out + (emd > smd))

  # pos <- cheapr::val_find(l2r, TRUE)
  # neg <- cheapr::val_find(l2r, FALSE)

  # Adjust for full months
  # if (length(pos) > 0){
  #   out[pos] <- out[pos] - (emd < smd)[pos]
  # }
  # if (length(neg) > 0){
  #   out[neg] <- out[neg] + (emd > smd)[neg]
  # }

  out <- as.integer(divide(out, cheapr::val_replace(n, 0, NA)))

  # Fractional through the month
  if (fractional){
    temp <- out * n

    small_int_start <- cpp_add_months(
      start, new_timespan("months", temp), roll_month = 1L
    )
    big_int_end <- cpp_add_months(
      start, new_timespan("months", temp + cheapr::cheapr_if_else(l2r, n, -n)), roll_month = 1L
      )
    # big_int_end <- cpp_add_months(start, new_timespan("months", temp + n), roll_month = 1L)
    # if (length(neg) > 0L){
    #   big_int_end[neg] <- cpp_add_months(start, new_timespan("months", temp - n), roll_month = 1L)[neg]
    # }
    fraction <- strip_attrs(
      (unclass(end) - unclass(small_int_start)) /
        abs(unclass(big_int_end) - unclass(small_int_start))
    )
    fraction[start == end] <- 0
    if (!all_val(fraction, 0)){
      out <- out + fraction
    }
  }
  out
}
#' @export
diff_months.POSIXct <- function(x, y, n = 1L, fractional = FALSE, ...){

  x <- as.POSIXct(x)
  y <- as.POSIXct(y)

  start <- as.POSIXlt(x)
  end <- as.POSIXlt(y)

  sy <- start$year
  ey <- end$year
  sm <- start$mon
  em <- end$mon
  smd <- start$mday
  emd <- end$mday

  out <- (12L * (ey - sy)) + (em - sm)

  l2r <- y >= x
  pos <- cheapr::val_find(l2r, TRUE)
  neg <- cheapr::val_find(l2r, FALSE)

  # Adjust for full months & time of day
  up <- time_add(x, new_timespan("months", out))

  if (length(pos) > 0){
    out[pos] <- out[pos] - (up > y)[pos]
  }
  if (length(neg) > 0){
    out[neg] <- out[neg] + (up < y)[neg]
  }

  out <- as.integer(divide(out, cheapr::val_replace(n, 0, NA)))

  if (fractional){

    temp <- out * n

    small_int_start <- C_time_add(
      x, list(month = temp),
      "preday", c("NA", "pre")
    )
    if (length(n) != 1){
      n <- rep_len2(n, length(out))
    }
    big_int_end <- C_time_add(
      x, list(month = temp + n), "preday", c("NA", "pre")
    )
    if (length(neg) > 0L){
      big_int_end[neg] <- C_time_add(
        x[neg], list(month = (temp - n)[neg]),
        "preday", c("NA", "pre")
      )
    }
    fraction <- strip_attrs(
      (unclass(y) - unclass(small_int_start)) /
        abs(unclass(big_int_end) - unclass(small_int_start))
    )
    fraction[start == end] <- 0

    if (!all_val(fraction, 0)){
      out <- out + fraction
    }
  }
  out
}

diff_days <- function(x, y, n = 1L, ...){
  set_time_cast(x, y)
  UseMethod("diff_days", x)
}
#' @export
diff_days.default <- function(x, y, n = 1L, fractional = FALSE, ...){
  x <- as.POSIXct(x)
  y <- as.POSIXct(y)
  diff_days(x, y, fractional = fractional, n = n, ...)
}
#' @export
diff_days.Date <- function(x, y, n = 1L, ...){
  divide(strip_attrs(unclass(as.Date(y)) - unclass(x)), n)
}

#' @export
diff_days.POSIXct <- function(x, y, n = 1L, fractional = FALSE, ...){

  x <- as.POSIXct(x)
  y <- as.POSIXct(y)

  xlt <- as.POSIXlt(x)
  ylt <- as.POSIXlt(y)

  # sseconds <- clock_seconds(xlt)
  # eseconds <- clock_seconds(ylt)

  # sseconds <- strip_attrs(unclass(x) - unclass(timechange::time_floor(x, "day")))
  # eseconds <- strip_attrs(unclass(y) - unclass(timechange::time_floor(y, "day")))

  sdate <- lubridate::make_date(xlt$year + 1900L, xlt$mon + 1L, xlt$mday)
  edate <- lubridate::make_date(ylt$year + 1900L, ylt$mon + 1L, ylt$mday)
  storage.mode(sdate) <- "integer"
  storage.mode(edate) <- "integer"

  out <- diff_days(sdate, edate)

  l2r <- y >= x
  pos <- cheapr::val_find(l2r, TRUE)
  neg <- cheapr::val_find(l2r, FALSE)

  # Adjust for time of day
  up <- time_add(x, new_timespan("days", out))

  if (length(pos) > 0){
    out[pos] <- out[pos] - (up > y)[pos]
  }
  if (length(neg) > 0){
    out[neg] <- out[neg] + (up < y)[neg]
  }
  out <- as.integer(divide(out, cheapr::val_replace(n, 0, NA)))

  if (fractional){
    int_end1 <- C_time_add(x, list(day = cheapr::val_replace(out * n, NaN, NA)), "preday", c("NA", "pre"))
    if (length(n) != 1){
      n <- rep_len2(n, length(out))
    }
    int_end2 <- C_time_add(int_end1, list(day = n), "preday", c("NA", "pre"))
    if (length(neg) > 0L){
      int_end2[neg] <- C_time_add(int_end1[neg], list(day = -scalar_if_else(length(n) == 1, n, n[neg])), "preday", c("NA", "pre"))
    }
    fraction <- strip_attrs(
      (unclass(y) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
    )
    out <- out + fraction
  }
  out
}

# Exact difference in clock time

period_diff <- function(x, y, timespan, fractional = TRUE){
  check_is_timespan(timespan)
  unit <- timespan_unit(timespan)
  num <- timespan_num(timespan)

  if (!is_whole_number(num)){
    cli::cli_abort("{.arg timespan} must be a {.cls timespan} of whole numbers")
  }

  set_time_cast(x, y)

  # Reduce to unique x-y pairs

  distinct_pairs <- FALSE

  if (length(x) == 0 || length(y) == 0 || length(num) == 0){
    interval_tbl <- cheapr::new_df(
      x = x, y = y, num = num,
      .recycle = TRUE
    )
  } else {
    interval_tbl <- cheapr::new_df(
      x = scalar_if_else(length(x) == 1, NULL, x),
      y = scalar_if_else(length(y) == 1, NULL, y),
      num = scalar_if_else(length(num) == 1, NULL, num),
      .recycle = TRUE
    )
    interval_groups <- group2(interval_tbl)
    starts <- attr(interval_groups, "starts")
    sizes <- attr(interval_groups, "group.sizes")
    n_groups <- attr(interval_groups, "N.groups")
    # If distinct pairs results in a 2x reduction in data size, then we do that
    distinct_pairs <- isTRUE((df_nrow(interval_tbl) %/% n_groups) >= 2L)
    if ( distinct_pairs ){
      interval_tbl <- cheapr::sset(interval_tbl, starts)
    }
  }
  x <- interval_tbl[["x"]] %||% x
  y <- interval_tbl[["y"]] %||% y
  num <- interval_tbl[["num"]] %||% num

  out <- switch(
    unit,
    years = {
      diff_months(x, y, fractional = fractional, n = num * 12L)
    },
    months = {
      diff_months(x, y, fractional = fractional, n = num)
    },
    weeks = {
      diff_days(x, y, fractional = fractional, n = num * 7L)
    },
    days = {
      diff_days(x, y, fractional = fractional, n = num)
    },
    rlang::arg_match0(unit, .period_units)
  )

  if (distinct_pairs){
    out <- out[interval_groups]
  }

  out

}
