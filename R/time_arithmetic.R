

# Period arithmetic -------------------------------------------------------

# Taken from timechange to be used in a tight period sequence loop
# All credits go to the authors of timechange
C_time_add <- get("C_time_add", asNamespace("timechange"), inherits = FALSE)

timespan_as_timechange_period <- function(x){
  `names<-`(list(timespan_num(x)), plural_unit_to_single(timespan_unit(x)))
}

# A subset tailored for timechange
maybe_scalar_sset <- function(x, i){
  if (length(x) == 1 && length(i) != 0){
    x
  } else {
    x[i]
  }
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
period_add.default <- function(x, add,
                               roll_month = getOption("timeplyr.roll_month", "xlast"),
                               roll_dst = getOption("timeplyr.roll_dst", c("NA", "xfirst")),
                               ...){
  x <- lubridate::as_datetime(x)

  if (length(add) == 0){
    return(x[0L])
  }

  if (timespan_unit(add) == "days" && lubridate::tz(x) == "UTC"){
    x + (timespan_num(add) * 86400)
  } else {
    if (roll_month == "xlast" || roll_month == "xfirst"){
      out <- x
      l2r <- add >= 0L
      if (length(l2r) < length(x)){
        l2r <- rep_len(l2r, length(x))
      }
      pos <- cheapr::val_find(l2r, TRUE)
      neg <- cheapr::val_find(l2r, FALSE)
      na <- cheapr::na_find(l2r)

      out[pos] <- C_time_add(
        maybe_scalar_sset(x, pos),
        timespan_as_timechange_period(maybe_scalar_sset(add, pos)),
        scalar_if_else(roll_month == "xlast", "postday", "preday"), roll_dst
      )
      out[neg] <- C_time_add(
        maybe_scalar_sset(x, neg),
        timespan_as_timechange_period(maybe_scalar_sset(add, neg)),
        scalar_if_else(roll_month == "xlast", "preday", "postday"), roll_dst
      )
      out[na] <- NA
      out

    } else {
      C_time_add(x, timespan_as_timechange_period(add), roll_month, roll_dst)
    }
  }
}
#' @export
period_add.Date <- function(x, add, roll_month = getOption("timeplyr.roll_month", "xlast"), ...){
  num <- timespan_num(add)
  unit <- timespan_unit(add)

  # The match number is important as it lines up with what the C++ code expects
  roll_choice <- match(roll_month, c("preday", "postday", "xlast", "xfirst", "NA"))

  if (is.na(roll_choice)){
    # Helpful error msg
    return(
      lubridate::as_date(
        period_add(lubridate::as_datetime(x), add, roll_month = roll_month, ...)
      )
    )
    # rlang::arg_match0(roll_month, c("preday", "postday", "NA"))
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
#'
#' @export
time_add <- function(x, timespan,
                     roll_month = getOption("timeplyr.roll_month", "xlast"),
                     roll_dst = getOption("timeplyr.roll_dst", c("NA", "xfirst"))){

  span <- timespan(timespan)
  num <- timespan_num(span)
  unit <- timespan_unit(span)

  if (!is.character(roll_month)){
    if (is.numeric(roll_month)){
      cli::cli_abort("
      A {.cls numeric} vector has been supplied
      to {.arg roll_month}, perhaps you meant
      `timespan({unit}, {deparse1(substitute(roll_month))})`}")
    } else {
      cli::cli_abort("{.arg roll_month} must be a length 1 character vector")
    }
  }
  if (!is.character(roll_dst)){
    cli::cli_abort("{.arg roll_dst} must be a length 1 or length 2 character vector")
  }

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
#' @rdname time_add
#' @export
time_subtract <- function(x, timespan,
                          roll_month = getOption("timeplyr.roll_month", "xlast"),
                          roll_dst = getOption("timeplyr.roll_dst", c("NA", "xfirst"))){
  time_add(x, -timespan(timespan), roll_month = roll_month, roll_dst = roll_dst)
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
# e.g. years are blocks of 12 months
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

  x <- data.table::as.IDate(x)
  y <- data.table::as.IDate(y)
  sy <- data.table::year(x)
  ey <- data.table::year(y)
  sm <- data.table::month(x)
  em <- data.table::month(y)
  smd <- data.table::mday(x)
  emd <- data.table::mday(y)

  out <- (12L * (ey - sy)) + (em - sm)

  l2r <- cheapr::na_replace(y >= x, TRUE)

  if (length(l2r) < length(n)){
    l2r <- rep_len(l2r, length(n))
  }

  out <- cheapr::cheapr_if_else(l2r, out - (emd < smd), out + (emd > smd))
  out <- as.integer(divide(out, cheapr::val_replace(n, 0, NA)))

  # Fractional through the month
  if (fractional){
    months_add <- new_timespan("months", out * n)

    small_int_start <- cpp_add_months(x, months_add, roll_month = 3L)

    if (identical(y, small_int_start)){
      return(out)
    }

    big_int_end <- cpp_add_months(
      x, (months_add + cheapr::cheapr_if_else(l2r, n, -n)),
      roll_month = 3L
    )
    fraction <- strip_attrs(
      (unclass(y) - unclass(small_int_start)) /
        abs(unclass(big_int_end) - unclass(small_int_start))
    )
    fraction[cheapr::which_(x == y)] <- 0
    if (!all_val(fraction, 0)){
      out <- out + fraction
    }
  }
  out
}
#' @export
diff_months.POSIXt <- function(x, y, n = 1L, fractional = FALSE, ...){

  x <- as.POSIXct(x)
  y <- as.POSIXct(y)

  xlt <- as.POSIXlt(x)
  ylt <- as.POSIXlt(y)

  sy <- xlt$year
  ey <- ylt$year
  sm <- xlt$mon
  em <- ylt$mon
  smd <- xlt$mday
  emd <- ylt$mday

  out <- (12L * (ey - sy)) + (em - sm)

  l2r <- cheapr::na_replace(y >= x, TRUE)
  if (length(l2r) < length(n)){
    l2r <- rep_len(l2r, length(n))
  }

  # Adjust for full months & time of day
  up <- time_add(x, new_timespan("months", out))

  out <- cheapr::cheapr_if_else(
    l2r,
    out - (up > y),
    out + (up < y)
  )
  out <- as.integer(divide(out, cheapr::val_replace(n, 0, NA)))

  if (fractional){
    temp <- out * n

    small_int_start <- cheapr::cheapr_if_else(
      l2r,
      C_time_add(
        x, list(month = temp),
        "postday", c("NA", "xfirst")
      ),
      C_time_add(
        x, list(month = temp),
        "preday", c("NA", "xfirst")
      )
    )

    if (identical(y, small_int_start)){
      return(out)
    }

    if (length(n) != 1){
      n <- rep_len2(n, length(out))
    }
    big_int_end <- cheapr::cheapr_if_else(
      l2r,
      C_time_add(
        x, list(month = (temp + n)),
        "postday", c("NA", "xfirst")
      ),
      C_time_add(
        x, list(month = (temp - n)),
        "preday", c("NA", "xfirst")
      )
    )
    fraction <- strip_attrs(
      (unclass(y) - unclass(small_int_start)) /
        abs(unclass(big_int_end) - unclass(small_int_start))
    )
    fraction[cheapr::which_(x == y)] <- 0

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
  out <- divide(unclass(as.Date(y)) - unclass(x), n)
  if (is_whole_number(out)){
    out <- as.integer(out)
  }
  out
}

#' @export
diff_days.POSIXt <- function(x, y, n = 1L, fractional = FALSE, ...){

  x <- as.POSIXct(x)
  y <- as.POSIXct(y)

  xlt <- as.POSIXlt(x)
  ylt <- as.POSIXlt(y)

  sdate <- lubridate::make_date(
    cheapr::set_add(xlt$year, 1900L),
    cheapr::set_add(xlt$mon, 1L),
    xlt$mday
  )
  edate <- lubridate::make_date(
    cheapr::set_add(ylt$year, 1900L),
    cheapr::set_add(ylt$mon, 1L),
    ylt$mday
  )
  storage.mode(sdate) <- "integer"
  storage.mode(edate) <- "integer"

  out <- diff_days(sdate, edate)

  l2r <- cheapr::na_replace(y >= x, TRUE)

  if (length(l2r) < length(n)){
    l2r <- rep_len(l2r, length(n))
  }

  # Adjust for time of day
  up <- time_add(x, new_timespan("days", out))
  out <- cheapr::cheapr_if_else(l2r, out - (up > y), out + (up < y))
  out <- as.integer(divide(out, cheapr::val_replace(n, 0, NA)))

  if (fractional){
    temp <- new_timespan("days", out * n)
    small_int_start <- time_add(x, temp)

    if (identical(y, small_int_start)){
      return(out)
    }

    big_int_end <- time_add(x, temp + cheapr::cheapr_if_else(l2r, n, -n))
    fraction <- strip_attrs(
      (unclass(y) - unclass(small_int_start)) / abs(unclass(big_int_end) - unclass(small_int_start))
    )
    fraction[cheapr::which_(x == y)] <- 0
    if (!all_val(fraction, 0)){
      out <- out + fraction
    }
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
