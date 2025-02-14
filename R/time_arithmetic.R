

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
period_add.Date <- function(x, add, ...){
  num <- timespan_num(add)
  unit <- timespan_unit(add)

  switch(
    unit,
    years = {
      cpp_add_months(x, num * 12L)
    },
    months = {
      cpp_add_months(x, num)
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
      x + unit_to_seconds(span)
    } else {
      period_add(x, span, roll_month = roll_month, roll_dst = roll_dst)
    }
  }
}
time_subtract <- function(x, timespan,
                          roll_month = getOption("timeplyr.roll_month", "preday"),
                          roll_dst = getOption("timeplyr.roll_dst", "NA")){
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

diff_months <- function(x, y, fractional = FALSE, ...){
  set_time_cast(x, y)
  UseMethod("diff_months")
}
#' @export
diff_months.default <- function(x, y, fractional = FALSE, ...){

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

  sign <- cheapr::int_sign(out)

  # Adjust for full months
  pos <- cheapr::val_find(sign, 1L)
  neg <- cheapr::val_find(sign, -1L)
  out[pos] <- out[pos] - (emd < smd)[pos]
  out[neg] <- out[neg] + (emd > smd)[neg]

  # Fractional through the month
  if (fractional){
    zero <- cheapr::val_find(sign, 0L)
    start_dt <- as.POSIXct(x)
    end_dt <- as.POSIXct(y)

    int_end1 <- C_time_add(start_dt, list(month = out), "preday", "NA")
    int_end2 <- int_end1 # Initialise
    int_end2[pos] <- C_time_add(int_end1[pos], list(month = 1), "preday", "NA")
    int_end2[neg] <- C_time_add(int_end1[neg], list(month = -1), "preday", "NA")
    int_end2[zero] <- C_time_add(int_end1[zero], list(month = 1), "preday", "NA")
    fraction <- strip_attrs(
      (unclass(end_dt) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
    )
    out <- out + fraction
  }
  out
}

#' @export
diff_months.Date <- function(x, y, fractional = FALSE, ...){

  start <- data.table::as.IDate(x)
  end <- data.table::as.IDate(y)
  sy <- data.table::year(start)
  sm <- data.table::month(start)
  ey <- data.table::year(end)
  em <- data.table::month(end)
  smd <- data.table::mday(start)
  emd <- data.table::mday(end)

  out <- (12L * (ey - sy)) + (em - sm)

  sign <- cheapr::int_sign(out)

  # Adjust for full months
  pos <- cheapr::val_find(sign, 1L)
  neg <- cheapr::val_find(sign, -1L)
  out[pos] <- out[pos] - (emd < smd)[pos]
  out[neg] <- out[neg] + (emd > smd)[neg]

  # Fractional through the month
  if (fractional){
    zero <- cheapr::val_find(sign, 0L)
    # start_dt <- as_datetime2(x)
    # end_dt <- as_datetime2(y)
    int_end1 <- cpp_add_months(x, new_timespan("months", out))

    # Initialise
    int_end2 <- int_end1
    months <- new_timespan("months", 1L)
    int_end2[pos] <- cpp_add_months(int_end1[pos], months)
    int_end2[neg] <- cpp_add_months(int_end1[neg], -months)
    int_end2[zero] <- cpp_add_months(int_end1[zero], months)
    fraction <- strip_attrs(
      (unclass(y) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
    )
    out <- out + fraction
  }
  out
}
#' @export
diff_months.POSIXct <- function(x, y, fractional = FALSE, ...){

  start <- unclass(as.POSIXlt(x))
  end <- unclass(as.POSIXlt(y))
  sy <- start[["year"]]
  sm <- start[["mon"]]
  ey <- end[["year"]]
  em <- end[["mon"]]
  smd <- start[["mday"]]
  emd <- end[["mday"]]

  out <- (12L * (ey - sy)) + (em - sm)

  sign <- cheapr::int_sign(out)

  # Adjust for full months
  pos <- cheapr::val_find(sign, 1L)
  neg <- cheapr::val_find(sign, -1L)
  out[pos] <- out[pos] - (emd < smd)[pos]
  out[neg] <- out[neg] + (emd > smd)[neg]

  # Fractional through the month
  if (fractional){
    zero <- cheapr::val_find(sign, 0L)
    int_end1 <- C_time_add(x, list(month = out), "preday", "NA")
    int_end2 <- int_end1 # Initialise
    int_end2[pos] <- C_time_add(int_end1[pos], list(month = 1), "preday", "NA")
    int_end2[neg] <- C_time_add(int_end1[neg], list(month = -1), "preday", "NA")
    int_end2[zero] <- C_time_add(int_end1[zero], list(month = 1), "preday", "NA")
    fraction <- strip_attrs(
      (unclass(y) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
    )
    out <- out + fraction
  }
  out
}



diff_years <- function(x, y, fractional = FALSE){

  start <- data.table::as.IDate(x)
  end <- data.table::as.IDate(y)
  sy <- data.table::year(start)
  ey <- data.table::year(end)
  syd <- data.table::yday(start)
  eyd <- data.table::yday(end)

  out <- ey - sy

  sign <- cheapr::int_sign(out)

  # Adjust for full months
  pos <- cheapr::val_find(sign, 1L)
  neg <- cheapr::val_find(sign, -1L)
  out[pos] <- out[pos] - (eyd < syd)[pos]
  out[neg] <- out[neg] + (eyd > syd)[neg]

  # Fractional through the year
  if (fractional){
    zero <- cheapr::val_find(sign, 0L)
    start_dt <- as_datetime2(x)
    end_dt <- as_datetime2(y)
    int_end1 <- C_time_add(start_dt, list(year = out), "preday", "NA")
    int_end2 <- int_end1 # Initialise
    int_end2[pos] <- C_time_add(int_end1[pos], list(year = 1), "preday", "NA")
    int_end2[neg] <- C_time_add(int_end1[neg], list(year = -1), "preday", "NA")
    int_end2[zero] <- C_time_add(int_end1[zero], list(year = 1), "preday", "NA")
    fraction <- strip_attrs(
      (unclass(end_dt) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
    )
    out <- out + fraction
  }

  out
}
