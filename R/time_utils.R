# Time utility functions
# Error for the user to see available units
unit_match_stop <- function(units = .time_units){
  stop(paste0("'arg' should be one of '", paste(units, collapse = "', '"), "'"))
}
# The idea is that when you supply a list, no regex
# is needed to separate out the numbers
# This is handy with loops to reduce overhead
unit_list_match <- function(l){
  if (names(l) == "numeric") {
    unit <- "numeric"
  } else {
    unit <- rlang::arg_match0(names(l), .time_units)
  }
  if (is.na(unit)) unit_match_stop()

  num <- .subset2(l, 1L)
  scale <- 1L
  if (unit %in% .extra_time_units){
    exotic_info <- convert_exotic_units(unit)
    scale <- .subset2(exotic_info, "scale")
    unit <- .subset2(exotic_info, "unit")
  }
  list("unit" = unit, "num" = num * scale)
}
# Convert exotic units to normal units
convert_exotic_units <- function(x){
  scales <- c(2, 3, 18, 4, 5, 10, 15, 20, 100, 1000)
  base_units <- c("weeks", "months", "weeks",
                  rep_len("years", 7L))
  match_i <- match(x, .extra_time_units,
                   nomatch = NA_integer_)
  list("unit" = base_units[match_i],
       "scale" = scales[match_i])
}
# Partial unit matching
unit_match <- function(x){
  if (length(x) != 1L) stop("x must be of length 1.")
  units <- .time_units
  match_i <- pmatch(x, units,
                    nomatch = NA_character_,
                    duplicates.ok = FALSE)
  .subset(.time_units, match_i)
}
# Unit string parsing
unit_parse <- function(x){
  # Extract numbers from string
  # Try decimal numbers
  num_str <- regmatches(x, m = regexpr(pattern = ".*[[:digit:]]*\\.[[:digit:]]+",
                                       text = x))
  # If not try regular numbers
  if (length(num_str) == 0L){
    num_str <- regmatches(x, m = regexpr(pattern = ".*[[:digit:]]+",
                                         text = x))
  }
  num <- as.numeric(num_str)
  if (length(num) == 0L){
    num <- 1L
  }
  if (is_whole_number(num) && is_integerable(num)){
    num <- as.integer(num)
  }
  scale <- 1L
  if (length(num_str) > 0L){
    x <- sub(num_str, "", x, fixed = TRUE) # Remove numbers
  }
  x <- gsub(" ", "", x, fixed = TRUE) # Remove whitespace
  unit <- unit_match(x)
  if (is.na(unit)) unit_match_stop()
  if (unit %in% .extra_time_units){
    exotic_info <- convert_exotic_units(unit)
    scale <- .subset2(exotic_info, "scale")
    unit <- .subset2(exotic_info, "unit")
  }
  list("unit" = unit, "num" = num * scale)
}
get_time_granularity <- function(x, timespan = NULL, quiet = FALSE){
  if (is.null(timespan)){
    time_granularity(x)
  } else {
    timespan(timespan)
  }
}
time_by_pretty <- function(time_by, sep = " "){
  span <- timespan(time_by)
  units <- timespan_unit(span)
  num <- timespan_num(span)
  if (length(num) > 1){
    stop("Please supply only one numeric value in `time_by`")
  }
  if (!timespan_has_unit(span)){
    if (isTRUE(num == 1)){
      paste(num, "numeric unit", sep = " ")
    } else {
      pretty_num <- round(num, 2)
      if (isTRUE(!cppdoubles::double_equal(num, pretty_num))){
        pretty_num <- paste0("~", pretty_num)
      }
      paste(pretty_num, "numeric units", sep = " ")
    }
  } else {
    num_seconds <- unit_to_seconds(span)
    higher_unit_info <- seconds_to_unit(num_seconds)
    scale <- higher_unit_info$scale
    higher_unit <- higher_unit_info$unit
    num <- num_seconds / scale
    units <- higher_unit

    pretty_num <- round(num, 2)
    if (isTRUE(!cppdoubles::double_equal(num, pretty_num))){
      pretty_num <- paste0("~", pretty_num)
    }
    if (isTRUE(num == 1)){
      paste0(plural_unit_to_single(units))
    } else {
      paste0(pretty_num, sep, units)
    }
  }
}
gcd_time_diff <- function(x){
  if (length(x) <= 1){
    out <- 1L
  } else {
    out <- abs(gcd_diff(x))
  }
  cheapr::na_replace(out, 1L)
}

# Converts seconds to duration unit
# Scale is in comparison to seconds
seconds_to_unit <- function(x){
  if (length(x) == 0L){
    return(
      list(
        unit = "seconds",
        scale = numeric()
      )
    )
  }
  if (length(x) == 1 && is.na(x)){
    return(
      list(
        unit = "seconds",
        scale = NA_real_
      )
    )
  }
  x <- abs(x)
  if (x == 0){
    unit <- "seconds"
    scale <- 1
  } else if (x > 0 && x < 1/1000/1000/1000){
    unit <- "picoseconds"
    scale <- 1/1000/1000/1000/1000
  } else if (x >= 1/1000/1000/1000 && x < 1/1000/1000){
    unit <- "nanoseconds"
    scale <- 1/1000/1000/1000
  } else if (x >= 1/1000/1000 && x < 1/1000){
    unit <- "microseconds"
    scale <- 1/1000/1000
  } else if (x >= 1/1000 && x < 1){
    unit <- "milliseconds"
    scale <- 1/1000
  }  else if (x >= 1 && x < 60){
    unit <- "seconds"
    scale <- 1
  } else if (x >= 60 && x < 3600){
    unit <- "minutes"
    scale <- 60
  } else if (x >= 3600 && x < 86400){
    unit <- "hours"
    scale <- 3600
  } else if (x >= 86400 && x < 604800){
    unit <- "days"
    scale <- 86400
  } else if (x >= 604800 && x < 2629800){
    unit <- "weeks"
    scale <- 604800
  } else if (x >= 2629800 && x < 31557600){
    unit <- "months"
    scale <- 2629800
  } else if (x >= 31557600){
    unit <- "years"
    scale <- 31557600
  }
  list("unit" = unit,
       "scale" = scale)
}
unit_to_seconds <- function(x){
  unit_info <- timespan(x)
  unit <- unit_info[["unit"]]
  num <- unit_info[["num"]]
  scales <- c(1/1000/1000/1000/1000, # Pico
              1/1000/1000/1000, # Nano
              1/1000/1000, # Micro
              1/1000, # Milli
              1, # Second
              60, # Hour
              3600, # Minute
              86400, # Day
              604800, # Week
              2629800, # Month
              31557600) # Year
  unit_match <- match(unit, .duration_units)
  if (is.na(unit_match)){
    unit_match_stop(.duration_units)
  }
  num * scales[unit_match]
}

convert_common_dates <- function(x){
  if (is_time(x)){
    out <- x
  } else if (is.character(x)){
    which_na <- cheapr::na_find(x)
    out <- lubridate::ymd(x, quiet = TRUE)
    num_na <- cheapr::na_count(out)
    if (num_na > length(which_na)){
      out <- lubridate::dmy(x, quiet = TRUE)
    }
    num_na <- cheapr::na_count(out)
    if (num_na > length(which_na)){
      out <- lubridate::Date(length(x))
    }
  } else {
    out <- lubridate::Date(length(x))
  }
  out
}
# Calculate size of period unit to expand from and to for specified length
period_by_calc <- function(from, to, length){
  seconds_unit <- period_unit("seconds")
  set_recycle_args(from, to, length)
  which_len_1 <- cheapr::val_find(length, 1)
  sec_diff <- time_diff(from, to,
                        time_by = list("seconds" = 1),
                        time_type = "period")
  out <- lubridate::seconds_to_period(sec_diff / (length - 1))
  period_info <- collapse::qDF(time_unit_info(out))
  n_unique_slots <- df_ncol(period_info) - rowSums(period_info == 0)
  which_multi <- which(n_unique_slots > 1)
  out[which_multi] <- seconds_unit(
    lubridate::period_to_seconds(out[which_multi])
  )
  out[which_len_1] <- seconds_unit(0)
  out
}
# Calculates size of duration to cut a pre-specified interval of
# a certain length
duration_by_calc <- function(from, to, length){
  seconds_unit <- duration_unit("seconds")
  sec_diff <- time_diff(from, to,
                        time_by = list("seconds" = 1),
                        time_type = "duration")
  out <- seconds_unit(sec_diff / (length - 1))
  length <- rep_len(length, length(out))
  out[cheapr::val_find(length, 1)] <- seconds_unit(0) # Special case
  out
}
num_by_calc <- function(from, to, length){
  out <- (to - from) / (length - 1)
  length <- rep_len(length, length(out))
  out[cheapr::val_find(length, 1)] <- 0
  out
}
# Vectorized except for periods
time_by_calc <- function(from, to, length){
  if (is_time(from) && is_time(to)){
    # if (time_type == "period"){
      period_by_calc(from, to, length)
    # } else {
    #   duration_by_calc(from, to, length)
    # }
  } else {
    num_by_calc(from, to, length)
  }
}
# This only works for single unit vectors
# Periods with multiple types of units do not work.
time_unit_info <- function(time_unit){
  tclass <- class(time_unit)
  time_value <- unclass(time_unit)
  if (tclass == "Duration"){
    list("second" = time_value)
  } else if (tclass == "Period"){
    out <- attributes(time_value)
    seconds <- lubridate::second(time_unit)
    out[["second"]] <- seconds
    sum_rng <- lapply(out, function(x) sum(abs(collapse::frange(x, na.rm = TRUE))))
    keep <- vapply(sum_rng, function(x) isTRUE(cppdoubles::double_gt(x, 0)), FALSE)
    if (sum(keep) == 0){
      out["second"]
    } else {
      out[keep]
    }
  } else {
    list("numeric" = time_value)
  }
}
# Functional that returns lubridate duration function
duration_unit <- function(units = "seconds"){
  if (!units %in% .duration_units) unit_match_stop(.duration_units)
  switch(units,
         picoseconds = lubridate::dpicoseconds,
         nanoseconds = lubridate::dnanoseconds,
         microseconds = lubridate::dmicroseconds,
         milliseconds = lubridate::dmilliseconds,
         seconds = lubridate::dseconds,
         minutes = lubridate::dminutes,
         hours = lubridate::dhours,
         days = lubridate::ddays,
         weeks = lubridate::dweeks,
         months = lubridate::dmonths,
         years = lubridate::dyears)
}

numeric_unit <- function(units){
  identity
}
# Functional that returns time unit function
time_unit <- function(units){
  if (is.na(units)){
    numeric_unit(units)
  } else if (is_duration_unit(units)){
    duration_unit(units)
  } else {
    period_unit(units)
  }
}

# Faster time_cast
# numeric > yearqtr > yearmon > date > datetime
# You can move from left to right but not right to left
time_cast <- function(x, template){
  if (inherits(template, "POSIXt")){
    tzone <- lubridate::tz(template)
    if (inherits(x, "POSIXt")){
      if (identical(tzone, lubridate::tz(x))){
        x
      } else {
        lubridate::with_tz(x, tzone = tzone)
      }
    } else if (inherits(x, "Date")){
      lubridate::with_tz(x, tzone = tzone)
    } else {
      as.POSIXct(x, tz = tzone, origin = lubridate::origin)
    }
  } else if (inherits(template, "Date") &&
             !inherits(x, "POSIXt")){
    if (identical(class(x), "integer")){
      .Date(x)
    } else {
      lubridate::as_date(x)
    }
  } else if (inherits(template, "year_month") &&
           !inherits(x, c("POSIXt", "Date"))){
    year_month(x)
  } else if (inherits(template, "yearmon") &&
             !inherits(x, c("POSIXt", "Date"))){
    as_yearmon(x)
  } else if (inherits(template, "yearqtr") &&
             !inherits(x, c("POSIXt", "Date", "yearmon"))){
    as_yearqtr(x)
  } else if (inherits(template, "year_quarter") &&
             !inherits(x, c("POSIXt", "Date"))){
    year_quarter(x)
  } else {
    x
  }
}

# Coerce pair of time based vectors to the most informative
# class between them
set_time_cast <- function(x, y){
  if (identical(base::parent.frame(n = 1), base::globalenv())){
    stop("Users cannot use set_time_cast from the global environment")
  }
  if (!identical(class(x), class(y))){
    x_nm <- deparse2(substitute(x))
    y_nm <- deparse2(substitute(y))
    assign(x_nm, time_cast(x, y), envir = parent.frame(n = 1))
    assign(y_nm, time_cast(y, x), envir = parent.frame(n = 1))
  }
}
# Faster as_datetime
as_datetime2 <- function(x){
  if (is_datetime(x)){
    x
  } else {
    lubridate::as_datetime(x)
  }
}

cut_time <- function(x, breaks, include_oob = FALSE, codes = FALSE, include_lowest = FALSE){
  cheapr::bin(x, breaks, codes = codes, left_closed = TRUE,
              include_oob = include_oob,
              include_endpoint = include_lowest)
}

# Check for date sequences that should not be coerced to datetimes
is_special_case_days <- function(from, to, unit, num){
    unit %in% c("days", "weeks") &&
    is_date(from) &&
    is_date(to) &&
    is_whole_number(num)
}
# Repeat methods for zoo yearmon and yearqtr class
#' @exportS3Method base::rep_len
rep_len.yearmon <- function(x, length.out){
  x[rep_len(seq_along(x), length.out = length.out)]
}
#' @exportS3Method base::rep.int
rep.int.yearmon <- function(x, ...){
  x[rep.int(seq_along(x), ...)]
}
#' @exportS3Method base::rep
rep.yearmon <- function(x, ...){
  x[rep(seq_along(x), ...)]
}
#' @exportS3Method base::rep_len
rep_len.yearqtr <- function(x, length.out){
  x[rep_len(seq_along(x), length.out = length.out)]
}
#' @exportS3Method base::rep.int
rep.int.yearqtr <- function(x, ...){
  x[rep.int(seq_along(x), ...)]
}
#' @exportS3Method base::rep
rep.yearqtr <- function(x, ...){
  x[rep(seq_along(x), ...)]
}
# Coerce to yearmon
# Safer for datetimes as it doesn't coerce to a specific timezone
as_yearmon <- function(x){
  if (inherits(x, "yearmon")){
    x
  } else {
    if (is_time(x)){
      x <- lubridate::year(x) + ( (lubridate::month(x) - 1) / 12 )
    }
    structure(floor(12 * as.double(x) + 1e-04)/12, class = "yearmon")
  }
}
# Coerce to yearqtr
as_yearqtr <- function(x){
  if (inherits(x, "yearqtr")){
    x
  } else {
    if (is_time(x)){
      x <- lubridate::year(x) + ( (lubridate::month(x) - 1) / 12 )
    }
    structure(floor(4 * x + 0.001)/4, class = "yearqtr")
  }
}
is_interval <- function(x){
  (isS4(x) && inherits(x, "Interval")) ||
    inherits(x, "time_interval")
}

# Internal helper to process from/to args
get_from_to <- function(data, ..., time, from = NULL, to = NULL,
                        .by = NULL){
  from_var <- col_select_names(data, .cols = from)
  to_var <- col_select_names(data, .cols = to)
  time_var <- col_select_names(data, .cols = time)
  dot_vars <- tidy_select_names(data, ...)
  by_vars <- tidy_select_names(data, {{ .by }})
  if (length(from_var) == 0L || length(to_var) == 0L){
    g <- df_to_GRP(data, .cols = c(by_vars, dot_vars))
  }
  if (length(from_var) == 0L){
    .from <- gmin(data[[time_var]], g = g)
  } else {
    .from <- time_cast(data[[from_var]], data[[time_var]])
  }
  if (length(to_var) == 0L){
    .to <- gmax(data[[time_var]], g = g)
  } else {
    .to <- time_cast(data[[to_var]], data[[time_var]])
  }
  list(.from = .from,
       .to = .to)
}
# Taken from timechange to be used in a tight period sequence loop
# All credits go to the authors of timechange
C_time_add <- get("C_time_add", asNamespace("timechange"), inherits = FALSE)

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
      x + duration_unit(unit)(num)
    } else {
      unit <- plural_unit_to_single(unit)
      timechange::time_add(x, periods = add_names(list(num), unit),
                           roll_month = roll_month, roll_dst = roll_dst)
    }
  }
}
time_floor <- function(x, time_by, week_start = getOption("lubridate.week.start", 1)){
  span <- timespan(time_by)
  num <- timespan_num(span)
  unit <- timespan_unit(span)

  if (is_time(x)){
    time_by <- paste(num, unit)
    timechange::time_floor(x, unit = time_by, week_start = week_start)
  } else {
    floor(x / num) * num
  }
}
# Custom time flooring..
time_floor2 <- function(x, time_by, week_start = getOption("lubridate.week.start", 1)){
  if (time_by_is_num(time_by)){
    num <- unlist(time_by, use.names = FALSE, recursive = FALSE)
    floor(x / num) * num
  } else {
    time_floor(x, time_by = add_names(list(1), names(time_by)), week_start = week_start)
  }
}
time_ceiling <- function(x, time_by, week_start = getOption("lubridate.week.start", 1),
                         change_on_boundary = inherits(x, "Date")){
  unit_info <- timespan(time_by)
  by_unit <- unit_info[["unit"]]
  by_n <- unit_info[["num"]] * unit_info[["scale"]]
  if (is_time(x)){
    time_by <- paste(by_n, by_unit)
    timechange::time_ceiling(x, unit = time_by, week_start = week_start)
  } else {
    ceiling(x / by_n) * by_n
  }
}
# Custom time flooring..
time_ceiling2 <- function(x, time_by, week_start = getOption("lubridate.week.start", 1),
                          change_on_boundary = FALSE){
  if (time_by_is_num(time_by)){
    num <- unlist(time_by, use.names = FALSE, recursive = FALSE)
    ceiling(x / num) * num
  } else {
    time_ceiling(x, time_by = add_names(list(1), names(time_by)),
                 week_start = week_start,
                 change_on_boundary = change_on_boundary)
  }
}
tomorrow <- function(){
  as_int_date(Sys.Date()) + 1L
}
# Unique posix vector to character remains unique
time_as_character <- function(x){
  if (is_datetime(x)){
    format(x, usetz = TRUE)
  } else {
    as.character(x)
  }
}
time_int_rm_attrs <- function(x){
  attr(x, "end") <- NULL
  attr(x, "direction") <- NULL
  x
}

check_is_date <- function(x){
  if (!is_date(x)){
    stop(paste(deparse2(substitute(x)),
               "must be a date"))
  }
}
check_is_datetime <- function(x){
  if (!is_datetime(x)){
    stop(paste(deparse2(substitute(x)),
               "must be a datetime"))
  }
}
check_is_time <- function(x){
  if (!is_time(x)){
    stop(paste(deparse2(substitute(x)),
                "must be a date or datetime"))
  }
}
check_is_time_or_num <- function(x){
  if (!is_time_or_num(x)){
    stop(paste(deparse2(substitute(x)),
               "must be a date, datetime, or numeric vector"))
  }
}
# Turn date storage into integer
as_int_date <- function(x){
  check_is_date(x)
  out <- as.integer(x)
  class(out) <- "Date"
  out
}
check_time_not_missing <- function(x){
  if (cheapr::any_na(x)){
    stop("time index must not contain NA values")
  }
}
# Turn "days" into "day", etc
plural_unit_to_single <- function(x){
  substr(x, 1L, nchar(x) -1L)
}

# Multiplies a single unit period like days(7) or months(2)
multiply_single_unit_period_by_number <- function(per, num){
  per_list <- timespan(per)
  per_list <- time_by_list_convert_weeks_to_days(per_list)
  per_num <- timespan_num(per_list)
  per_unit <- timespan_unit(per_list)
  # per_unit <- plural_unit_to_single(per_unit)
  # if (per_unit == "second"){
  #   per_unit <- ".Data"
  # }
  # recycle <- length(per_num) != length(num)
  # TEMPORARY infinite replacement
  num[is.infinite(num)] <- NA_real_
  per_num <- per_num * num
  per_length <- length(per_num)
  per_num[which(is.nan(per_num))] <- NA_real_
  other_fill <- integer(per_length)
  other_fill[which_na(per_num)] <- NA_integer_
  switch(
    per_unit,
    years = {
      per@year <- per_num
       per@month <- other_fill
       per@day <- other_fill
       per@hour <- other_fill
       per@minute <- other_fill
       per@.Data <- other_fill
    },
    months = {
      per@month <- per_num
        per@year <- other_fill
        per@day <- other_fill
        per@hour <- other_fill
        per@minute <- other_fill
        per@.Data <- other_fill
    },
    days = {
      per@day <- per_num
        per@year <- other_fill
        per@month <- other_fill
        per@hour <- other_fill
        per@minute <- other_fill
        per@.Data <- other_fill
    },
    hours = {
      per@hour <- per_num
        per@year <- other_fill
        per@month <- other_fill
        per@day <- other_fill
        per@minute <- other_fill
        per@.Data <- other_fill
    },
    minutes = {
      per@minute <- per_num
        per@year <- other_fill
        per@month <- other_fill
        per@day <- other_fill
        per@hour <- other_fill
        per@.Data <- other_fill
    },
    seconds = {
      per@.Data <- per_num
        per@year <- other_fill
        per@month <- other_fill
        per@day <- other_fill
        per@hour <- other_fill
        per@minute <- other_fill
    }
  )
  per
}

rep_single_unit_period <- function(per, ...){
  per_list <- timespan(per)
  per_list <- time_by_list_convert_weeks_to_days(per_list)
  per_num <- timespan_num(per_list)
  per_unit <- timespan_unit(per_list)
  per_num <- rep(per_num, ...)
  other_fill <- integer(length(per_num))
  switch(
    per_unit,
    years = {
      per@year <- per_num
      per@month <- other_fill
      per@day <- other_fill
      per@hour <- other_fill
      per@minute <- other_fill
      per@.Data <- other_fill
    },
    months = {
      per@month <- per_num
      per@year <- other_fill
      per@day <- other_fill
      per@hour <- other_fill
      per@minute <- other_fill
      per@.Data <- other_fill
    },
    days = {
      per@day <- per_num
      per@year <- other_fill
      per@month <- other_fill
      per@hour <- other_fill
      per@minute <- other_fill
      per@.Data <- other_fill
    },
    hours = {
      per@hour <- per_num
      per@year <- other_fill
      per@month <- other_fill
      per@day <- other_fill
      per@minute <- other_fill
      per@.Data <- other_fill
    },
    minutes = {
      per@minute <- per_num
      per@year <- other_fill
      per@month <- other_fill
      per@day <- other_fill
      per@hour <- other_fill
      per@.Data <- other_fill
    },
    seconds = {
      per@.Data <- per_num
      per@year <- other_fill
      per@month <- other_fill
      per@day <- other_fill
      per@hour <- other_fill
      per@minute <- other_fill
    }
  )
  per
}

### Taken from lubridate ###

# Accepts an estimate ala (interval / duration)
# Start datetime, end datetime, and period object
adj_dur_est <- function (est, start, end, per){
  est <- ceiling(est)
  up_date <- time_add(start,
                       # est * per)
                       multiply_single_unit_period_by_number(per, est),
                       ### NOT SURE ABOUT THE BELOW roll_dst LINE
                       roll_dst = "NA")
  # up_date2 <- up_date
  while (length(which <- which(up_date < end))) {
    # up_date2 <- up_date
    est[which] <- est[which] + 1
    up_date[which] <- timechange::time_add(start[which],

    # As as 20-March-2024 lubridate uses the below line
    # Which I think is likely wrong and a typo...
    # It was causing incorrect hourly time differences
    # Between 2 date-times after DST

    # up_date[which] <- time_add(up_date[which],
                                period_to_list(
                                  multiply_single_unit_period_by_number(per[which], est[which])
                                  ))
  }
  low_date <- up_date
  while (length(which <- which(low_date > end))) {
    est[which] <- est[which] - 1
    up_date[which] <- low_date[which]
    low_date[which] <- timechange::time_add(start[which],
                                 period_to_list(
                                   multiply_single_unit_period_by_number(per[which], est[which])
                                   ))
  }
  frac <- strip_attrs(difftime(end, low_date, units = "secs")) /
    strip_attrs(difftime(up_date, low_date, units = "secs"))
  frac[which(low_date == up_date)] <- 0
  est + frac
}
# Faster method for interval(start, end) / period() when period
# is a single unit period which is very common
divide_interval_by_period2 <- function(start, end, per){
  if (length(start) == 0 || length(end) == 0 || length(per) == 0) {
    return(numeric())
  }
  estimate <- (strip_attrs(as_datetime2(end)) -
                 strip_attrs(as_datetime2(start)) ) / unit_to_seconds(per)
  max_len <- max(length(start), length(end), length(per))
  timespans <- cheapr::recycle(start = start, end = end, length = max_len)
  # Here we make sure to use rep method for lubridate periods
  timespans[[3]] <- rep_single_unit_period(per, length.out = max_len)
  if (cheapr::na_count(estimate) == 0) {
    adj_dur_est(estimate, timespans[[1]], timespans[[2]], timespans[[3]])
  } else {
    not_nas <- which_not_na(estimate)
    start2 <- timespans[[1]][not_nas]
    end2 <- timespans[[2]][not_nas]
    per2 <- timespans[[3]][not_nas]
    estimate[not_nas] <- adj_dur_est(estimate[not_nas], start2, end2, per2)
    estimate
  }
}
time_by_list_convert_weeks_to_days <- function(time_by){
  out <- time_by
  if (timespan_unit(out) == "weeks"){
    out <- list("days" = as.double(timespan_num(out) * 7))
  }
  out
}
# time_by_as_timechange_period_list <- function(time_by){
#   time_by <- time_by_list(time_by)
#   num <- timespan_num(time_by)
#   unit <- plural_unit_to_single(timespan_unit(time_by))
#   add_names(list(num), unit)
# }
period_to_list <- function(x){
  list(year = attr(x, "year"),
       month = attr(x, "month"),
       day = attr(x, "day"),
       hour = attr(x, "hour"),
       minute = attr(x, "minute"),
       second = x@.Data)
}

days_in_month <- function (m, y) {
  N_DAYS_IN_MONTHS <- c(Jan = 31L,
                        Feb = 28L,
                        Mar = 31L,
                        Apr = 30L,
                        May = 31L,
                        Jun = 30L,
                        Jul = 31L,
                        Aug = 31L,
                        Sep = 30L,
                        Oct = 31L,
                        Nov = 30L,
                        Dec = 31L
  )
  n_days <- N_DAYS_IN_MONTHS[m]
  n_days[which(m == 2L & lubridate::leap_year(y))] <- 29L
  n_days
}
int_to_per <- function (start, end){
  set_recycle_args(start, end)
  start <- as_datetime2(start)
  end <- time_cast(end, start)
  duration <- strip_attrs(end) - strip_attrs(start)
  start <- strip_attrs(as.POSIXlt(start))
  end <- strip_attrs(as.POSIXlt(end))
  negs <- duration < 0
  wnegs <- which(negs)
  wnnegs <- which(negs, invert = TRUE)
  per <- list()
  for (nm in c("sec", "min", "hour", "mday", "mon", "year")) {
    per[[nm]] <- integer(length(negs))
    per[[nm]][wnegs] <- (start[[nm]] - end[[nm]])[wnegs]
    per[[nm]][wnnegs] <- (end[[nm]] - start[[nm]])[wnnegs]
  }
  names(per) <- c("second", "minute", "hour", "day", "month",
                  "year")
  nsecs <- per$second < 0
  wnsecs <- which(nsecs)
  per$second[wnsecs] <- 60 + per$second[wnsecs]
  per$minute[wnsecs] <- per$minute[wnsecs] - 1
  per$second[wnegs] <- -per$second[wnegs]
  nmins <- per$minute < 0
  wnmins <- which(nmins)
  per$minute[wnmins] <- 60 + per$minute[wnmins]
  per$hour[wnmins] <- per$hour[wnmins] - 1
  per$minute[wnegs] <- -per$minute[wnegs]
  nhous <- per$hour < 0
  wnhous <- which(nhous)
  per$hour[wnhous] <- 24 + per$hour[wnhous]
  per$hour[wnegs] <- -per$hour[wnegs]
  ndays <- !negs & per$day < 0
  wndays <- which(ndays)
  if (length(wndays) > 0) {
    add_months <- rep.int(-1, sum(ndays, na.rm = TRUE))
    pmonth <- end$mon[wndays]
    pmonth[which(pmonth == 0)] <- 1
    prev_month_days <- days_in_month(pmonth, end$year[wndays])
    per$day[wndays] <- pmax(prev_month_days - start$mday[wndays],
                            0) + end$mday[wndays]
    per$month[wndays] <- per$month[wndays] + add_months
  }
  ndays <- negs & per$day < 0
  wndays <- which(ndays)
  if (length(wndays) > 0) {
    add_months <- rep.int(1L, sum(ndays, na.rm = TRUE))
    this_month_days <- days_in_month(end$mon[wndays] + 1,
                                     end$year[wndays])
    per$day[wndays] <- pmax(this_month_days - end$mday[wndays],
                            0) + start$mday[wndays]
    per$month[wndays] <- per$month[wndays] - add_months
  }
  per$day[wnhous] <- per$day[wnhous] - 1
  per$day[wnegs] <- -per$day[wnegs]
  nmons <- per$month < 0
  wnmons <- which(nmons)
  per$month[wnmons] <- 12 + per$month[wnmons]
  per$year[wnmons] <- per$year[wnmons] - 1
  per$month[wnegs] <- -per$month[wnegs]
  per$year[wnegs] <- -per$year[wnegs]
  per
}

# Convenience function to return base time unit of time variable
# Useless now, use `time_resolution()`
get_time_unit <- function(x){
  if (is_date(x)){
    "days"
  } else if (is_datetime(x)){
   "seconds"
  } else {
    "numeric"
  }
}

is_duration_unit <- function(x){
  collapse::fmatch(x, .time_units) <= 7L
}
