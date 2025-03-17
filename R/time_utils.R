# Time utility functions
# Error for the user to see available units
unit_match_stop <- function(units = .time_units){
  cli::cli_abort(paste0("'arg' should be one of '", paste(units, collapse = "', '"), "'"))
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
  check_length(x, 1)
  .time_units[
    pmatch(x, .time_units,
           nomatch = NA_character_,
           duplicates.ok = FALSE)
  ]
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
get_granularity <- function(x, timespan = NULL, quiet = FALSE){
  if (is.null(timespan)){
    granularity(x)
  } else {
    timespan(timespan)
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

seconds_to_higher_timespan <- function(x){

  if (length(x) > 1){
    return(x)
  }

  check_length_lte(x, 1)

  if (length(x) == 0L){
    return(new_timespan("seconds", numeric()))
  }
  if (is.na(x)){
    return(new_timespan("seconds", NA_real_))
  }

  num <- as.double(x)

  if ( (num %% 3600) == 0 ){
    num <- num / 3600
    unit <- "hours"
  } else if ( (num %% 60) == 0 ){
    num <- num / 60
    unit <- "minutes"
  } else {
    unit <- "seconds"
  }

  new_timespan(unit, num)
}
unit_to_seconds <- function(x){
  unit_info <- timespan(x)
  unit <- timespan_unit(unit_info)
  num <- timespan_num(unit_info)

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

# Calculate size of period unit to expand from and to for specified length
# period_by_calc2 <- function(from, to, length){
#   quo <- (length - 1)
#
#   # This returns `0` when `length == 1`
#   quo[cheapr::val_find(quo, 0)] <- Inf
#
#   # First try and see if we can create a sequence in
#   # whole months, if not then whole days,
#   # if not then it must be in seconds
#
#   month_delta <- time_diff(from, to, new_timespan("months")) / quo
#
#   if (is_whole_number(month_delta)){
#     out <- new_timespan("months", month_delta)
#   } else {
#     day_delta <- time_diff(from, to, new_timespan("days")) / quo
#     if (is_whole_number(day_delta)){
#       out <- new_timespan("days", day_delta)
#     } else {
#       out <- time_diff(from, to, new_timespan("seconds")) / quo
#       out <- seconds_to_higher_timespan(new_timespan("seconds", out))
#     }
#   }
#   out
# }

period_by_calc <- function(from, to, length){

  quo <- (length - 1L)

  # Use `fct` as a factor to multiply and `quo` as a divisor
  fct <- quo
  quo[cheapr::val_find(quo, 0)] <- Inf

  # Difference in whole months
  # multiply this ans by (length - 1) and add to `from`
  # and if it equals `to` then the ans can be returned

  month_delta <- diff_months(from, to, n = quo, fractional = FALSE)
  up <- from %>%
    time_add(new_timespan("months", month_delta * fct), roll_month = "xlast")
  if (identical(up, to)){
    out <- new_timespan("months", month_delta)
  } else {
    day_delta <- diff_days(from, to, n = quo, fractional = FALSE)
    up <- from %>%
      time_add(new_timespan("days", day_delta * fct))
    if (identical(up, to)){
      out <- new_timespan("days", day_delta)
    } else {
      out <- time_diff(from, to, new_timespan("seconds", quo))
      out <- seconds_to_higher_timespan(new_timespan("seconds", out))
    }
  }
  out
}

num_by_calc <- function(from, to, length){
  by <- (unclass(to) - unclass(from)) / (cheapr::val_replace(length - 1, 0, Inf))
  new_timespan(NA_character_, as.double(by))
}
time_by_calc <- function(from, to, length){
  if (is_time(from) && is_time(to)){
    period_by_calc(from, to, length)
  } else {
    num_by_calc(from, to, length)
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
  if (identical(parent.frame(n = 1), globalenv())){
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

check_is_date <- function(x){
  if (!is_date(x)){
    cli::cli_abort("{.arg x} must be a date")
  }
}
check_is_datetime <- function(x){
  if (!is_datetime(x)){
    cli::cli_abort("{.arg x} must be a datetime")
  }
}
check_is_time <- function(x){
  if (!is_time(x)){
    cli::cli_abort("{.arg x} must be a date or datetime")
  }
}
check_is_time_or_num <- function(x){
  if (!is_time_or_num(x)){
    cli::cli_abort(c("{.arg x} must be one of the following classes:",
                   paste(time_classes, collapse = ", ")))
  }
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

### Taken from lubridate ###

# Accepts an estimate ala (interval / duration)
# Start datetime, end datetime, and period object
adj_dur_est <- function (est, start, end, width){
  est <- ceiling(est)
  up_date <- time_add(
    start, cheapr::val_replace(width * est, NaN, NA),
    roll_month = "xlast", roll_dst = c("NA", "xfirst")
  )
  while (length(which <- which(up_date < end))) {
    est[which] <- est[which] + 1
    up_date[which] <- time_add(
      start[which],
      width[which] * est[which],
      roll_month = "xlast", roll_dst = c("NA", "xfirst")
    )
  }
  low_date <- up_date
  while (length(which <- which(low_date > end))) {
    est[which] <- est[which] - 1
    up_date[which] <- low_date[which]
    low_date[which] <- time_add(
      start[which],
      width[which] * est[which],
      roll_month = "xlast", roll_dst = c("NA", "xfirst")
    )
  }
  frac <- ( unclass(end) - unclass(low_date) ) /
    ( unclass(up_date) - unclass(low_date) )
  frac <- strip_attrs(frac)
  frac[which(low_date == up_date)] <- 0
  est + frac
}
# Faster method for interval(start, end) / period() when period
# is a single unit period which is very common
divide_interval_by_period <- function(start, end, width){
  if (length(start) == 0 || length(end) == 0 || length(width) == 0) {
    return(numeric())
  }
  start <- as_datetime2(start)
  end <- as_datetime2(end)
  estimate <- strip_attrs((unclass(end) - unclass(start)) / unit_to_seconds(width))
  timespans <- cheapr::recycle(start = start, end = end, width = width)
  start <- timespans[[1L]]
  end <- timespans[[2L]]
  width <- timespans[[3L]]

  if (cheapr::na_count(estimate) == 0) {
    adj_dur_est(estimate, start, end, width)
    # adjust_duration_estimate(as.double(estimate), start, end, as.double(timespan_num(width)), timespan_unit(width))
  } else {
    not_nas <- which_not_na(estimate)
    start <- start[not_nas]
    end <- end[not_nas]
    width <- width[not_nas]
    estimate[not_nas] <- adj_dur_est(estimate[not_nas], start, end, width)
    estimate
  }
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
int_to_per <- function(start, end){
  set_recycle_args(start, end)
  start <- as_datetime2(start)
  end <- time_cast(end, start)
  duration <- strip_attrs(end) - strip_attrs(start)
  start <- unclass(as.POSIXlt(start))
  end <- unclass(as.POSIXlt(end))
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

is_duration_unit <- function(x){
  collapse::fmatch(x, .time_units) <= 7L
}
