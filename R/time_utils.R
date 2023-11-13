# Time utility functions
# Error for the user to see available units
unit_match_stop <- function(units = .time_units){
  stop(paste0("'arg' should be one of '", paste(units, collapse = "', '"), "'"))
}
# The idea is that when you supply a list, no regex
# is needed to separate out the numbers
# This is handy with loops to reduce overhead
unit_list_match <- function(l){
  if (length(l) != 1L) stop("l must of a list of length 1.")
  if (names(l) == "numeric") {
    unit <- "numeric"
  } else {
    unit <- rlang::arg_match0(names(l), .time_units)
  }
  if (is.na(unit)) unit_match_stop()
  # if (length(unit) == 0L) stop("unit list must be named")
  num <- .subset2(l, 1L)
  scale <- 1L
  if (unit %in% .extra_time_units){
    exotic_info <- convert_exotic_units(unit)
    scale <- .subset2(exotic_info, "scale")
    unit <- .subset2(exotic_info, "unit")
  }
  # num <- num * scale
  list("unit" = unit,
       "num" = num,
       "scale" = scale)
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
  if (length(num) == 0L) num <- 1
  scale <- 1
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
  out <- list("unit" = unit,
              "num" = num,
              "scale" = scale)
  out
}
time_by_list <- function(time_by){
  unit_info <- unit_guess(time_by)
  units <- .subset2(unit_info, "unit")
  num <- .subset2(unit_info, "num") * .subset2(unit_info, "scale")
  add_names(list(num), units)
}
# Returns list with numeric vector element, where the name of the list
# is the time unit name
time_by_get <- function(x, time_by = NULL, is_sorted = FALSE,
                        quiet = FALSE){
  if (is.null(time_by)){
    unit_info <- time_granularity(x, is_sorted = is_sorted,
                                  msg = !quiet)
    by_n <- unit_info[["num"]]
    by_unit <- unit_info[["unit"]]
    out <- add_names(list(by_n), by_unit)
  } else {
    out <- time_by_list(time_by)
  }
  out
}
time_by_length <- function(time_by){
  length(time_by_num(time_by))
}
time_by_num <- function(time_by){
  time_by[[1L]]
}
time_by_unit <- function(time_by){
  names(time_by)
}
time_by_is_num <- function(time_by){
  isTRUE(class(time_by) %in% c("integer", "numeric")) ||
    (is.list(time_by) &&
       length(time_by_unit(time_by)) == 1L &&
       time_by_unit(time_by) == "numeric")
}
time_by_sign <- function(time_by){
  sign(time_by_num(time_by))
}
check_time_by_length_is_one <- function(time_by){
  if (time_by_length(time_by) != 1){
    stop("Please supply only one numeric value in time_by")
  }
}
time_by_pretty <- function(time_by){
  time_by <- time_by_list(time_by)
  units <- names(time_by)
  if (time_by_length(time_by) > 1){
    stop("Please supply only one numeric value in time_by")
  }
  num <- time_by[[1L]]
  if (units == "numeric"){
    if (num == 1){
      paste(num, "numeric unit", sep = " ")
    } else {
      paste(num, "numeric units", sep = " ")
    }
  } else {
    num_seconds <- unit_to_seconds(time_by)
    higher_unit_info <- seconds_to_unit(num_seconds)
    scale <- higher_unit_info$scale
    higher_unit <- higher_unit_info$unit
    num <- num_seconds / scale
    units <- higher_unit
    # pretty_unit_info <- seconds_to_unit(num_seconds)
    # pretty_unit <- sub("(s)", "", pretty_unit_info[["unit"]],
    #                    fixed = TRUE)
    # scale <- pretty_unit_info[["scale"]]
    # pretty_num <- prettyNum(round(num_seconds / scale, 2))
    pretty_num <- round(num, 2)
    if (!double_equal(num, pretty_num)){
      pretty_num <- paste0("~", pretty_num)
    }
    if (num == 1){
      paste0(plural_unit_to_single(units))
    } else {
      paste0(pretty_num, " ", units)
    }
  }
}
# Creates interval even using num
time_interval <- function(from, to){
  if (is_time(from) && is_time(to)){
    out <- lubridate::interval(from, to)
  } else {
    out <- time_diff(from, to, time_by = 1)
    if (length(from) < length(out)){
      from <- rep_len(from, length(out))
    }
    attr(out, "start") <- from
  }
  out
}
time_granularity <- function(x, is_sorted = FALSE, msg = TRUE){
  gcd_diff <- time_diff_gcd(x, is_sorted = is_sorted)
  if (length(gcd_diff) == 0){
    gcd_diff <- 1
  }
  if (is_date(x)){
    granularity <- "day(s)"
    scale <- 1
    unit <- "days"
    num_and_unit <- paste(gcd_diff, unit, sep = " ")
  } else if (is_datetime(x)){
    convert_seconds <- seconds_to_unit(gcd_diff)
    scale <- convert_seconds[["scale"]]
    granularity <- convert_seconds[["unit"]]
    granularity <- paste0(plural_unit_to_single(granularity), "(s)")
    unit <- "seconds"
    num_and_unit <- stringr::str_c(gcd_diff, unit, sep = " ")
  } else {
    granularity <- "numeric unit(s)"
    scale <- 1
    unit <- "numeric"
    num_and_unit <- gcd_diff
  }
  if (msg){
    message(paste("Assuming a time granularity of",
                  gcd_diff/scale,
                  granularity,
                  sep = " "))
  }
  list("granularity" = granularity,
       "unit" = unit,
       "num" = gcd_diff,
       "num_and_unit" = num_and_unit,
       "scale" = scale)
}
# A more focused version
time_granularity2 <- function(x, is_sorted = FALSE){
  gcd_diff <- time_diff_gcd(x, is_sorted = is_sorted)
  if (length(gcd_diff) == 0){
    gcd_diff <- 1
  }
  if (is_date(x)){
    unit <- "days"
    scale <- 1
  } else if (is_datetime(x)){
    convert_seconds <- seconds_to_unit(gcd_diff)
    unit <- convert_seconds[["unit"]]
    scale <- convert_seconds[["scale"]]
  } else {
    unit <- "numeric"
    scale <- 1
  }
  add_names(list(gcd_diff / scale), unit)
}
# Converts seconds to duration unit
# Scale is in comparison to seconds
seconds_to_unit <- function(x){
  if (length(x) == 0L){
    return(list("unit" = "seconds",
                "scale" = numeric(0)))
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
# unit_to_seconds <- function(x){
#   unit_info <- unit_guess(x)
#   unit <- unit_info[["unit"]]
#   num <- unit_info[["num"]] * unit_info[["scale"]]
#   if (unit == "picoseconds"){
#     scale <- 1/1000/1000/1000/1000
#   } else if (unit == "nanoseconds"){
#     scale <- 1/1000/1000/1000
#   } else if (unit == "microseconds"){
#     scale <- 1/1000/1000
#   } else if (unit == "milliseconds"){
#     scale <- 1/1000
#   }  else if (unit == "seconds"){
#     scale <- 1
#   } else if (unit == "minutes"){
#     scale <- 60
#   } else if (unit == "hours"){
#     scale <- 3600
#   } else if (unit == "days"){
#     scale <- 86400
#   } else if (unit == "weeks"){
#     scale <- 604800
#   } else if (unit == "months"){
#     scale <- 2629800
#   } else if (unit == "years"){
#     scale <- 31557600
#   }
#   num * scale
# }
unit_to_seconds <- function(x){
  unit_info <- unit_guess(x)
  unit <- unit_info[["unit"]]
  num <- unit_info[["num"]] * unit_info[["scale"]]
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
# No string guessing at all
guess_seq_type <- function(units){
  if (units %in% c("days", "weeks", "months", "years",
                   .extra_time_units)){
    "period"
  } else if (units %in% c("picoseconds", "nanoseconds",
                          "microseconds", "milliseconds",
                          "seconds", "minutes", "hours")){
    "duration"
  } else {
    "numeric"
  }
}
# get_time_type <- function(time_type, time_by){
#   type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
#   if (type == "duration"){
#     out <- "duration"
#   }
#   if (type == "period"){
#    out <- "period"
#   }
#   if (type == "auto"){
#     if (inherits(time_by, "Duration")){
#       out <- "Duration"
#     } else if (inherits(time_by, "Period")){
#       out <- "Period"
#     } else {
#       units <- time_by_unit(time_by)
#       if (units %in% c("days", "weeks", "months", "years",
#                        .extra_time_units)){
#         out <- "period"
#       } else if (units %in% c("picoseconds", "nanoseconds",
#                               "microseconds", "milliseconds",
#                               "seconds", "minutes", "hours")){
#         out <- "duration"
#       } else {
#         out <- "numeric"
#       }
#     }
#   }
#   out
# }
# date_formats_to_try <- c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d",
#                          "%d-%m-%Y", "%d/%m/%Y", "%d%m%Y")
# time_formats_to_try <- c("%Hh%Mm%Ss", " %Hh%Mm%Ss", "%Hh:%Mm:%Ss", " %Hh:%Mm:%Ss",
#                          "%H%M%S", " %H%M%S", "%H:%M:%S", " %H:%M:%S")
# date_time_formats_to_try <- apply(expand.grid(date_formats_to_try,
#                                               time_formats_to_try),
#                                   1, function(x) paste(x, collapse = ""))
convert_common_dates <- function(x){
  if (is_time(x)){
    out <- x
  } else if (is.character(x)){
    which_na <- cpp_which(is.na(x))
    out <- lubridate::ymd(x, quiet = TRUE)
    num_na <- num_na(out)
    if (num_na > length(which_na)){
      out <- lubridate::dmy(x, quiet = TRUE)
    }
    num_na <- num_na(out)
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
  which_len_1 <- cpp_which(length == 1)
  sec_diff <- time_diff(from, to,
                        time_by = list("seconds" = 1),
                        time_type = "period")
  out <- lubridate::seconds_to_period(sec_diff / (length - 1))
  period_info <- collapse::qDF(time_unit_info(out))
  n_unique_slots <- df_ncol(period_info) - rowSums(period_info == 0)
  which_multi <- cpp_which(n_unique_slots > 1)
  out[which_multi] <- seconds_unit(
    lubridate::period_to_seconds(out[which_multi])
  )
  out[which_len_1] <- seconds_unit(0)
  out
}
# More accurate but slower and less efficient
# period_by_calc2 <- function(from, to, length){
#   periods_to_try <- rev(.period_units)
#   n_units <- length(periods_to_try)
#   for (i in seq_len(n_units)){
#     assign(periods_to_try[i], period_unit(periods_to_try[i])(1))
#   }
#   set_recycle_args(from, to, length)
#   out <- matrix(numeric(length(from) * n_units), ncol = n_units)
#   colnames(out) <- periods_to_try
#   out2 <- out
#   for (j in seq_len(n_units)){
#     out2[, j] <- divide_interval_by_period2(from, to, get(periods_to_try[j]))
#   }
#   ok <- apply(out2, 2, function(x) x / (length - 1))
#   attributes(ok) <- attributes(out)
#   which_len_1 <- which(length == 1)
#   if (length(which_len_1) > 0){
#     ok[which_len_1, ] <- 0
#   }
#   remainder <- ok %% 1
#   ok2 <- abs(out2) >= 1 & remainder == 0
#   ok3 <- ok2 == TRUE
#   res <- max.col(ok3, ties.method = "first")
#   res[rowSums(ok3) == 0] <- n_units
#   # Special way of subsetting specific elements from matrix
#   msub <- matrix(c(seq_len(length(from)), res), ncol = 2)
#   out[msub] <- ok[msub]
#   out[, 1:(n_units - 1)][(out[, 1:(n_units - 1)] %% 1) != 0] <- 0
#   do.call(get("period", asNamespace("lubridate")), as.data.frame(out))
# }
# period_by <- function(from, to, length){
#   if (length == 1){
#     lubridate::seconds(0)
#   } else {
#     int <- lubridate::interval(from, to)
#     periods_to_try <- rev(.period_units)
#     for (i in seq_along(periods_to_try)){
#       unit <- period_unit(periods_to_try[[i]])
#       division <- int / unit(1)
#       # remainder <- division %% 1
#       remainder <- ( (division / (length - 1)) %% 1 )
#       if (abs(division) >= 1 &&
#           remainder == 0){
#         out <- unit( (division / (length - 1)) )
#         return(out)
#       }
#     }
#     unit( (division / (length - 1)) )
#   }
# }
# Calculates size of duration to cut a pre-specified interval of
# a certain length
duration_by_calc <- function(from, to, length){
  seconds_unit <- duration_unit("seconds")
  sec_diff <- time_diff(from, to,
                        time_by = list("seconds" = 1),
                        time_type = "duration")
  out <- seconds_unit(sec_diff / (length - 1))
  length <- rep_len(length, length(out))
  out[cpp_which(length == 1)] <- seconds_unit(0) # Special case
  out
}
num_by_calc <- function(from, to, length){
  out <- (to - from) / (length - 1)
  length <- rep_len(length, length(out))
  out[cpp_which(length == 1)] <- 0
  out
}
# Vectorized except for periods
time_by_calc <- function(from, to, length, time_type){
  if (is_time(from) && is_time(to)){
    if (time_type == "period"){
      period_by_calc(from, to, length)
    } else {
      duration_by_calc(from, to, length)
    }
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
    keep <- vapply(sum_rng, function(x) isTRUE(double_gt(x, 0)), FALSE)
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
# Functional that returns lubridate period function
period_unit <- function(units = "seconds"){
  if (!units %in% .period_units) unit_match_stop(.period_units)
  switch(units,
         seconds = lubridate::seconds,
         minutes = lubridate::minutes,
         hours = lubridate::hours,
         days = lubridate::days,
         weeks = lubridate::weeks,
         months = months,
         years = lubridate::years)
}
numeric_unit <- function(units){
  identity
}
# # Functional that returns lubridate duration function
# duration_unit <- function(units = "seconds"){
#   if (length(units) <= 1L){
#     if (!units %in% .duration_units) unit_match_stop(.duration_units)
#     switch(units,
#            picoseconds = lubridate::dpicoseconds,
#            nanoseconds = lubridate::dnanoseconds,
#            microseconds = lubridate::dmicroseconds,
#            milliseconds = lubridate::dmilliseconds,
#            seconds = lubridate::dseconds,
#            minutes = lubridate::dminutes,
#            hours = lubridate::dhours,
#            days = lubridate::ddays,
#            weeks = lubridate::dweeks,
#            months = lubridate::dmonths,
#            years = lubridate::dyears)
#   } else {
#     if (length(setdiff(units, .duration_units)) > 0L) unit_match_stop(.duration_units)
#     fns_list <- c(lubridate::dpicoseconds,
#                   lubridate::dnanoseconds,
#                   lubridate::dmicroseconds,
#                   lubridate::dmilliseconds,
#                   lubridate::dseconds,
#                   lubridate::dminutes,
#                   lubridate::dhours,
#                   lubridate::ddays,
#                   lubridate::dweeks,
#                   lubridate::dmonths,
#                   lubridate::dyears)
#     fns_list[match(units, .duration_units)]
#   }
# }
# # Functional that returns lubridate period function
# period_unit <- function(units = "seconds"){
#   if (length(units) <= 1L){
#     if (!units %in% .period_units) unit_match_stop(.period_units)
#     switch(units,
#            seconds = lubridate::seconds,
#            minutes = lubridate::minutes,
#            hours = lubridate::hours,
#            days = lubridate::days,
#            weeks = lubridate::weeks,
#            months = months,
#            years = lubridate::years)
#   } else {
#     if (length(setdiff(units, .period_units)) > 0L) unit_match_stop(.period_units)
#     fns_list <- c(lubridate::seconds,
#                   lubridate::minutes,
#                   lubridate::hours,
#                   lubridate::days,
#                   lubridate::weeks,
#                   months,
#                   lubridate::years)
#     fns_list[match(units, .period_units)]
#   }
# }
# Functional that returns time unit function
time_unit <- function(units, time_type = c("duration", "period", "numeric")){
  time_type <- rlang::arg_match0(time_type, c("duration", "period", "numeric"))
  if (units == "numeric"){
    numeric_unit(units)
  } else if (time_type == "duration"){
    duration_unit(units)
  } else {
    period_unit(units)
  }
}
# Coerce pair of time based vectors to the most informative
# class between them
set_time_cast <- function(x, y){
  if (identical(parent.frame(n = 1), globalenv())){
    stop("Users cannot use set_time_cast from the global environment")
  }
  xcl <- class(x)
  ycl <- class(y)
  # We time cast when either:
  # * length  of classes differ
  # * class values differ
  # We do the length check first to avoid length recycling messages
  if (
    ( length(xcl) != length(ycl) ) ||
    ( sum(xcl != ycl) > 0 )
  ){
    x_nm <- simple_deparse(substitute(x))
    y_nm <- simple_deparse(substitute(y))
    assign(x_nm, time_cast(x, y), envir = parent.frame(n = 1))
    assign(y_nm, time_cast(y, x), envir = parent.frame(n = 1))
  }
}

# Safe time concatenation
# time_c <- function(...){
#   vctrs::vec_c(...)
# }
# Faster time_cast
# numeric > yearqtr > yearmon > date > datetime
# You can move from left to right but not right to left
time_cast <- function(x, template){
  if (inherits(template, "POSIXt")){
    if (!inherits(x, c("Date", "POSIXt"))){
      x <- as.POSIXct(x, origin = lubridate::origin)
    }
    lubridate::with_tz(x, tzone = lubridate::tz(template))
    # as.POSIXct(x, tz = lubridate::tz(template), origin = lubridate::origin)
  } else if (inherits(template, "Date") && !inherits(x, "POSIXt")){
    lubridate::as_date(x)
  } else if (inherits(template, "yearmon") &&
             !inherits(x, c("POSIXt", "Date"))){
    as_yearmon(x)
  } else if (inherits(template, "yearqtr") &&
             !inherits(x, c("POSIXt", "Date", "yearmon"))){
    as_yearqtr(x)
  } else {
    x
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
# This bounds start point based on x vector
bound_from <- function(from, x){
  x_min <- collapse::fmin(x, na.rm = TRUE,
                          use.g.names = FALSE)
  if (is.null(from)){
    from <- x_min
  } else {
    from <- time_cast(from, x) # Cast to as datetime if x is
    # Bound from by the minimum of x
    from <- max(x_min, from)
  }
  from
}
# This bounds end point based on x vector
bound_to <- function(to, x){
  x_max <- collapse::fmax(x, na.rm = TRUE,
                          use.g.names = FALSE)
  if (is.null(to)){
    to <- x_max
  } else {
    to <- time_cast(to, x) # Cast to as datetime if x is
    # Bound to by the maximum of x
    to <- min(x_max, to)
  }
  to
}

fcut_ind <- function(x, breaks, rightmost.closed = FALSE,
                     left.open = FALSE, all.inside = FALSE){
  breaksi <- findInterval(x,
                          breaks,
                          rightmost.closed = rightmost.closed,
                          left.open = left.open,
                          all.inside = all.inside)
  # This makes it so that NA is returned for any x where findinterval
  # resorts to 0 and doesn't just remove them
  breaksi[collapse::whichv(breaksi, 0L)] <- NA_integer_
  breaksi
}
cut_time2 <- function(x, breaks, rightmost.closed = FALSE, left.open = FALSE){
  breaks[
    fcut_ind(x,
             breaks,
             rightmost.closed = rightmost.closed,
             left.open = left.open,
             all.inside = FALSE)
  ]
}
# Newer version of cut_time2
# min(breaks) is expected to be <= min(x)
# Interval is closed on the left
# out-of-bounds times can be included in the last interval
# This can return either break codes or the cut vector
# cut_time <- function(x, breaks, include_oob = FALSE, codes = FALSE,
#                      end = NULL){
#   x <- `attributes<-`(unclass(x), NULL)
#   breaks_num <- `attributes<-`(unclass(breaks), NULL)
#   end <- `attributes<-`(unclass(end), NULL) + 1
#   inf_val <- Inf * (numeric(as.integer(include_oob)) + 1)
#   breaks_num <- c(breaks_num, end, inf_val)
#   out <- .bincode(x, breaks = breaks_num, right = FALSE, include.lowest = FALSE)
#   if (codes){
#     out
#   } else {
#     breaks[out]
#   }
# }
cut_time <- function(x, breaks, include_oob = FALSE, codes = FALSE){
  breaks_num <- unclass(breaks)
  # Cheeky way of converting FALSE to numeric() and TRUE to Inf without if statement
  inf_val <- Inf * (numeric(as.integer(include_oob)) + 1)
  breaks_num <- c(breaks_num, inf_val)
  out <- .bincode(unclass(x), breaks = breaks_num, right = FALSE, include.lowest = TRUE)
  if (codes){
    out
  } else {
    breaks[out]
  }
}
# cut_time_intervals <- function(x, breaks, codes = FALSE, end = NULL){
#   x <- unclass(x)
#   breaks_num <- unclass(breaks)
#   end <- unclass(end) + 0
#   breaks_num <- c(breaks_num, end)
#   out <- findInterval(x, breaks_num, rightmost.closed = TRUE)
#   # n_breaks <- length(breaks_num)
#   # collapse::setv(out, n_breaks, NA_integer_, vind1 = FALSE)
#   if (codes){
#     out
#   } else {
#     breaks[out]
#   }
# }
is_date_or_utc <- function(x){
  is_date(x) || lubridate::tz(x) == "UTC"
}
# Check for date sequences that should not be coerced to datetimes
is_special_case_days <- function(from, to, unit, num, time_type){
  time_type == "auto" &&
    unit %in% c("days", "weeks") &&
    is_date(from) &&
    is_date(to) &&
    is_whole_number(num)
}
is_special_case_utc <- function(from, to, unit, num, time_type){
  time_type == "auto" &&
    unit %in% c("days", "weeks") &&
    is_datetime(from) &&
    is_datetime(to) &&
    lubridate::tz(from) == "UTC" &&
    lubridate::tz(to) == "UTC" &&
    is_whole_number(num)
}
# Repeat methods for zoo yearmon and yearqtr class
rep_len.yearmon <- function(x, length.out){
  x[rep_len(seq_along(x), length.out = length.out)]
}
rep.int.yearmon <- function(x, ...){
  x[rep.int(seq_along(x), ...)]
}
rep.yearmon <- function(x, ...){
  x[rep(seq_along(x), ...)]
}
rep_len.yearqtr <- function(x, length.out){
  x[rep_len(seq_along(x), length.out = length.out)]
}
rep.int.yearqtr <- function(x, ...){
  x[rep.int(seq_along(x), ...)]
}
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
  isS4(x) && inherits(x, "Interval")
}
# time_agg2 <- function(time_seq_data, data, time, g){
#   by <- dplyr::join_by(!!rlang::sym(g), closest(!!rlang::sym(time) >= !!rlang::sym(time)))
#   data %>%
#     dplyr::left_join(time_seq_data, by = by,
#                      suffix = c(".raw", "")) %>%
#     dplyr::pull(all_of(stringr::str_c(time, ".raw")))
#     # dplyr::select(-all_of(stringr::str_c(time, ".raw")))
# }

# Grouped functions that utilise
# x - time variable (ascending order)
# seq - regular time sequence (ascending order)
# gx - Integer group ID of x (ascending order)
# gseq - Integer group ID of seq (ascending order)

# Aggregate x to higher time unit based on seq
# taggregate2 <- function(x, seq, gx, gseq){
#   data <- data.frame(t = x, g = gx)
#   lookup <- data.frame(seq = seq, g = gseq)
#   by <- dplyr::join_by("g", closest("t" >= "seq"))
#   data %>%
#     dplyr::left_join(lookup, by = by, multiple = "any") %>%
#     dplyr::pull(all_of("seq"))
# }
# taggregate <- function(x, seq, gx = NULL, gseq = NULL){
#   dt1 <- data.table::data.table(g = gx, t = x)
#   dt2 <- data.table::data.table(g = gseq, t = seq)
#   # Set key directly as inputs should all be pre-sorted.
#   data.table::setattr(dt1, "sorted", names(dt1))
#   data.table::setattr(dt2, "sorted", names(dt2))
#   dt1[dt2, ("tagg") := .SD, .SDcols = "t", mult = "first",
#       roll = -Inf, on = names(dt1)]
#   # dt1[dt2, ("tagg") := .SD, .SDcols = "t", mult = "first", on = .(g, t >= t)]
#   data.table::setnafill(dt1, cols = "tagg", type = "locf")
#   dt1[["tagg"]]
# }

# Convert time sequence to interval
tseq_interval <- function(x, seq, gx = NULL, gseq = NULL){
  out <- time_interval(seq, flag2(seq, n = -1, g = gseq))
  to <- collapse::fmax(x, g = gx, use.g.names = FALSE, na.rm = TRUE)
  end_points <- cpp_which(is.na(out) & !is.na(seq))
  out[end_points] <- time_interval(seq[end_points], to)
  out
}
# Interval from x, aggregate x, and seq
# tagg_interval <- function(xagg, x, seq, gagg = NULL, gx = NULL, gseq = NULL){
#   int <- tseq_interval(x = x, seq = seq, gx = gx, gseq = gseq)
#   agg_df <- dplyr::tibble(t = xagg,
#                           g = gagg)
#   int_df <- dplyr::tibble(t = seq,
#                           g = gseq,
#                           interval = int)
#   agg_df %>%
#     dplyr::left_join(int_df, by = c("g", "t"),
#                      multiple = "any") %>%
#     fpluck("interval")
# }
# Convert time sequence to min max list
# tseq_min_max <- function(x, seq, gx = NULL, gseq = NULL){
#   n <- length(x)
#   end <- flag2(seq, n = max(-1L, -n), g = gseq)
#   to <- collapse::fmax(x, g = gx, use.g.names = FALSE, na.rm = TRUE)
#   end_points <- which(is.na(end) & !is.na(seq))
#   setv(end, end_points, to, vind1 = TRUE)
#   list(min = seq, max = end)
# }
# Time cut levels from ascending time sequence
tseq_levels <- function(x, seq, gx = NULL, gseq = NULL, fmt = NULL){
  if (is.null(fmt)){
    fmt_f <- time_as_character
  } else {
    fmt_f <- function(x, ...) format(x, format = fmt, ...)
  }
  n <- length(x)
  time_breaks_fmt <- fmt_f(seq)
  out <- stringr::str_c("[",
                        time_breaks_fmt,
                        ", ",
                        flag2(time_breaks_fmt,
                              g = gseq, n = max(-1L, -n)),
                        ")")
  to <- collapse::fmax(x, g = gx, use.g.names = FALSE, na.rm = TRUE)
  end_points <- cpp_which(is.na(out) & !is.na(seq))
  out[end_points] <- stringr::str_c("[",
                                    time_breaks_fmt[end_points],
                                    ", ",
                                    fmt_f(to),
                                    "]")
  out
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
C_time_add <- getFromNamespace("C_time_add", "timechange")
time_add2 <- function(x, time_by,
                      time_type = getOption("timeplyr.time_type", "auto"),
                      roll_month = getOption("timeplyr.roll_month", "preday"), roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  time_by <- time_by_list(time_by)
  time_num <- time_by_num(time_by)
  time_unit <- time_by_unit(time_by)
  if (time_by_is_num(time_by)){
    x + time_num
  } else {
    time_type <- match_time_type(time_type)
    if (time_type == "auto"){
      time_type <- guess_seq_type(time_unit)
    }
    if (time_type == "period"){
      unit <- plural_unit_to_single(time_unit)
      time_add(x, periods = add_names(list(time_num), unit),
               roll_month = roll_month, roll_dst = roll_dst)
    } else {
      x + duration_unit(time_unit)(time_num)
    }
  }
}
time_floor <- function(x, time_by, week_start = getOption("lubridate.week.start", 1)){
  unit_info <- unit_guess(time_by)
  by_unit <- unit_info[["unit"]]
  by_n <- unit_info[["num"]] * unit_info[["scale"]]
  if (is_time(x)){
    time_by <- paste(by_n, by_unit)
    timechange::time_floor(x, unit = time_by, week_start = week_start)
  } else {
    floor(x / by_n) * by_n
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
  unit_info <- unit_guess(time_by)
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
  out <- time_as_number(Sys.Date()) + 1
  class(out) <- "Date"
  out
}
### All credit goes to the scales package developers for this function
label_date_short <- function(format = c("%Y", "%b", "%d", "%H:%M"),
                             sep = "\n"){
  force_all <- function(...) list(...)
  changed <- function(x){
    c(TRUE, is.na(x[-length(x)]) | x[-1] != x[-length(x)])
  }
  force_all(format, sep)
  function(x) {
    dt <- unclass(as.POSIXlt(x))
    changes <- cbind(year = changed(dt$year), month = changed(dt$mon),
                     day = changed(dt$mday))
    changes <- t(apply(changes, 1, cumsum)) >= 1
    if (inherits(x, "Date") || all(dt$hour == 0 & dt$min ==
                                   0, na.rm = TRUE)) {
      format[[4]] <- NA
      if (all(dt$mday == 1, na.rm = TRUE)) {
        format[[3]] <- NA
        if (all(dt$mon == 0, na.rm = TRUE)) {
          format[[2]] <- NA
        }
      }
    }
    for_mat <- cbind(ifelse(changes[, 1], format[[1]], NA),
                     ifelse(changes[, 2], format[[2]], NA), ifelse(changes[,
                                                                           3], format[[3]], NA), format[[4]])
    format <- apply(for_mat, 1, function(x) paste(rev(x[!is.na(x)]),
                                                  collapse = sep))
    format(x, format)
  }
}
# Unique posix vector to character remains unique
time_as_character <- function(x){
  if (is_datetime(x)){
    format(x, format = "%Y-%m-%d %H:%M:%S %Z")
  } else {
    as.character(x)
  }
}
# Fast NA check for lubridate intervals
int_is_na <- function(x){
  X <- interval_separate(x)
  is.na(X[[1L]]) & is.na(X[[2L]])
}
time_as_number <- function(x){
  strip_attrs(unclass(x))
}
# Important that this holds:
# length(x) == length(start) == length(end)
time_aggregate_left <- function(x, time_by, g = NULL,
                                start = NULL, end = NULL,
                                time_floor = FALSE,
                                week_start = getOption("lubridate.week.start", 1),
                                time_type = getOption("timeplyr.time_type", "auto"),
                                roll_month = getOption("timeplyr.roll_month", "preday"), roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                                as_int = TRUE){
  time_by <- time_by_list(time_by)
  num <- time_by_num(time_by)
  units <- time_by_unit(time_by)
  time_na <- na_init(x)
  g <- GRP2(g, return.groups = FALSE)
  if (!is.null(start)){
    if (length(start) != length(x)){
      stop("start must be the same length as x")
    }
    start <- time_cast(start, x)
    x[cpp_which(x < start)] <- time_na
  } else {
    start <- gmin(x, g = g, na.rm = TRUE)
  }
  if (!is.null(end)){
    if (length(end) != length(x)){
      stop("end must be the same length as x")
    }
    end <- time_cast(end, x)
    x[cpp_which(x > end)] <- time_na
  } else {
    end <- gmax(x, g = g, na.rm = TRUE)
  }
  if (time_floor){
    start <- time_floor2(start, time_by = time_by,
                         week_start = week_start)
  }
  tdiff <- time_diff(start, x, time_by = time_by, time_type = time_type)
  time_to_add <- add_names(list(trunc(tdiff) * num), units)
  out <- time_add2(start, time_by = time_to_add, time_type = time_type,
                   roll_month = roll_month, roll_dst = roll_dst)
  if (as_int){
    int_end <- time_add2(out, time_by = time_by, time_type = time_type,
                         roll_month = roll_month, roll_dst = roll_dst)
    set_time_cast(out, int_end)
    end <- time_cast(end, out)
    which_out_of_bounds <- cpp_which(double_gt(time_as_number(int_end),
                                               time_as_number(end)))
    int_end[which_out_of_bounds] <- end[which_out_of_bounds]
    out <- structure(out,
                     end = int_end,
                     direction = "left-to-right")
  }
  out
}
# Important that this holds:
# length(x) == length(start) == length(end)
time_aggregate_right <- function(x, time_by, g = NULL,
                                 start = NULL, end = NULL,
                                 time_ceiling = FALSE,
                                 week_start = getOption("lubridate.week.start", 1),
                                 time_type = getOption("timeplyr.time_type", "auto"),
                                 roll_month = getOption("timeplyr.roll_month", "preday"), roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                                 as_int = TRUE){
  time_by <- time_by_list(time_by)
  num <- time_by_num(time_by)
  units <- time_by_unit(time_by)
  time_na <- na_init(x)
  g <- GRP2(g, return.groups = FALSE)
  if (!is.null(start)){
    if (length(start) != length(x)){
      stop("start must be the same length as x")
    }
    start <- time_cast(start, x)
    x[cpp_which(x < start)] <- time_na
  } else {
    start <- gmin(x, g = g, na.rm = TRUE)
  }
  if (!is.null(end)){
    if (length(end) != length(x)){
      stop("end must be the same length as x")
    }
    end <- time_cast(end, x)
    x[cpp_which(x > end)] <- time_na
  } else {
    end <- gmax(x, g = g, na.rm = TRUE)
  }
  if (time_ceiling){
    start <- time_ceiling2(start, time_by = time_by,
                           week_start = week_start)
  }
  tdiff <- time_diff(end, x, time_by = time_by, time_type = time_type)
  time_to_add <- add_names(list(trunc(tdiff) * num), units)
  out <- time_add2(end, time_by = time_to_add, time_type = time_type,
                   roll_month = roll_month, roll_dst = roll_dst)
  if (as_int){
    int_end <- time_add2(out, time_by = time_by, time_type = time_type,
                         roll_month = roll_month, roll_dst = roll_dst)
    set_time_cast(out, int_end)
    start <- time_cast(start, out)
    which_out_of_bounds <- cpp_which(double_lt(time_as_number(int_end),
                                               time_as_number(start)))
    int_end[which_out_of_bounds] <- start[which_out_of_bounds]
    out <- structure(out,
                     end = int_end,
                     direction = "right-to-left")
  }
  out
  # class = c("time_int", class(out)))
}
time_int_end <- function(x){
  attr(x, "end")
}
time_int_rm_attrs <- function(x){
  attr(x, "end") <- NULL
  attr(x, "direction") <- NULL
  x
}
# Time aggregation using expanded sequences from data directly
time_aggregate_expand <- function(x, time_by, g = NULL,
                                  start = NULL, end = NULL,
                                  time_floor = FALSE,
                                  week_start = getOption("lubridate.week.start", 1),
                                  time_type = getOption("timeplyr.time_type", "auto"),
                                  roll_month = getOption("timeplyr.roll_month", "preday"), roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                                  as_int = TRUE){
  time_by <- time_by_list(time_by)
  num <- time_by_num(time_by)
  units <- time_by_unit(time_by)
  time_na <- na_init(x)
  no_groups <- is.null(g)
  g <- GRP2(g)
  if (no_groups){
    n_groups <- min(1L, length(x))
    group_sizes <- length(x)
    group_starts <- n_groups
  } else {
    n_groups <- GRP_n_groups(g)
    group_sizes <- GRP_group_sizes(g)
    group_starts <- GRP_starts(g)
  }
  if (is.null(start)){
    start <- gmin(x, g = g, na.rm = TRUE)
  } else {
    if (length(start) != length(x)){
      stop("start must be the same length as x")
    }
    start <- time_cast(start, x)
    x[cpp_which(x < start)] <- time_na
  }
  if (is.null(end)){
    end <- gmax(x, g = g, na.rm = TRUE)
  } else {
    if (length(end) != length(x)){
      stop("end must be the same length as x")
    }
    end <- time_cast(end, x)
    x[cpp_which(x > end)] <- time_na
  }
  .start <- start[group_starts]
  .end <- end[group_starts]
  if (time_floor){
    .start <- time_floor2(.start, time_by = time_by, week_start = week_start)
  }
  seq_sizes <- time_seq_sizes(.start, .end, time_by, time_type = time_type)
  time_full <- time_seq_v2(seq_sizes, from = .start,
                           time_by = time_by,
                           time_type = time_type,
                           time_floor = FALSE,
                           week_start = week_start,
                           roll_month = roll_month,
                           roll_dst = roll_dst)
  group_id <- rep.int(seq_len(n_groups), times = seq_sizes)
  # Creating a GRP object from scratch
  g2 <- sorted_group_id_to_GRP(group_id,
                               n_groups = n_groups,
                               group_sizes = seq_sizes)
  x <- time_cast(x, time_full)
  # group_ends <- cumsum(collapse::GRPN(group_id, expand = FALSE))
  if (no_groups){
    out <- cut_time(x, time_full, include_oob = TRUE)
  } else {
    time_list <- collapse::gsplit(time_as_number(x), g = g)
    time_full_list <- collapse::gsplit(time_as_number(time_full), g = g2)
    out <- vector("list", n_groups)
    for (i in seq_len(n_groups)){
      ti <- .bincode(.subset2(time_list, i),
                     breaks = c(.subset2(time_full_list, i), Inf),
                     include.lowest = FALSE, right = FALSE)
      out[[i]] <- .subset(.subset2(time_full_list, i), ti)
    }
    out <- unlist(out, recursive = FALSE, use.names = FALSE)
    out <- collapse::greorder(out, g = g)
  }
  out <- time_cast(out, time_full)
  if (as_int){
    int_end <- time_add2(out, time_by = time_by, time_type = time_type,
                         roll_month = roll_month, roll_dst = roll_dst)
    set_time_cast(out, int_end)
    end <- time_cast(end, out)
    which_out_of_bounds <- cpp_which(double_gt(time_as_number(int_end),
                                               time_as_number(end)))
    int_end[which_out_of_bounds] <- end[which_out_of_bounds]
    out <- structure(out,
                     end = int_end,
                     direction = "left-to-right")
  }
  out
}

# Time aggregation using expanded sequences when:
# * Data is over 100k rows
# * Expansion size is less than 10 million
# * Number of groups are less than 1m
# And using time differencing otherwise
time_aggregate_switch <- function(x, time_by, time_type,
                                  g = NULL,
                                  start = NULL, end = NULL,
                                  time_floor = FALSE,
                                  week_start = getOption("lubridate.week.start", 1),
                                  roll_month = getOption("timeplyr.roll_month", "preday"), roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                                  as_int = TRUE){
  check_is_time_or_num(x)
  time_by <- time_by_list(time_by)
  num <- time_by_num(time_by)
  units <- time_by_unit(time_by)
  check_time_by_length_is_one(time_by)
  time_type <- match_time_type(time_type)
  g <- GRP2(g, return.groups = FALSE)
  if (is.null(g)){
    n_groups <- min(1L, length(x))
  } else {
    n_groups <- GRP_n_groups(g)
  }
  ###
  if (is.null(start)){
    from <- collapse::fmin(x, g = g, use.g.names = FALSE, na.rm = TRUE)
  } else {
    from <- collapse::ffirst(start, g = g, use.g.names = FALSE)
  }
  if (is.null(end)){
    to <- collapse::fmax(x, g = g, use.g.names = FALSE, na.rm = TRUE)
  } else {
    to <- collapse::ffirst(end, g = g, use.g.names = FALSE)
  }
  # Make sure from/to are datetimes if x is datetime
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  time_span_sizes <- time_seq_sizes(from = from, to = to,
                                    time_by = time_by,
                                    time_type = time_type)
  ###
  if (time_type == "auto"){
    time_type <- guess_seq_type(units)
  }
  if (time_type == "period" &&
      n_groups < 1e06 &&
      sum(time_span_sizes) < 1e07 &&
      length(x) > 1e05){
    time_aggregate_expand(x, g = g, time_by = time_by,
                          start = start,
                          end = end,
                          time_type = time_type,
                          roll_month = roll_month,
                          roll_dst = roll_dst,
                          time_floor = time_floor,
                          week_start = week_start,
                          as_int = as_int)
  } else {
    time_aggregate_left(x, g = g, time_by = time_by,
                        start = start,
                        end = end,
                        time_type = time_type,
                        roll_month = roll_month,
                        roll_dst = roll_dst,
                        time_floor = time_floor,
                        week_start = week_start,
                        as_int = as_int)
  }
}
check_is_date <- function(x){
  if (!is_date(x)){
    stop(paste(deparse1(substitute(x)),
               "must be a date"))
  }
}
check_is_datetime <- function(x){
  if (!is_datetime(x)){
    stop(paste(deparse1(substitute(x)),
               "must be a datetime"))
  }
}
check_is_time <- function(x){
  if (!is_time(x)){
    # Alternative and better way for future errors.
    # cli::cli_abort("{.x {x}} must be a date or datetime")
    stop(paste(deparse1(substitute(x)),
                "must be a date or datetime"))
  }
}
check_is_time_or_num <- function(x){
  if (!is_time_or_num(x)){
    stop(paste(deparse1(substitute(x)),
               "must be a date, datetime, or numeric vector"))
  }
}
# Turn date storage into integer
as_int_date <- function(x){
  check_is_date(x)
  out <- as.integer(unclass(x))
  class(out) <- "Date"
  out
}
check_time_not_missing <- function(x){
  if (anyNA(x)){
    stop("time index must not contain NA values")
  }
}
match_time_type <- function(time_type){
  rlang::arg_match0(time_type, c("auto", "duration", "period"))
}
# Turn "days" into "day", etc
plural_unit_to_single <- function(x){
  substr(x, 1L, nchar(x) -1L)
}

# Multiplies a single unit period like days(7) or months(2)
multiply_single_unit_period_by_number <- function(per, num){
  per_list <- time_by_list(per)
  per_list <- time_by_list_convert_weeks_to_days(per_list)
  per_num <- time_by_num(per_list)
  per_unit <- time_by_unit(per_list)
  # per_unit <- plural_unit_to_single(per_unit)
  # if (per_unit == "second"){
  #   per_unit <- ".Data"
  # }
  # recycle <- length(per_num) != length(num)
  # TEMPORARY infinite replacement
  num[is.infinite(num)] <- NA_real_
  per_num <- per_num * num
  per_length <- length(per_num)
  per_num[cpp_which(is.nan(per_num))] <- NA_real_
  other_fill <- numeric(per_length)
  other_fill[cpp_which(is.na(per_num))] <- NA_real_
  switch(
    per_unit,
    years = {
      per@year <- per_num
      # if (recycle){
       per@month <- other_fill
       per@day <- other_fill
       per@hour <- other_fill
       per@minute <- other_fill
       per@.Data <- other_fill
      # }
    },
    months = {
      per@month <- per_num
      # if (recycle){
        per@year <- other_fill
        per@day <- other_fill
        per@hour <- other_fill
        per@minute <- other_fill
        per@.Data <- other_fill
      # }
    },
    days = {
      per@day <- per_num
      # if (recycle){
        per@year <- other_fill
        per@month <- other_fill
        per@hour <- other_fill
        per@minute <- other_fill
        per@.Data <- other_fill
      # }
    },
    hours = {
      per@hour <- per_num
      # if (recycle){
        per@year <- other_fill
        per@month <- other_fill
        per@day <- other_fill
        per@minute <- other_fill
        per@.Data <- other_fill
      # }
    },
    minutes = {
      per@minute <- per_num
      # if (recycle){
        per@year <- other_fill
        per@month <- other_fill
        per@day <- other_fill
        per@hour <- other_fill
        per@.Data <- other_fill
      # }
    },
    seconds = {
      per@.Data <- per_num
      # if (recycle){
        per@year <- other_fill
        per@month <- other_fill
        per@day <- other_fill
        per@hour <- other_fill
        per@minute <- other_fill
      # }
    }
  )
  # attr(per, per_unit) <- per_num
  # slot(per, per_unit) <- per_num
  per
  # period_unit(per_unit)(per_num * num)
}

rep_single_unit_period <- function(per, ...){
  per_list <- time_by_list(per)
  per_list <- time_by_list_convert_weeks_to_days(per_list)
  per_num <- time_by_num(per_list)
  per_unit <- time_by_unit(per_list)
  per_num <- rep(per_num, ...)
  other_fill <- numeric(length(per_num))
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
  up_date <- time_add2(start,
                       # est * per)
                       multiply_single_unit_period_by_number(per, est),
                       time_type = "period")
  while (length(which <- cpp_which(up_date < end))) {
    est[which] <- est[which] + 1
    up_date[which] <- time_add2(up_date[which],
                                # est[which] * per[which])
                                multiply_single_unit_period_by_number(per[which], est[which]),
                                time_type = "period")
  }
  low_date <- up_date
  while (length(which <- cpp_which(low_date > end))) {
    est[which] <- est[which] - 1
    up_date[which] <- low_date[which]
    low_date[which] <- time_add2(start[which],
                                 # est[which] * per[which])
                                 multiply_single_unit_period_by_number(per[which], est[which]),
                                 time_type = "period")
  }
  frac <- strip_attrs(unclass(difftime(end, low_date, units = "secs"))) /
    strip_attrs(unclass(difftime(up_date, low_date, units = "secs")))
  frac[cpp_which(low_date == up_date)] <- 0
  est + frac
}
# Faster method for interval(start, end) / period() when period
# is a single unit period which is very common
divide_interval_by_period2 <- function(start, end, per){
  if (length(start) == 0 || length(end) == 0 || length(per) == 0) {
    return(numeric())
  }
  estimate <- (time_as_number(as_datetime2(end)) -
                 time_as_number(as_datetime2(start)) ) / unit_to_seconds(per)
  max_len <- max(length(start), length(end), length(per))
  timespans <- recycle_args(start, end, length = max_len)
  # Here we make sure to use rep method for lubridate periods
  timespans[[3]] <- rep_single_unit_period(per, length.out = max_len)
  if (num_na(estimate) == 0) {
    adj_dur_est(estimate, timespans[[1]], timespans[[2]], timespans[[3]])
  } else {
    not_nas <- cpp_which(is.na(estimate), invert = TRUE)
    start2 <- timespans[[1]][not_nas]
    end2 <- timespans[[2]][not_nas]
    per2 <- timespans[[3]][not_nas]
    estimate[not_nas] <- adj_dur_est(estimate[not_nas], start2, end2, per2)
    estimate
  }
}
time_by_list_convert_weeks_to_days <- function(time_by){
  out <- time_by
  if (time_by_unit(out) == "weeks"){
    out <- list("days" = as.double(time_by_num(out) * 7))
  }
  out
}
# time_by_as_timechange_period_list <- function(time_by){
#   time_by <- time_by_list(time_by)
#   num <- time_by_num(time_by)
#   unit <- plural_unit_to_single(time_by_unit(time_by))
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
  n_days[cpp_which(m == 2L & lubridate::leap_year(y))] <- 29L
  n_days
}
int_to_per <- function (start, end){
  set_recycle_args(start, end)
  start <- as_datetime2(start)
  end <- time_cast(end, start)
  duration <- time_as_number(end) - time_as_number(start)
  start <- unclass(as.POSIXlt(start))
  end <- unclass(as.POSIXlt(end))
  negs <- duration < 0
  wnegs <- cpp_which(negs)
  wnnegs <- cpp_which(negs, invert = TRUE)
  per <- list()
  for (nm in c("sec", "min", "hour", "mday", "mon", "year")) {
    per[[nm]] <- integer(length(negs))
    per[[nm]][wnegs] <- (start[[nm]] - end[[nm]])[wnegs]
    per[[nm]][wnnegs] <- (end[[nm]] - start[[nm]])[wnnegs]
  }
  names(per) <- c("second", "minute", "hour", "day", "month",
                  "year")
  nsecs <- per$second < 0
  wnsecs <- cpp_which(nsecs)
  per$second[wnsecs] <- 60 + per$second[wnsecs]
  per$minute[wnsecs] <- per$minute[wnsecs] - 1
  per$second[wnegs] <- -per$second[wnegs]
  nmins <- per$minute < 0
  wnmins <- cpp_which(nmins)
  per$minute[wnmins] <- 60 + per$minute[wnmins]
  per$hour[wnmins] <- per$hour[wnmins] - 1
  per$minute[wnegs] <- -per$minute[wnegs]
  nhous <- per$hour < 0
  wnhous <- cpp_which(nhous)
  per$hour[wnhous] <- 24 + per$hour[wnhous]
  per$hour[wnegs] <- -per$hour[wnegs]
  ndays <- !negs & per$day < 0
  wndays <- cpp_which(ndays)
  if (length(wndays) > 0) {
    add_months <- rep.int(-1, sum(ndays, na.rm = TRUE))
    pmonth <- end$mon[wndays]
    pmonth[cpp_which(pmonth == 0)] <- 1
    prev_month_days <- days_in_month(pmonth, end$year[wndays])
    per$day[wndays] <- pmax(prev_month_days - start$mday[wndays],
                            0) + end$mday[wndays]
    per$month[wndays] <- per$month[wndays] + add_months
  }
  ndays <- negs & per$day < 0
  wndays <- cpp_which(ndays)
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
  wnmons <- cpp_which(nmons)
  per$month[wnmons] <- 12 + per$month[wnmons]
  per$year[wnmons] <- per$year[wnmons] - 1
  per$month[wnegs] <- -per$month[wnegs]
  per$year[wnegs] <- -per$year[wnegs]
  per
}
# int_to_per <- function (start, end){
#   set_recycle_args(start, end)
#   start <- as_datetime2(start)
#   end <- time_cast(end, start)
#   duration <- unclass(difftime(end, start, units = "secs"))
#   start <- unclass(as.POSIXlt(start))
#   end <- unclass(as.POSIXlt(end))
#   negs <- duration < 0 & !is.na(duration)
#   wnegs <- cpp_which(negs)
#   wnnegs <- cpp_which(negs, invert = TRUE)
#   per <- list()
#   for (nm in c("sec", "min", "hour", "mday", "mon", "year")) {
#     per[[nm]] <- integer(length(negs))
#     per[[nm]][wnegs] <- (start[[nm]] - end[[nm]])[wnegs]
#     per[[nm]][wnnegs] <- (end[[nm]] - start[[nm]])[wnnegs]
#   }
#   names(per) <- c("second", "minute", "hour", "day", "month",
#                   "year")
#   nsecs <- per$second < 0 & !is.na(per$second)
#   wnsecs <- cpp_which(nsecs)
#   per$second[wnsecs] <- 60 + per$second[wnsecs]
#   per$minute[wnsecs] <- per$minute[wnsecs] - 1
#   per$second[wnegs] <- -per$second[wnegs]
#   nmins <- per$minute < 0 & !is.na(per$minute)
#   wnmins <- cpp_which(nmins)
#   per$minute[wnmins] <- 60 + per$minute[wnmins]
#   per$hour[wnmins] <- per$hour[wnmins] - 1
#   per$minute[wnegs] <- -per$minute[wnegs]
#   nhous <- per$hour < 0 & !is.na(per$hour)
#   wnhous <- cpp_which(nhous)
#   per$hour[wnhous] <- 24 + per$hour[wnhous]
#   per$hour[wnegs] <- -per$hour[wnegs]
#   ndays <- !negs & per$day < 0 & !is.na(per$day)
#   wndays <- cpp_which(ndays)
#   if (length(wndays) > 0) {
#     add_months <- rep.int(-1, sum(ndays))
#     pmonth <- end$mon[wndays]
#     pmonth[cpp_which(pmonth == 0)] <- 1
#     prev_month_days <- days_in_month(pmonth, end$year[wndays])
#     per$day[wndays] <- pmax(prev_month_days - start$mday[wndays],
#                             0) + end$mday[wndays]
#     per$month[wndays] <- per$month[wndays] + add_months
#   }
#   ndays <- negs & per$day < 0 & !is.na(per$day)
#   wndays <- cpp_which(ndays)
#   if (length(wndays) > 0) {
#     add_months <- rep.int(1L, sum(ndays))
#     this_month_days <- days_in_month(end$mon[wndays] + 1,
#                                      end$year[wndays])
#     per$day[wndays] <- pmax(this_month_days - end$mday[wndays],
#                             0) + start$mday[wndays]
#     per$month[wndays] <- per$month[wndays] - add_months
#   }
#   per$day[wnhous] <- per$day[wnhous] - 1
#   per$day[wnegs] <- -per$day[wnegs]
#   nmons <- per$month < 0 & !is.na(per$month)
#   wnmons <- cpp_which(nmons)
#   per$month[wnmons] <- 12 + per$month[wnmons]
#   per$year[wnmons] <- per$year[wnmons] - 1
#   per$month[wnegs] <- -per$month[wnegs]
#   per$year[wnegs] <- -per$year[wnegs]
#   per
# }
