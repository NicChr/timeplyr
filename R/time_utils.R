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
      paste0(substr(units, 1L, nchar(units) -1L))
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
# Calculates greatest common divisor of
# rolling time difference, taking into account
# time type
####### GROUPED TIME DIFF GCD
####### IN PROGRESS
# time_diff_gcd2 <- function(x, g = NULL, use.g.names = TRUE,
#                           time_type = c("auto", "duration", "period"),
#                           is_sorted = FALSE,
#                           tol = sqrt(.Machine$double.eps)){
#   if (!is.null(g)){
#     g <- GRP2(g)
#     check_data_GRP_size(x, g)
#   }
#   x <- gunique(unname(x), g = g, sort = TRUE, use.g.names = TRUE)
#   # g <- collapse::group(names(x), starts = TRUE, group.sizes = TRUE)
#   is_na <- is.na(x)
#   x <- x[!is_na]
#   # g <- g[!is_na]
#   if (length(x) == 0L){
#     gcd_diff <- numeric(0)
#   } else if (length(x) == 1L){
#     gcd_diff <- 1
#   } else {
#     if (is_date(x)){
#       tby <- "days"
#     } else if (is_datetime(x)){
#       tby <- "seconds"
#     } else {
#       tby <- 1
#     }
#     tdiff <- time_elapsed(x, rolling = TRUE,
#                           g = g,
#                           time_by = tby,
#                           time_type = time_type,
#                           fill = 1,
#                           na_skip = FALSE)
#     names(tdiff) <- names(x)
#     log10_tol <- floor(abs(log10(tol)))
#     # g <- g[-1L]
#     tdiff <- round(abs(tdiff[-1L]), digits = log10_tol)
#     # g <- g[double_gt(tdiff, 0)]
#     tdiff <- tdiff[double_gt(tdiff, 0)]
#     # group_starts <- attr(g, "starts")
#     # group_sizes <- attr(g, "group.sizes")
#     # group_ends <- group_starts + group_sizes - 1L
#     # group_ends <- pmax(group_ends, 0L)
#     #
#     # for (i in seq_along(group_starts)){
#     #
#     # }
#     gcd_diff <- vapply(collapse::gsplit(tdiff, g = names(tdiff)),
#                        collapse::vgcd, numeric(1))
#   }
#   gcd_diff
# }
# Calculates time granularity with numeric tolerance
# time_diff_gcd <- function(x, is_sorted = FALSE,
#                           tol = sqrt(.Machine$double.eps)){
#   x <- as.double(x)
#   x <- collapse::funique(x, sort = !is_sorted)
#   N <- length(x)
#   x <- x[!is.na(x)]
#   if (N == 0L){
#     gcd_diff <- numeric(0)
#   } else if (N < 2){
#     gcd_diff <- 1
#   } else {
#     y_diff <- collapse::fdiff(x,
#                               n = 1, diff = 1, g = NULL,
#                               fill = NA, log = FALSE, rho = 1,
#                               stubs = FALSE)
#     log10_tol <- floor(abs(log10(tol)))
#     y_diff <- round(abs(y_diff[-1L]), digits = log10_tol)
#     y_diff <- collapse::funique(y_diff, sort = FALSE)
#     y_diff <- y_diff[double_gt(y_diff, 0)]
#     if (length(y_diff) == 0L){
#       gcd_diff <- 10^(-log10_tol)
#     } else {
#       gcd_diff <- collapse::vgcd(y_diff)
#     }
#     # gcd_diff <- Reduce(gcd, y_diff)
#   }
#   gcd_diff
# }
time_granularity <- function(x, is_sorted = FALSE, msg = TRUE){
  gcd_diff <- time_diff_gcd(x, is_sorted = is_sorted)
  if (is_date(x)){
    granularity <- "day(s)"
    scale <- 1
    unit <- "days"
    num_and_unit <- paste(gcd_diff, unit, sep = " ")
  } else if (is_datetime(x)){
    convert_seconds <- seconds_to_unit(gcd_diff)
    scale <- convert_seconds[["scale"]]
    granularity <- convert_seconds[["unit"]]
    granularity <- paste0(substr(granularity, 1L, nchar(granularity) -1L),
                          "(s)")
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
  } else if (is.character(x) || is.numeric(x)){
    x2 <- as.character(x)
    out <- lubridate::ymd(x2, quiet = TRUE)
    if (sum(is.na(out[!is.na(x2)])) > 0) out <- lubridate::dmy(x2, quiet = TRUE)
    n_na <- sum(is.na(out[!is.na(x2)]))
    if (n_na < length(x) && n_na > 0) out <- rep_len(lubridate::NA_Date_, length(x))
  } else {
    out <- rep_len(lubridate::NA_Date_, length(x))
  }
  out
}
# Calculate size of period unit to expand from and to for specified length
period_by_calc <- function(from, to, length){
  seconds_unit <- period_unit("seconds")
  recycled_args <- recycle_args(from, to, length)
  from <- recycled_args[[1L]]
  to <- recycled_args[[2L]]
  length <- recycled_args[[3L]]
  which_len_1 <- which(length == 1)
  sec_diff <- time_diff(from, to,
                        time_by = list("seconds" = 1),
                        time_type = "period")
  out <- lubridate::seconds_to_period(sec_diff / (length - 1))
  period_info <- collapse::qDF(time_unit_info(out))
  n_unique_slots <- ncol(period_info) - rowSums(period_info == 0)
  which_multi <- which(n_unique_slots > 1)
  out[which_multi] <- seconds_unit(
    lubridate::period_to_seconds(out[which_multi])
  )
  out[which_len_1] <- seconds_unit(0)
  out
}
# Unvectorised version
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
  out[length == 1] <- seconds_unit(0) # Special case
  out
}
num_by_calc <- function(from, to, length){
  out <- (to - from) / (length - 1)
  length <- rep_len(length, length(out))
  out[length == 1] <- 0
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
    keep <- vapply(sum_rng, function(x) isTRUE(double_gt(x, 0)), logical(1))
    if (sum(keep) == 0){
      out["second"]
    } else {
      out[keep]
    }
    # if (!any(keep, na.rm = TRUE)){
    #   out["second"]
    # } else {
    #   out[keep]
    # }
    # periods <- c("second",
    #              "minute",
    #              "hour",
    #              "day",
    #              "month",
    #              "year")
    # out <- add_names(vector("list", length(periods)),
    #                 periods)
    # keep <- logical(length(periods))
    # for (i in seq_along(periods)){
    #   out[[i]] <- do.call(get(periods[[i]],
    #                           asNamespace("lubridate")),
    #                       list(time_unit))
    #   keep[[i]] <- sum(abs(out[[i]])) > 0
    # }
    # out[keep]
    # tattr <- attributes(time_unit)
    # lubridate_units <- intersect(names(tattr), substr(.period_units, 1L, nchar(.period_units) -1L))
    # tattr <- tattr[lubridate_units]
    # m <- matrix(unlist(tattr, use.names = FALSE), nrow = length(time_unit),
    #             ncol = length(lubridate_units), byrow = FALSE)
    # m <- cbind(lubridate::second(time_unit), m)
    # colnames(m) <- c("second", lubridate_units)
    # which_zero <- which(rowSums(m) == 0)
    # # Replace empty rows with one temporarily..
    # m[which_zero, 1] <- 1
    # keep <- which(t(m) != 0) %% 6
    # keep[keep == 0] <- 6 # If exactly divisible, then these are years.
    # # Replace the zero rows with zeroes again
    # m[which_zero, 1] <- 0
    # keep[is.na(keep)] <- 1L # Keep seconds if NA
    # keep_m <- matrix(c(seq_len(nrow(m)), keep),
    #                  byrow = FALSE, ncol = 2L)
    # out <- m[keep_m]
    # out_nms <- c("second", lubridate_units)[keep]
    # out <- add_names(as.list(out), out_nms)
    # out
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
# Coerce pair of dates/datetimes to the most informative
# class between them
set_time_cast <- function(x, y){
  if (!isTRUE(class(x) == class(y))){
    if (is_date(x) && is_datetime(y)){
      x_nm <- deparse(substitute(x))
      assign(x_nm, lubridate::with_tz(.POSIXct(unclass(x) * 86400),
                                      tzone = lubridate::tz(y)),
             envir = parent.frame(n = 1))

    }
    if (is_date(y) && is_datetime(x)){
      y_nm <- deparse(substitute(y))
      assign(y_nm, lubridate::with_tz(.POSIXct(unclass(y) * 86400),
                                      tzone = lubridate::tz(x)),
             envir = parent.frame(n = 1))
    }
  }
}

# Safe time concatenation
time_c <- function(...){
  vctrs::vec_c(...)
}
# This coerces x into template class if template class is more granular
time_cast <- function(x, template){
  if (is.null(x) || is.null(template)){
    return(x)
  }
  x_dttm <- is_datetime(x)
  temp_dttm <- is_datetime(template)
  if (x_dttm){
    x_tz <- lubridate::tz(x)
  } else {
    x_tz <- ""
  }
  if (temp_dttm){
    temp_tz <- lubridate::tz(template)
  } else {
    temp_tz <- ""
  }
  if (identical(class(x), class(template)) &&
      identical(x_tz, temp_tz)){
    return(x)
  }
  if (temp_dttm && !x_dttm){
    lubridate::with_tz(lubridate::as_datetime(x), tzone = temp_tz)
  } else if (x_dttm && temp_dttm){
    if (!isTRUE(x_tz == temp_tz)){
      lubridate::with_tz(x, tzone = temp_tz)
    } else {
      x
    }
  } else if (is_date(template) && !is_date(x) && !x_dttm){
    lubridate::as_date(x)
  }
  else if (inherits(template, "yearmon")){
    as_yearmon(x)
  } else if (inherits(template, "yearqtr")){
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
  setv(breaksi, 0L, length(breaks) + 1L, vind1 = FALSE)
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
cut_time3 <- function(x, breaks, right = FALSE, include.lowest = FALSE){
  bin_codes <- .bincode(x, breaks = breaks,
                        right = right, include.lowest = include.lowest)
  breaks[bin_codes]
}
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
rep.int.yearmon <- function(x, times){
  x[rep.int(seq_along(x), times = times)]
}
rep.yearmon <- function(x, ...){
  x[rep(seq_along(x), ...)]
}
rep_len.yearqtr <- function(x, length.out){
  x[rep_len(seq_along(x), length.out = length.out)]
}
rep.int.yearqtr <- function(x, times){
  x[rep.int(seq_along(x), times = times)]
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
  inherits(x, "Interval")
}
# Check if data has lubridate interval
has_interval <- function(data, quiet = TRUE){
  out <- FALSE
  for (i in seq_along(data)){
    if (is_interval(.subset2(data, i))){
      out <- TRUE
      break
    }
  }
  if (out && !quiet){
    message("A variable of class 'Interval' exists.
    The grouping will be done using 'dplyr'.
            See https://github.com/SebKrantz/collapse/issues/418")
  }
  out
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
  n <- length(x)
  out <- time_interval(seq, flag2(seq, n = max(-1L, -n), g = gseq))
  to <- collapse::fmax(x, g = gx, use.g.names = FALSE, na.rm = TRUE)
  end_points <- which(is.na(out) & !is.na(seq))
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
  end_points <- which(is.na(out) & !is.na(seq))
  setv(out, end_points, stringr::str_c("[",
                                       time_breaks_fmt[end_points],
                                       ", ",
                                       fmt_f(to),
                                       "]"),
       vind1 = TRUE)
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
C_time_add <- getFromNamespace("C_time_add", "timechange")
time_add2 <- function(x, time_by,
                      time_type = c("auto", "duration", "period"),
                      roll_month = "preday", roll_dst = "pre"){
  time_by <- time_by_list(time_by)
  time_num <- time_by_num(time_by)
  time_unit <- time_by_unit(time_by)
  if (time_by_is_num(time_by)){
    x + time_num
  } else {
    time_type <- rlang::arg_match0(time_type,
                                   c("auto", "duration", "period"))
    if (time_type == "auto"){
      time_type <- guess_seq_type(time_unit)
    }
    if (time_type == "period"){
      unit <- substr(time_unit, 1L, nchar(time_unit) -1L)
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
  Sys.Date() + 1L
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
  # is.na(.subset2(X, 1L)) & is.na(.subset2(X, 2L))
  is.na(X[[1L]]) & is.na(X[[2L]])
}
check_is_time_or_num <- function(x){
  if (!is_time_or_num(x)){
    stop("x must be a date, datetime, or numeric vector")
  }
}
time_as_number <- function(x){
  strip_attrs(unclass(x))
}
time_aggregate_left <- function(x, time_by, g = NULL,
                                start = NULL, end = NULL,
                                time_floor = FALSE,
                                week_start = getOption("lubridate.week.start", 1),
                                time_type = c("auto", "duration", "period"),
                                roll_month = "preday", roll_dst = "pre",
                                as_int = TRUE){
  time_by <- time_by_list(time_by)
  num <- time_by_num(time_by)
  units <- time_by_unit(time_by)
  time_na <- x[NA_integer_]
  g <- GRP2(g, return.groups = FALSE)
  if (!is.null(start)){
    if (length(start) != length(x)){
      stop("start must be the same length as x")
    }
    start <- time_cast(start, x)
    x[x < start] <- time_na
  } else {
    start <- gmin(x, g = g, na.rm = TRUE)
  }
  if (!is.null(end)){
    if (length(end) != length(x)){
      stop("end must be the same length as x")
    }
    end <- time_cast(end, x)
    x[x > end] <- time_na
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
    which_out_of_bounds <- which(double_gt(time_as_number(int_end),
                                           time_as_number(end)))
    int_end[which_out_of_bounds] <- end[which_out_of_bounds]
    out <- structure(out,
                     end = int_end,
                     direction = "left-to-right")
  }
  out
}
time_aggregate_right <- function(x, time_by, g = NULL,
                                 start = NULL, end = NULL,
                                 time_ceiling = FALSE,
                                 week_start = getOption("lubridate.week.start", 1),
                                 time_type = c("auto", "duration", "period"),
                                 roll_month = "preday", roll_dst = "pre",
                                 as_int = TRUE){
  time_by <- time_by_list(time_by)
  num <- time_by_num(time_by)
  units <- time_by_unit(time_by)
  time_na <- x[NA_integer_]
  g <- GRP2(g, return.groups = FALSE)
  if (!is.null(start)){
    if (length(start) != length(x)){
      stop("start must be the same length as x")
    }
    start <- time_cast(start, x)
    x[x < start] <- time_na
  } else {
    start <- gmin(x, g = g, na.rm = TRUE)
  }
  if (!is.null(end)){
    if (length(end) != length(x)){
      stop("end must be the same length as x")
    }
    end <- time_cast(end, x)
    x[x > end] <- time_na
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
    which_out_of_bounds <- which(double_lt(time_as_number(int_end),
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
                                  time_type = c("auto", "duration", "period"),
                                  roll_month = "preday", roll_dst = "pre",
                                  as_int = TRUE){
  time_by <- time_by_list(time_by)
  num <- time_by_num(time_by)
  units <- time_by_unit(time_by)
  time_na <- x[NA_integer_]
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
    x[x < start] <- time_na
  }
  if (is.null(end)){
    end <- gmax(x, g = g, na.rm = TRUE)
  } else {
    if (length(end) != length(x)){
      stop("end must be the same length as x")
    }
    end <- time_cast(end, x)
    x[x > end] <- time_na
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
  # group_ends <- cumsum(collapse::GRPN(group_id, expand = FALSE))
  if (no_groups){
    out <- cut_time2(time_cast(x, time_full), time_full)
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
    out <- time_cast(out, time_full)
  }
  if (as_int){
    int_end <- time_add2(out, time_by = time_by, time_type = time_type,
                         roll_month = roll_month, roll_dst = roll_dst)
    set_time_cast(out, int_end)
    which_out_of_bounds <- which(double_gt(time_as_number(int_end),
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
                                  roll_month = "preday", roll_dst = "pre",
                                  as_int = TRUE){
  check_is_time_or_num(x)
  time_by <- time_by_list(time_by)
  num <- time_by_num(time_by)
  units <- time_by_unit(time_by)
  check_time_by_length_is_one(time_by)
  time_type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
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
    from <- ffirst(start, g = g, use.g.names = FALSE)
  }
  if (is.null(end)){
    to <- collapse::fmax(x, g = g, use.g.names = FALSE, na.rm = TRUE)
  } else {
    to <- ffirst(end, g = g, use.g.names = FALSE)
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
                          as_int = as_int)
  } else {
    time_aggregate_left(x, g = g, time_by = time_by,
                        start = start,
                        end = end,
                        time_type = time_type,
                        roll_month = roll_month,
                        roll_dst = roll_dst,
                        time_floor = time_floor,
                        as_int = as_int)
  }
}
check_is_date <- function(x){
  if (!is_date(x)){
    stop("x must be a date")
  }
}
# Turn date storage into integer
as_int_date <- function(x){
  check_is_date(x)
  `class<-`(as.integer(unclass(x)), "Date")
}
check_time_not_missing <- function(x){
  if (anyNA(x)){
    stop("time index must not contain NA values")
  }
}
