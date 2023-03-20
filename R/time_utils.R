# Time utility functions
# Error for the user to see available units
unit_match_stop <- function(units = .time_units){
  stop(paste0("'arg' should be one of '", paste(units, collapse = "', '"), "'"))
}
# The idea is that when you supply a list, no regex
# is needed to seperate out the numbers
# This is handy with loops to reduce overhead
unit_list_match <- function(l){
  stopifnot(length(l) == 1L)
  unit <- unit_match(names(l))
  if (names(l) == "numeric") {
    unit <- "numeric"
  }
  if (is.na(unit)) unit_match_stop()
  if (length(unit) == 0L) stop("unit list must be named")
  num <- l[[1L]]
  scale <- 1
  if (unit %in% .extra_time_units){
    exotic_info <- convert_exotic_units(unit)
    scale <- exotic_info[["scale"]]
    unit <- exotic_info[["unit"]]
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
  scale <- scales[pmatch(x, .extra_time_units,
                         nomatch = NA_real_,
                         duplicates.ok = FALSE)]
  units <- base_units[pmatch(x, .extra_time_units,
                             nomatch = NA_character_,
                             duplicates.ok = FALSE)]
  list("unit" = units,
       "scale" = scale)
}
# Partial unit matching
unit_match <- function(x){
  stopifnot(length(x) == 1L)
  # Normal lubridate units
  match_index1 <- pmatch(x, .duration_units,
                  nomatch = NA_character_,
                  duplicates.ok = FALSE)
  if (is.na(match_index1)){
    # Exotic units
    match_index2 <- pmatch(x, .extra_time_units,
                           nomatch = NA_character_,
                           duplicates.ok = FALSE)
    .extra_time_units[match_index2]

  } else {
    .duration_units[match_index1]
  }
}
# Unit string parsing
unit_parse <- function(x){
  # Extract numbers from string
  # Try decimal numbers
  eval_num <- FALSE # Flag to use eval(parse())
  num_str <- regmatches(x, m = regexpr(pattern = ".*[[:digit:]]*\\.[[:digit:]]+",
                                        text = x))
  # Try strings like n/m
  if (length(num_str) == 0L){
    num_str <- regmatches(x, m = regexpr(pattern = ".*[[:digit:]]+\\/[[:digit:]]+",
                                         text = x))
    eval_num <- TRUE
  }
  # Try strings like n*m
  if (length(num_str) == 0L){
    num_str <- regmatches(x, m = regexpr(pattern = ".*[[:digit:]]+\\*[[:digit:]]+",
                                         text = x))
    eval_num <- TRUE
  }
  # If not try regular numbers
  if (length(num_str) == 0L){
    num_str <- regmatches(x, m = regexpr(pattern = ".*[[:digit:]]+",
                                         text = x))
    eval_num <- FALSE
  }
  if (eval_num){
    num <- eval(parse(text = num_str))
  } else {
    num <- as.numeric(num_str)
  }
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
    scale <- exotic_info[["scale"]]
    unit <- exotic_info[["unit"]]
  }
  out <- list("unit" = unit,
              "num" = num,
              "scale" = scale)
  out
}
# This function brings together all the above unit_ small helpers
# It tries to first match the unit to an existing unit
# If it can't, then it uses regex to separate the numbers
# from the words
# And also scales exotic units like quarters appropriately
unit_guess <- function(x){
  # If numeric then just return this..
  if (is.numeric(x)){
    out <- list("unit" = "numeric",
                "num" = x,
                "scale" = 1)
  } else if (is.list(x)){
    # If it's a list, string match but no parse
    out <- unit_list_match(x)
  } else {
    # Try matching first as it's faster
    unit <- unit_match(x)
    # If that doesn't work finally try parsing
    if (is.na(unit)){
      out <- unit_parse(x)
    } else {
      num <- 1
      scale <- 1
      # If the unit is something exotic,
      # The num needs to be scaled correctly
      if (unit %in% .extra_time_units){
        if (unit %in% .extra_time_units){
          exotic_info <- convert_exotic_units(unit)
          scale <- exotic_info[["scale"]]
          unit <- exotic_info[["unit"]]
        }
      }
      # num <- num * scale
      out <- list("unit" = unit,
                  "num" = num,
                  "scale" = scale)
    }
  }
  out
}
# Creates interval even using num
time_interval <- function(from, to){
  if (is_time(from) || is_time(to)){
    out <- lubridate::interval(from, to)
  } else {
    labels <- paste(from, to, sep = " to ")
    from <- rep_len(from, length(labels))
    to <- rep_len(to, length(labels))
    out <- labels
    attr(out, "starts") <- from
    attr(out, "ends") <- to
  }
  out
}
# Time interval from ascending x
time_interval3 <- function(x, to = NULL, is_sorted = FALSE){
  time_breaks <- collapse::funique(collapse::na_rm(x), sort = !is_sorted)
  if (is.null(to)) to <- collapse::fmax(time_breaks, na.rm = TRUE,
                                        use.g.names = FALSE)
  time_int <- time_interval(time_breaks, collapse::flag(time_breaks, n = -1))
  time_int[length(time_int)] <- time_interval(time_breaks[length(time_breaks)], to)
  time_start <- cut_time2(x, breaks = c(time_breaks, to + 1))
  time_int[match(time_start, time_breaks)]
}
# Time interval from ascending time sequence
# These 2 functions are weird as to must match the length of the unique groups, not x..
# also x must be in ascending order
time_seq_interval <- function(x, to, g = NULL){
  n <- length(x)
  out <- time_interval(x, collapse::flag(x, n = -1L, g = g))
  if (!is.null(g)){
    # Use time seq df to get time intervals
    end_points <- which(is.na(out) & !is.na(x))
    # Use data end-points to create the rightmost intervals of each group
    out[end_points] <- time_interval(x[end_points], to)
  } else {
    out[n] <- time_interval(x[n], to)
  }
  out
}
# Time cut levels from ascending time sequence
time_seq_levels <- function(x, to, g = NULL, fmt = NULL){
  if (is.null(fmt)){
    fmt_f <- identity
  } else {
    fmt_f <- function(x, ...) format(x, fmt, ...)
  }
  n <- length(x)
  time_breaks_fmt <- fmt_f(x)
  out <- stringr::str_c("[",
                        time_breaks_fmt,
                        ", ",
                        collapse::flag(time_breaks_fmt,
                                       g = g, n = -1L),
                        ")")
  if (!is.null(g)){
    # Use time seq df to get time intervals
    end_points <- which(is.na(out) & !is.na(x))
    # Use data end-points to create the rightmost intervals of each group
    out[end_points] <- stringr::str_c("[",
                              time_breaks_fmt[end_points],
                              ", ",
                              fmt_f(to),
                              "]")
  } else {
    out[[n]] <- stringr::str_c("[",
                       time_breaks_fmt[[n]],
                       ", ",
                       fmt_f(to),
                       "]")

  }
  out
}
# Calculates time granularity
time_diff_gcd <- function(x, is_sorted = FALSE){
  x <- as.double(x)
  x <- collapse::funique(x, sort = !is_sorted)
  if (length(x) < 2){
    gcd_diff <- 1
  } else {
    y_diff <- collapse::fdiff(x,
                              n = 1, diff = 1, g = NULL,
                              fill = NA, log = FALSE, rho = 1,
                              stubs = FALSE)
    y_diff <- collapse::na_rm(collapse::funique(round(abs(y_diff[-1L])),
                                                        sort = FALSE))
    # gcd_diff <- Reduce(gcd, y_diff)
    gcd_diff <- collapse::vgcd(y_diff)
    if (!isTRUE(gcd_diff >= 1)) gcd_diff <- 1
    gcd_diff
  }
}
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
    unit <- "seconds"
    num_and_unit <- paste(gcd_diff, unit, sep = " ")
  } else {
    granularity <- "numeric unit(s)"
    scale <- 1
    unit <- "numeric"
    num_and_unit <- gcd_diff
  }
  if (msg) message(paste("Assuming a time granularity of", gcd_diff/scale, granularity, sep = " "))
  list("granularity" = granularity,
       "unit" = unit,
       "num" = gcd_diff,
       "num_and_unit" = num_and_unit,
       "scale" = scale)
}
# Converts seconds to duration unit
# Scale is in comparison to seconds
seconds_to_unit <- function(x){
  if (x >= 0 && x < 1/1000/1000/1000){
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
    unit <- "minute(s)"
    scale <- 60
  } else if (x >= 3600 && x < 86400){
    unit <- "hour(s)"
    scale <- 3600
  } else if (x >= 86400 && x < 604800){
    unit <- "day(s)"
    scale <- 86400
  } else if (x >= 604800 && x < 2629800){
    unit <- "week(s)"
    scale <- 604800
  } else if (x >= 2629800 && x < 31557600){
    unit <- "month(s)"
    scale <- 2629800
  } else if (x >= 31557600){
    unit <- "year(s)"
    scale <- 31557600
  }
  list("unit" = unit,
       "scale" = scale)
}
# No string guessing at all
guess_seq_type <- function(units){
  if (units %in% c("days", "weeks", "months", "years")){
    "period"
  } else {
    "duration"
  }
}
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
# Calculate size of period unit to expand from and to to get a specified length
# Similar to how base::seq() calculates by when from, to, and length.out are specified
# period_by <- function(from, to, length){
#   seconds_unit <- period_unit("seconds")
#   recycled_args <- recycle_args(from, to, length)
#   from <- recycled_args[[1L]]
#   to <- recycled_args[[2L]]
#   length <- recycled_args[[3L]]
#   which_len_1 <- which(length == 1)
#   int <- lubridate::interval(from, to)
#   # remainder_m <- matrix(numeric(length(from) * length(.period_units)),
#   #                       nrow = length(from), ncol = length(.period_units))
#   # division_m <- remainder_m
#   # for (i in seq_along(.period_units)){
#   #   j <- length(.period_units) - i + 1L
#   #   division <- int / period_unit(.period_units[[j]])(1)
#   #   division_m[, j] <- division
#   #   remainder_m[, j] <- division %% 1
#   # }
#   # Which period units to keep
#   # keep <- pmax(rowSums(remainder_m == 0 & division_m > 0), 1)
#   # division <- int / .period_units[keep]
#   # out <- lubridate::seconds_to_period(
#   #   seconds_unit( (division / (length - 1)) )
#   # )
#   division <- int / seconds_unit(1)
#   out <- seconds_unit( (division / (length - 1)) )
#   # out <- lubridate::seconds_to_period(
#   #   seconds_unit( (division / (length - 1)) )
#   # )
#   out[which_len_1] <- seconds_unit(0)
#   out
# }
# Unvectorised version
period_by <- function(from, to, length){
  if (length == 1){
    lubridate::seconds(0)
  } else {
    int <- lubridate::interval(from, to)
    periods_to_try <- rev(.period_units)
    for (i in seq_along(periods_to_try)){
      unit <- period_unit(periods_to_try[[i]])
      division <- int / unit(1)
      # remainder <- division %% 1
      remainder <- ( (division / (length - 1)) %% 1 )
      if (abs(division) >= 1 &&
          remainder == 0){
        out <- unit( (division / (length - 1)) )
        return(out)
      }
    }
    unit( (division / (length - 1)) )
  }
}
# Calculates size of duration to cut a pre-specified interval of
# a certain length
duration_by <- function(from, to, length){
  int <- lubridate::interval(from, to)
  seconds_unit <- duration_unit("seconds")
  division <- int / seconds_unit(1)
  out <- seconds_unit(division / (length - 1))
  out[length == 1] <- seconds_unit(0) # Special case
  out
}
num_by <- function(from, to, length){
  out <- (to - from) / (length - 1)
  out[length == 1] <- 0
  out
}
# Vectorized except for periods
time_by <- function(from, to, length, seq_type){
  if (is_time(from) && is_time(to)){
    if (seq_type == "period"){
      period_by(from, to, length)
    } else {
      duration_by(from, to, length)
    }
  } else {
    num_by(from, to, length)
  }
}
# This only works for single unit vectors
# Periods with multiple types of units do not work.
time_unit_info <- function(time_unit){
  tclass <- class(time_unit)
  if (tclass == "Duration"){
    setnames(as.list(as.double(time_unit)),
             rep_len("second", length(time_unit)))
  } else if (tclass == "Period"){
    tattr <- attributes(time_unit)
    lubridate_units <- intersect(names(tattr), substr(.period_units, 1L, nchar(.period_units) -1L))
    tattr <- tattr[lubridate_units]
    m <- matrix(unlist(tattr, use.names = FALSE), nrow = length(time_unit),
                ncol = length(lubridate_units), byrow = FALSE)
    m <- cbind(lubridate::second(time_unit), m)
    colnames(m) <- c("second", lubridate_units)
    which_zero <- which(rowSums(m) == 0)
    # Replace empty rows with one temporarily..
    m[which_zero, 1] <- 1
    keep <- which(t(m) != 0) %% 6
    keep[keep == 0] <- 6 # If exactly divisible, then these are years.
    # Replace the zero rows with zeroes again
    m[which_zero, 1] <- 0
    keep[is.na(keep)] <- 1L # Keep seconds if NA
    keep_m <- matrix(c(seq_len(nrow(m)), keep),
                     byrow = FALSE, ncol = 2L)
    out <- m[keep_m]
    out_nms <- c("second", lubridate_units)[keep]
    out <- setnames(as.list(out), out_nms)
    out
  } else {
    setnames(as.list(as.double(time_unit)),
             rep_len("numeric", length(time_unit)))
  }
}
is_date_or_utc <- function(...){
  x <- vctrs::vec_c(...)
  is_date(x) || lubridate::tz(x) == "UTC"
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
time_unit <- function(units, type = c("duration", "period")){
  if (units == "numeric"){
    numeric_unit(units)
  } else {
    type <- match.arg(type)
    if (type == "duration"){
      duration_unit(units)
    } else {
      period_unit(units)
    }
  }
}
# Safe time concatenation
time_c2 <- function(x, y){
  if (isTRUE(all.equal(attributes(x), attributes(y)))){
    list(x, y)
  } else {
    common_type <- vctrs::vec_ptype2(x, y)
    if (is_datetime(common_type)){
      x <- time_cast(x, common_type)
      y <- time_cast(y, common_type)
    }
    list(x, y)
  }
}
# Safe time concatenation
time_c <- function(...){
  vctrs::vec_c(...)
}
# This turns x into datetime if template is datetime
# and x into date is template is date (but x is not datetime)
# and coerces timezone of x to template timezone
time_cast <- function(x, template){
  stopifnot(is_time_or_num(x))
  if (is_datetime(template) && !is_datetime(x)){
    lubridate::with_tz(lubridate::as_datetime(x),
                       tzone = lubridate::tz(template))
  } else if (is_datetime(x) && is_datetime(template)){
    lubridate::with_tz(x, tzone = lubridate::tz(template))
  } else if (is_date(template) && !is_date(x) && !is_datetime(x)){
    lubridate::as_date(x)
  }
  else {
    x
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
    from <- collapse::fmax(c(x_min, from),
                           use.g.names = FALSE)
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
    to <- collapse::fmin(c(x_max, to),
                         use.g.names = FALSE)
  }
  to
}
# Get rolling window sizes, including partial
window_seq <- function(k, n, partial = TRUE){
  stopifnot(length(k) == 1L,
            length(n) == 1L)
  if (n > .Machine[["integer.max"]]) stop("n must not be greater than .Machine$integer.max")
  n <- as.integer(n)
  k <- as.integer(min(k, n))
  k <- max(k, 0L)
  partial_len <- max(min(k - 1L, n), 0L)
  other_len <- max(0L, n - partial_len)
  if (partial){
    c(seq_len(partial_len), collapse::alloc(k, other_len))
  } else {
    c(collapse::alloc(NA_integer_, partial_len), collapse::alloc(k, other_len))
  }
}
# time_seq_data must be sorted by groups + time
# data must also be sorted the same way
time_cut_grouped <- function(time_seq_data, data, time, group_id,
                             to){
  time_seq_num_max <- as.double(to)
  time_seq_list <- collapse::gsplit(time_seq_data[[time]],
                                    g = time_seq_data[[group_id]])
  time_list <- collapse::gsplit(data[[time]],
                                g = data[[group_id]])
  # Change time to numbers
  time_seq_list_num <- lapply(time_seq_list, as.double)
  time_list_num <- lapply(time_list, as.double)
  # Pre-allocate list of aggregated time
  time_agg_list <- vector("list", length(time_seq_list))
  breaks_list <- vector("list", length(time_seq_list))
  seq_lengths <- collapse::vlengths(time_seq_list)
  # This loops through each group and aggregates time based on
  # The sequences within each group
  for (i in seq_along(time_seq_list)){
    # print(i)
    .breaks <- fcut_ind(time_list_num[[i]],
                        c(time_seq_list_num[[i]],
                          time_seq_num_max[i] + 1))
    breaks_list[[i]] <- .breaks
    time_agg_list[[i]] <- time_seq_list[[i]][.breaks]
  }
  time_agg_v <- unlist(time_agg_list, recursive = FALSE, use.names = FALSE)
  time_agg_v <- time_cast(time_agg_v, data[[time]])
  setnames(list(time_agg_v,
       unlist(breaks_list, recursive = FALSE, use.names = FALSE),
       vctrs::vec_rep_each(seq_len(length(time_seq_list)), collapse::vlengths(time_agg_list))),
       c(time, ".breaks", group_id))
}
time_agg <- function(time_seq_data, data, time, group_id, to = NULL,
                     include_interval = FALSE){
  time_seq_data <- collapse::funique(time_seq_data,
                                     cols = c(group_id, time),
                                     sort = FALSE)
  # Create vector defining end-points
  if (length(to) == 0L){
    time_seq_max <- collapse::fmax(data[[time]], g = data[[group_id]],
                                   use.g.names = FALSE)
  } else {
    time_seq_max <- collapse::ffirst(data[[to]], g = data[[group_id]],
                                     use.g.names = FALSE)
    time_seq_max <- time_cast(time_seq_max, data[[time]])
    # time_seq_max <- rep_len(to, length(time_list))
  }
  time_seq_num_max <- as.double(time_seq_max)
  out <- dplyr::as_tibble(
    time_cut_grouped(time_seq_data, data, time = time,
                                  group_id = group_id, to = time_seq_max)
    )

  if (include_interval){
    time_int <- time_seq_interval(time_seq_data[[time]], to = time_seq_max,
                                  g = time_seq_data[[group_id]])
    # int_nm <- new_var_nm(data, "interval")
    int_df <- dplyr::tibble(!!time := time_seq_data[[time]],
                            !!group_id := time_seq_data[[group_id]],
                            !!"interval" := time_int)
    # Left-join time sequence df with interval
    out <- out %>%
      dplyr::left_join(int_df, by = c(group_id, time))
  }
  out
}

# # This checks if time aggregation is necessary
needs_aggregation <- function(by, granularity){
  if (is.null(by)){
    out <- FALSE
  } else {
    by_unit_info <- unit_guess(by)
    granularity_unit_info <- unit_guess(granularity)

    by_seconds <- as.numeric(time_unit(by_unit_info[["unit"]],
                                       type = "duration")(
      by_unit_info[["num"]] * by_unit_info[["scale"]]))
    granularity_seconds <- as.numeric(time_unit(granularity_unit_info[["unit"]],
                                                type = "duration")(
      granularity_unit_info[["num"]] * granularity_unit_info[["scale"]]))

    if (granularity_seconds < by_seconds){
      out <- TRUE
    } else {
      out <- FALSE
    }
  }
  out
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
  collapse::setv(breaksi, 0L, length(breaks) + 1L, vind1 = FALSE)
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
