

time_by_list <- function(time_by){
  unit_info <- unit_guess(time_by)
  units <- .subset2(unit_info, "unit")
  num <- .subset2(unit_info, "num") * .subset2(unit_info, "scale")
  add_names(list(num), units)
}
# Returns list with numeric vector element, where the name of the list
# is the time unit name
time_by_get <- function(x, time_by = NULL, quiet = FALSE){
  if (is.null(time_by)){
    unit_info <- time_granularity(x, msg = !quiet)
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
  is.numeric(time_by) ||
    # isTRUE(class(time_by) %in% c("integer", "numeric")) ||
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
time_by_pretty <- function(time_by, sep = " "){
  time_by <- time_by_list(time_by)
  units <- names(time_by)
  if (time_by_length(time_by) > 1){
    stop("Please supply only one numeric value in time_by")
  }
  num <- time_by[[1L]]
  if (units == "numeric"){
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
    num_seconds <- unit_to_seconds(time_by)
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

time_granularity <- function(x, msg = TRUE){
  gcd_diff <- gcd_time_diff(x)
  if (is_date(x)){
    granularity <- "day(s)"
    scale <- 1L
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
    scale <- 1L
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
time_granularity2 <- function(x){
  gcd_diff <- gcd_time_diff(x)
  if (is_date(x)){
    unit <- "days"
    scale <- 1
  } else if (is_datetime(x)){
    convert_seconds <- seconds_to_unit(gcd_diff)
    unit <- convert_seconds[["unit"]]
    scale <- convert_seconds[["scale"]]
  } else {
    unit <- "numeric"
    scale <- 1L
  }
  add_names(list(gcd_diff / scale), unit)
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

fcut_ind <- function(x, breaks, rightmost.closed = FALSE,
                     left.open = FALSE, all.inside = FALSE){
  breaksi <- findInterval(x,
                          breaks,
                          rightmost.closed = rightmost.closed,
                          left.open = left.open,
                          all.inside = all.inside)
  # This makes it so that NA is returned for any x where findinterval
  # resorts to 0 and doesn't just remove them
  breaksi[cheapr::val_find(breaksi, 0L)] <- NA_integer_
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


time_add2 <- function(x, time_by,
                      time_type = getOption("timeplyr.time_type", "auto"),
                      roll_month = getOption("timeplyr.roll_month", "preday"),
                      roll_dst = getOption("timeplyr.roll_dst", "NA")){
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
time_floor3 <- function(x, time_by, week_start = getOption("lubridate.week.start", 1)){
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
    time_floor3(x, time_by = add_names(list(1), names(time_by)), week_start = week_start)
  }
}
time_ceiling3 <- function(x, time_by, week_start = getOption("lubridate.week.start", 1),
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
    time_ceiling3(x, time_by = add_names(list(1), names(time_by)),
                 week_start = week_start,
                 change_on_boundary = change_on_boundary)
  }
}

time_as_number <- function(x){
  strip_attrs(unclass(x))
}
time_int_end <- function(x){
  attr(x, "end")
}
time_int_rm_attrs <- function(x){
  attr(x, "end") <- NULL
  attr(x, "direction") <- NULL
  x
}

match_time_type <- function(time_type){
  rlang::arg_match0(time_type, c("auto", "duration", "period"))
}

time_by_list_convert_weeks_to_days <- function(time_by){
  out <- time_by
  if (time_by_unit(out) == "weeks"){
    out <- list("days" = as.double(time_by_num(out) * 7))
  }
  out
}

# Convenience function to return base time unit of time variable
get_time_unit <- function(x){
  if (is_date(x)){
    "days"
  } else if (is_datetime(x)){
    "seconds"
  } else {
    "numeric"
  }
}
