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
  list("unit" = unit,
       "num" = num,
       "scale" = scale)
}

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
    num_seconds <- unit_to_seconds2(time_by)
    higher_unit_info <- seconds_to_unit2(num_seconds)
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
    convert_seconds <- seconds_to_unit2(gcd_diff)
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

unit_guess <- function(x){
  if (inherits(x, c("Duration", "Period"))){
    time_unit_info <- time_unit_info(x)
    if (length(time_unit_info) > 1L){
      stop("Multiple period units are currently not supported.")
    }
    unit <- paste0(names(time_unit_info), "s")
    num <- .subset2(time_unit_info, 1L)
    out <- list("unit" = unit,
                "num" = num,
                "scale" = 1L
    )
    # If numeric then just return this..
  } else if (is.numeric(x)){
    out <- list("unit" = "numeric",
                "num" = x,
                "scale" = 1L
    )
  } else if (is.list(x)){
    # If it's a list, string match but no parse
    out <- unit_list_match(x)
  } else {
    # Try matching first as it's faster
    unit <- unit_match(x)
    # If that doesn't work finally try parsing
    if (is.na(unit)){
      out <- unit_parse2(x)
    } else {
      num <- 1L
      scale <- 1L
      # If the unit is something exotic,
      # The num needs to be scaled correctly
      if (unit %in% .extra_time_units){
        exotic_info <- convert_exotic_units(unit)
        scale <- .subset2(exotic_info, "scale")
        unit <- .subset2(exotic_info, "unit")
      }
      out <- list("unit" = unit,
                  "num" = num,
                  "scale" = scale)
    }
  }
  out
}

seconds_to_unit2 <- function(x){
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

unit_to_seconds2 <- function(x){
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

unit_parse2 <- function(x){
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
  out <- list("unit" = unit,
              "num" = num,
              "scale" = scale)
  out
}

# Temporary helper to convert old-style timespans to new
time_by_list_as_timespan <- function(x){
  unit <- time_by_unit(x)
  num <- time_by_num(x)

  if (unit == "numeric"){
    unit <- NA_character_
  }

  timespan(unit, num)

}
