# Faster lubridate::period for larger data basically

lubridate_period <- function(...){
  periods <- unclass(time_period(...))
  out <- lubridate::period()
  out@year <- periods[["years"]]
  out@month <- periods[["months"]]
  out@day <- periods[["days"]] + (periods[["weeks"]] * 7L)
  out@hour <- periods[["hours"]]
  out@minute <- periods[["minutes"]]
  out@.Data <- periods[["seconds"]]
  out
}

# Functional that returns lubridate period function
period_unit <- function(units = "seconds"){
  if (!units %in% .period_units) unit_match_stop(.period_units)
  switch(units,
         seconds = seconds,
         minutes = minutes,
         hours = hours,
         days = days,
         weeks = weeks,
         months = months,
         years = years)
}

seconds <- function(x = 1){
  out <- lubridate::period()
  fill <- integer(length(x))
  per_num <- rep_len(x, length(x))
  out@year <- fill
  out@month <- fill
  out@day <- fill
  out@hour <- fill
  out@minute <- fill
  out@.Data <- per_num
  out
}

minutes <- function(x = 1L){
  out <- lubridate::period()
  fill <- integer(length(x))
  per_num <- rep_len(x, length(x))
  out@year <- fill
  out@month <- fill
  out@day <- fill
  out@hour <- fill
  out@minute <- per_num
  out@.Data <- fill
  out
}

hours <- function(x = 1L){
  out <- lubridate::period()
  fill <- integer(length(x))
  per_num <- rep_len(x, length(x))
  out@year <- fill
  out@month <- fill
  out@day <- fill
  out@hour <- per_num
  out@minute <- fill
  out@.Data <- fill
  out
}

days <- function(x = 1L){
  out <- lubridate::period()
  fill <- integer(length(x))
  per_num <- rep_len(x, length(x))
  out@year <- fill
  out@month <- fill
  out@day <- per_num
  out@hour <- fill
  out@minute <- fill
  out@.Data <- fill
  out
}

weeks <- function(x = 1L){
  out <- lubridate::period()
  fill <- integer(length(x))
  per_num <- rep_len(x, length(x)) * 7L
  out@year <- fill
  out@month <- fill
  out@day <- per_num
  out@hour <- fill
  out@minute <- fill
  out@.Data <- fill
  out
}

months <- function(x = 1L){
  out <- lubridate::period()
  fill <- integer(length(x))
  per_num <- rep_len(x, length(x))
  out@year <- fill
  out@month <- per_num
  out@day <- fill
  out@hour <- fill
  out@minute <- fill
  out@.Data <- fill
  out
}

years <- function(x = 1L){
  out <- lubridate::period()
  fill <- integer(length(x))
  per_num <- rep_len(x, length(x))
  out@year <- per_num
  out@month <- fill
  out@day <- fill
  out@hour <- fill
  out@minute <- fill
  out@.Data <- fill
  out
}
