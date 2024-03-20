# Faster lubridate::period for larger data basically

lubridate_period <- function(...){
  periods <- unclass(time_period(...))
  # lubridate::period() propagates NA values across all periods
  if (cheapr::num_na(periods, recursive = TRUE) > 0){
    which_na_fill <- cheapr::which_(cheapr::row_any_na(list_as_df(periods)))
    for (i in seq_along(periods)){
      periods[[i]][which_na_fill] <- NA
    }
  }
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
  lubridate_period(seconds = x)
}

minutes <- function(x = 1L){
  lubridate_period(minutes = x)
}

hours <- function(x = 1L){
  lubridate_period(hours = x)
}

days <- function(x = 1L){
  lubridate_period(days = x)
}

weeks <- function(x = 1L){
  lubridate_period(weeks = x)
}

months <- function(x = 1L){
  lubridate_period(months = x)
}

years <- function(x = 1L){
  lubridate_period(years = x)
}
