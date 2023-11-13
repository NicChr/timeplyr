#' Accurate and efficient age calculation
#'
#' @description Correct calculation of ages in years using lubridate periods.
#' Leap year calculations work as well.
#'
#' @param start Start date/datetime, typically date of birth.
#' @param end End date/datetime. Default is current date/datetime.
#'
#' @returns
#' Integer vector of age in years or months.
#'
#' @rdname age_years
#' @export
age_years <- function(start, end = if (is_date(start)) Sys.Date() else Sys.time()){
  check_is_time(start)
  check_is_time(end)
  per <- int_to_per(start, end)
  secs <- per[["second"]]
  mins <- per[["minute"]]
  hours <- per[["hour"]]
  days <- per[["day"]]
  months <- per[["month"]]
  years <- per[["year"]]
  per_as_secs <- secs + 60 * mins + 60 * 60 * hours + 60 * 60 * 24 *
    days + 60 * 60 * 24 * 365.25/12 * months + 60 * 60 *
    24 * 365.25 * years
  as.integer(per_as_secs / 31557600)
}
#' @rdname age_years
#' @export
age_months <- function(start, end = if (is_date(start)) Sys.Date() else Sys.time()){
  check_is_time(start)
  check_is_time(end)
  per <- int_to_per(start, end)
  sec <- per[["second"]]
  min <- per[["minute"]]
  hour <- per[["hour"]]
  day <- per[["day"]]
  month <- per[["month"]]
  year <- per[["year"]]
  per_as_secs <- sec + 60 * min + 60 * 60 * hour + 60 * 60 * 24 *
    day + 60 * 60 * 24 * 365.25/12 * month + 60 * 60 *
    24 * 365.25 * year
  months <- per_as_secs / 2629800
  if (all_integerable(months)){
    out <- as.integer(months)
  } else {
    out <- trunc(months)
  }
  out
}
