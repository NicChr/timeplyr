#' Accurate and efficient age calculation
#'
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Correct calculation of ages in years using lubridate periods.
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
  lifecycle::deprecate_warn(
    "1.1.1", "age_years()",
    details = "Please use `trunc(time_diff())` instead and set `timespan = 'years'`"
  )
  period_diff(start, end, new_timespan("years"), fractional = FALSE)
}
#' @rdname age_years
#' @export
age_months <- function(start, end = if (is_date(start)) Sys.Date() else Sys.time()){
  lifecycle::deprecate_warn(
    "1.1.1", "age_months()",
    details = "Please use `trunc(time_diff())` instead and set `timespan = 'months'`"
  )
  period_diff(start, end, new_timespan("months"), fractional = FALSE)
}
