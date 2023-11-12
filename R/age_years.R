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
  out <- per[["year"]]
  out
}
#' @rdname age_years
#' @export
age_months <- function(start, end = if (is_date(start)) Sys.Date() else Sys.time()){
  check_is_time(start)
  check_is_time(end)
  per <- int_to_per(start, end)
  months <- per[["month"]]
  years <- per[["year"]]
  if (all_integerable(
    abs(collapse::frange(months) ) +
    abs(collapse::frange(years)) * 12
  )){
    months + years * 12L
  } else {
    months + years * 12
  }
}
# as.integer(
#   lubridate::year(
#     lubridate::as.period(
#       lubridate::interval(start, end),
#       unit = "years"
#     )
#   )
# )
