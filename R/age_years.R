#' Accurate and efficient age calculation
#'
#' @description Correct calculation of ages in years using lubridate periods.
#' Leap year calculations work as well.
#'
#' @param start Start date/datetime, typically date of birth.
#' @param end End date/datetime. Default is current date/datetime.
#'
#' @returns
#' Integer vector of age in years.
#'
#' @export
age_years <- function(start, end = if (is_date(start)) Sys.Date() else Sys.time()){
  check_is_time(start)
  check_is_time(end)
  as.integer(
    lubridate::year(
      lubridate::as.period(
        lubridate::interval(start, end),
        unit = "years"
      )
    )
  )
}
