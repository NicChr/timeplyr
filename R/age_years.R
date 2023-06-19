#' Accurate and efficient age calculation
#'
#' @description Correct calculation of ages in years using lubridate periods.
#' Leap year calculations work as well.
#'
#' @return
#' Integer vector of age in years.
#'
#' @param start Start date/datetime, typically date of birth.
#' @param end End date/datetime. Default is current date/datetime.
#'
#' @export
age_years <- function(start, end = if (is_date(start)) Sys.Date() else Sys.time()){
  if (!is_time(start)) stop("start must be a date or datetime")
  if (!is_time(end)) stop("end must be a date or datetime")
  as.integer(time_diff(start, end, time_by = list("years" = 1),
                       as_period = TRUE,
                       time_type = "period"))
}
