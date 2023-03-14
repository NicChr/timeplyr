#' Wrapper around `timechange::time_floor()` to floor
#' dates and datetimes, as well as numeric values.
#'
#' @param x Date, datetime or numeric vector.
#' @param by Unit used to perform the rounding.
#' @param week_start Start of the week (between 1 and 7),
#' where 1 is Monday and 7 is Sunday.
#'
#' @export
time_floor <- function(x, by,
                       week_start = getOption("lubridate.week.start", 1)){
  unit_info <- unit_guess(by)
  by_unit <- unit_info[["unit"]]
  by_n <- unit_info[["num"]] * unit_info[["scale"]]
  if (is_time(x)){
    by <- paste(by_n, by_unit)
    timechange::time_floor(x, unit = by, week_start = week_start)
  } else {
    floor(x / by_n) * by_n
  }
}
