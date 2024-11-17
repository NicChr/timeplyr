#' Create a table of common time units from a date or datetime sequence.
#'
#' @param x date or datetime vector.
#' @param label Logical. Should labelled (ordered factor) versions of
#' week day and month be returned? Default is `TRUE`.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday, 7 means Sunday (default). When `label = TRUE`,
#' this will be the first level of the returned factor.
#' You can set `lubridate.week.start` option to control this parameter globally.
#' @param fiscal_start Numeric indicating the starting month of a fiscal year.
#' @param name Name of date/datetime column.
#'
#' @returns
#' An object of class `tibble`.
#'
#' @examples
#' library(timeplyr)
#' library(lubridate)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # Create a calendar for the current year
#' from <- floor_date(today(), unit = "year")
#' to <- ceiling_date(today(), unit = "year", change_on_boundary = TRUE) - days(1)
#'
#' my_seq <- time_seq(from, to, time_by = "day")
#' calendar(my_seq)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname calendar
#' @export
calendar <- function(x, label = TRUE,
                     week_start = getOption("lubridate.week.start", 1),
                     fiscal_start = getOption("lubridate.fiscal.start", 1),
                     name = "time"){
  dates <- convert_common_dates(x)
  time_info <- as.POSIXlt(dates)
  year <- time_info$year + 1900L
  quarter <- (( (time_info$mon %/% 3L) + (as.integer(fiscal_start) - 1L) ) %% 4L) + 1L
  month <- time_info$mon + 1L
  day <- time_info$mday
  yday <- time_info$yday
  week <- ( (yday %/% 7L) + 1L )
  isoyear <- as.integer(lubridate::isoyear(time_info))
  isoweek <- as.integer(lubridate::isoweek(time_info))
  isoday <- cheapr::val_replace(time_info$wday, 0L, 7L)
  epiyear <- as.integer(lubridate::epiyear(time_info))
  epiweek <- as.integer(lubridate::epiweek(time_info))
  wday <- cheapr::val_replace((time_info$wday - (as.integer(week_start) - 1L)) %% 7L, 0L, 7L)
  if (label){
    days <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    days <- days[cheapr::val_replace((1:7 + (week_start)) %% 7L, 0L, 7L)]

    wday_l <- as.integer(wday)
    attr(wday_l, "levels") <- days
    class(wday_l) <- c("ordered", "factor")

    month_l <- as.integer(time_info$mon + 1L)
    attr(month_l, "levels") <- .months
    class(month_l) <- c("ordered", "factor")
  } else {
    wday_l <- NULL
    month_l <- NULL
  }
  if (is_datetime(dates)){
    hour <- time_info$hour
    minute <- time_info$min
    second <- time_info$sec
  } else {
    hour <- NULL
    minute <- NULL
    second <- NULL
  }
  out <- fastplyr::new_tbl(
    x, year, quarter, month, month_l, week, day,
    yday, isoyear, isoweek, isoday, epiyear, epiweek, wday, wday_l,
    hour, minute, second
  )
  names(out)[1] <- name
  out
}
