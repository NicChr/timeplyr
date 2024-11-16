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
  quarter <- as.integer(lubridate::quarter(time_info,
                                           type = "quarter",
                                           fiscal_start = fiscal_start))
  month <- time_info$mon + 1L
  week <- as.integer(lubridate::week(time_info))
  day <- time_info$mday
  yday <- time_info$yday
  isoyear <- as.integer(lubridate::isoyear(time_info))
  isoweek <- as.integer(lubridate::isoweek(time_info))
  isoday <- isoday(time_info)
  epiyear <- as.integer(lubridate::epiyear(time_info))
  epiweek <- as.integer(lubridate::epiweek(time_info))
  wday <- as.integer(lubridate::wday(time_info, week_start = week_start))
  out_nms <- c(name, "year", "quarter", "month",
               "month_l", "week", "day", "yday", "isoyear",
               "isoweek", "isoday", "epiyear", "epiweek",
               "wday", "wday_l",
               "hour", "minute", "second")
  if (label){
    wday_l <- lubridate::wday(time_info, week_start = week_start, label = TRUE)
    month_l <- lubridate::month(time_info, label = TRUE, abbr = TRUE)
  } else {
    wday_l <- NULL
    month_l <- NULL
    out_nms <- setdiff(out_nms, c("wday_l", "month_l"))
  }
  if (is_datetime(dates)){
    hour <- time_info$hour
    minute <- time_info$min
    second <- time_info$sec
  } else {
    hour <- NULL
    minute <- NULL
    second <- NULL
    out_nms <- setdiff(out_nms, c("hour", "minute", "second"))
  }
  out <- fastplyr::new_tbl(
    x, year, quarter, month, month_l, week, day,
    yday, isoyear, isoweek, isoday, epiyear, epiweek, wday, wday_l,
    hour, minute, second
  )
  names(out)[1] <- name
  out
}
