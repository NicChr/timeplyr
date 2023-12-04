#' Create a table of common time units from a date or datetime sequence.
#'
#' @param data A data frame.
#' @param time Time variable.
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
  dates <- as.POSIXlt(convert_common_dates(x))
  year <- dates$year + 1900L
  quarter <- as.integer(lubridate::quarter(dates,
                                           type = "quarter",
                                           fiscal_start = fiscal_start))
  month <- dates$mon + 1L
  week <- as.integer(lubridate::week(dates))
  day <- dates$mday
  yday <- dates$yday
  isoyear <- as.integer(lubridate::isoyear(dates))
  isoweek <- as.integer(lubridate::isoweek(dates))
  isoday <- isoday(dates)
  epiyear <- as.integer(lubridate::epiyear(dates))
  epiweek <- as.integer(lubridate::epiweek(dates))
  wday <- as.integer(lubridate::wday(dates, week_start = week_start))
  out_nms <- c(name, "year", "quarter", "month",
               "month_l", "week", "day", "yday", "isoyear",
               "isoweek", "isoday", "epiyear", "epiweek",
               "wday", "wday_l",
               "hour", "minute", "second")
  if (label){
    wday_l <- lubridate::wday(dates, week_start = week_start, label = TRUE)
    month_l <- lubridate::month(dates, label = TRUE, abbr = TRUE)
  } else {
    wday_l <- NULL
    month_l <- NULL
    out_nms <- setdiff(out_nms, c("wday_l", "month_l"))
  }
  if (is_datetime(dates)){
    hour <- dates$hour
    minute <- dates$min
    second <- dates$sec
  } else {
    hour <- NULL
    minute <- NULL
    second <- NULL
    out_nms <- setdiff(out_nms, c("hour", "minute", "second"))
  }
  add_names(
    new_tbl(x, year, quarter, month, month_l, week, day,
            yday, isoyear, isoweek, isoday, epiyear, epiweek, wday, wday_l,
            hour, minute, second),
    out_nms
  )
}
#' @rdname calendar
#' @export
add_calendar <- function(data, time = NULL, label = TRUE,
                         week_start = getOption("lubridate.week.start", 1),
                         fiscal_start = getOption("lubridate.fiscal.start", 1)){
  if (rlang::quo_is_null(enquo(time))){
    time_vars <- names(data)[cpp_which(vapply(data, is_time, FALSE))]
    if (length(time_vars) == 0) {
      stop("Please specify a time variable")
    } else {
      time_var <- time_vars[[1L]]
      message(paste0("Using ", time_var))
    }
  } else {
    time_var <- tidy_select_names(data, !!enquo(time))
  }
  calendar <- fselect(calendar(data[[time_var]], label = label, week_start = week_start,
                       fiscal_start = fiscal_start), .cols = -1L)
  dplyr::bind_cols(data, calendar)
}
