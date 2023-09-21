#' Create a table of common time units from a date or datetime sequence.
#'
#' @param data A data frame.
#' @param time Time variable.
#' @param x date or datetime vector.
#' @param label Logical. Should labeled (ordered factor) versions of
#' week day and month be returned? Default is `TRUE`.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday, 7 means Sunday (default). When `label = TRUE`,
#' this will be the first level of the returned factor.
#' You can set lubridate.week.start option to control this parameter globally.
#' @param fiscal_start Numeric indicating the starting month of a fiscal year.
#' @param name Name of date/datetime column.
#' @return
#' An object of class `tibble`.
#'
#' @examples
#' library(timeplyr)
#' library(lubridate)
#'
#' # Create a calendar for the current year
#' from <- floor_date(today(), unit = "year")
#' to <- ceiling_date(today(), unit = "year", change_on_boundary = TRUE) - days(1)
#'
#' my_seq <- time_seq(from, to, time_by = "day")
#' calendar(my_seq)
#'
#' @rdname calendar
#' @export
calendar <- function(x, label = TRUE,
                     week_start = getOption("lubridate.week.start", 1),
                     fiscal_start = getOption("lubridate.fiscal.start", 1),
                     name = "time"){
  time_seq <- convert_common_dates(x)
  year <- as.integer(lubridate::year(time_seq))
  quarter <- as.integer(lubridate::quarter(time_seq,
                                           type = "quarter",
                                           fiscal_start = fiscal_start))
  month <- as.integer(lubridate::month(time_seq))
  week <- as.integer(lubridate::week(time_seq))
  day <- as.integer(lubridate::day(time_seq))
  yday <- as.integer(lubridate::yday(time_seq))
  isoyear <- as.integer(lubridate::isoyear(time_seq))
  isoweek <- as.integer(lubridate::isoweek(time_seq))
  isoday <- isoday(time_seq)
  epiyear <- as.integer(lubridate::epiyear(time_seq))
  epiweek <- as.integer(lubridate::epiweek(time_seq))
  wday <- as.integer(lubridate::wday(time_seq, week_start = week_start))
  out_nms <- c(name, "year", "quarter", "month",
               "month_l", "week", "day", "yday", "isoyear",
               "isoweek", "isoday", "epiyear", "epiweek",
               "wday", "wday_l",
               "hour", "minute", "second")
  if (label){
    wday_l <- lubridate::wday(time_seq, week_start = week_start, label = TRUE)
    month_l <- lubridate::month(time_seq, label = TRUE, abbr = TRUE)
  } else {
    wday_l <- NULL
    month_l <- NULL
    out_nms <- setdiff(out_nms, c("wday_l", "month_l"))
  }
  if (is_datetime(time_seq)){
    hour <- lubridate::hour(time_seq)
    minute <- lubridate::minute(time_seq)
    second <- lubridate::second(time_seq)
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
    time_vars <- tidy_select_names(data, dplyr::where(is_time))
    if (length(time_vars) == 0) {
      stop("Please specify a time variable")
    } else {
      time_var <- time_vars[[1L]]
      message(paste0("Using ", time_var))
    }
  } else {
    data <- mutate2(data, !!enquo(time))
    time_var <- tidy_transform_names(data, !!enquo(time))
  }
  calendar <- fselect(calendar(data[[time_var]], label = label, week_start = week_start,
                       fiscal_start = fiscal_start), .cols = -1L)
  dplyr::bind_cols(data, calendar)
}
