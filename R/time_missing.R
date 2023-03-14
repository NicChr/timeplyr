#' Check for missing dates or datetimes in a regular time sequence
#'
#' @description `missing_dates()` checks for missing dates, between
#' the first and last date of a date/datetime vector whereas
#' `time_missing()` checks for missing gaps in time for any date or datetime
#' sequence, and so is more general. Specify the time unit increment
#' through `by`.
#'
#' @param data A date or datetime vector or data frame.
#' @param by Argument to expand and summarise time series.
#' If `by` is `NULL` then a heuristic will try and estimate the highest
#' order time unit associated with the time variable.
#' If specified, then by must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `by = "days"` or `by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `by = 1`.
#' This is also vectorized where applicable.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' library(nycflights13)
#'
#' missing_dates(flights$time_hour)
#' time_missing(flights$time_hour)
#' time_missing(flights$time_hour, by = "hour")
#' #'
#' # Missing dates by origin and dest
#' flights %>%
#'   mutate(date = as_date(time_hour)) %>%
#'   group_by(origin, dest) %>%
#'   summarise(n_missing = n_time_missing(date))
#' @rdname time_missing
#' @export
time_missing <- function(data, by = NULL){
  UseMethod("time_missing")
}
#' @rdname time_missing
#' @export
time_missing.default <- function(data, by = NULL){
  data <- as.double(data)
  time_seq <- time_span(x = data, by = by, is_sorted = FALSE)
  time_seq[!time_seq %in% data]
}
#' @rdname time_missing
#' @export
time_missing.Date <- function(data, by = NULL){
  if (is.null(by)) by <- "days"
  time_seq <- time_span(x = data, by = by, is_sorted = FALSE)
  time_seq[!time_seq %in% data]
}
#' @rdname time_missing
#' @export
time_missing.POSIXt <- function(data, by = NULL){
  time_seq <- time_span(x = data, by = by, is_sorted = FALSE)
  time_seq[!time_seq %in% data]
}
#' @rdname time_missing
#' @export
time_missing.numeric <- function(data, by = NULL){
  time_seq <- time_span(x = data, by = by, is_sorted = FALSE)
  time_seq[!time_seq %in% data]
}
#' @rdname time_missing
#' @export
time_missing.integer <- function(data, by = NULL){
  time_seq <- time_span(x = data, by = by, is_sorted = FALSE)
  time_seq[!time_seq %in% data]
}
#' @rdname time_missing
#' @export
time_missing.data.frame <- function(data, by = NULL){
  time_vars <- names(data)[purrr::map_lgl(data, is_time_or_num)]
  if (dplyr::is_grouped_df(data)){
    purrr::map(time_vars, ~
               dplyr_summarise(data, !!.x := time_missing(.data[[.x]],
                                                          by = by)))
  } else {
    data %>%
      dplyr::select(dplyr::all_of(time_vars)) %>%
      purrr::map(~ time_missing(.x, by = by))
  }
}
#' @rdname time_missing
#' @export
missing_dates <- function(data){
  d_missing <- function(d){
    d <- lubridate::as_date(convert_common_dates(d))
    d_seq <- time_span(x = d, by = list("days" = 1),
                       seq_type = "period",
                       is_sorted = FALSE)
    d_seq[!d_seq %in% d]
  }
  if (is.list(data)){
    time_vars <- which(purrr::map_lgl(data, is_time))
    lapply(data[time_vars], d_missing)
  } else {
    d_missing(data)
  }
}
#' @rdname time_missing
#' @export
n_time_missing <- function(data, by = NULL){
  UseMethod("n_time_missing")
}
#' @rdname time_missing
#' @export
n_time_missing.default <- function(data, by = NULL){
  length(time_missing(data, by = by))
}
