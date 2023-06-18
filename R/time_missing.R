#' Check for missing dates or datetimes
#'
#' @description `missing_dates()` returns missing dates between
#' the first and last date of a date/datetime vector.
#'
#' `time_missing()` checks for missing gaps in time for any date or datetime
#' sequence, and so is more general. Specify the time unit increment
#' through `time_by`.
#'
#'
#' @param x A date or datetime vector, list, or data frame.
#' @param time_by Must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' library(nycflights13)
#'
#' missing_dates(flights$time_hour)
#' time_missing(flights$time_hour)
#' time_missing(flights$time_hour, "hours")
#'
#' # Missing dates by origin and dest
#' flights %>%
#'   mutate(date = as_date(time_hour)) %>%
#'   group_by(origin, dest) %>%
#'   summarise(n_missing = n_missing_dates(date))
#' @rdname time_missing
#' @export
time_missing <- function(x, time_by = NULL, na.rm = TRUE){
  if (!na.rm && sum(is.na(x)) > 0){
    vctrs::vec_init(x, n = 1L)
  } else {
    time_seq <- time_expandv(x, time_by = time_by)
    if (length(setdiff(x, time_seq) > 0L)){
      warning("x is not regular. Results may be misleading.")
    }
    time_seq[!time_seq %in% x]
  }
}
#' @rdname time_missing
#' @export
missing_dates <- function(x, na.rm = TRUE){
  d_missing <- function(d){
    if (!is_time(d)){
      stop("x must be a date or datetime")
    }
    if (!na.rm && sum(is.na(d)) > 0){
      lubridate::NA_Date_
    } else {
      d <- lubridate::as_date(d)
      d_seq <- time_expandv(d, time_by = list("days" = 1),
                            is_sorted = FALSE)
      d_seq[!d_seq %in% d]
    }
  }
  if (is.list(x)){
    time_vars <- which(purrr::map_lgl(x, is_time))
    lapply(x[time_vars], d_missing)
  } else {
    d_missing(x)
  }
}
#' @rdname time_missing
#' @export
n_missing_dates <- function(x, na.rm = TRUE){
  nmissdates <- function(x){
    if (!is_time(x)){
      stop("x must be a date or datetime")
    }
    if (length(x) == 0L){
      return(0L)
    }
    if (!na.rm && sum(is.na(x)) > 0){
      NA_integer_
    } else {
      x <- as.integer(lubridate::as_date(x))
      d_rng <- collapse::frange(x, na.rm = TRUE)
      diff(d_rng) + 1L - n_unique(x, na.rm = TRUE)
    }
  }
  if (is.list(x)){
    vapply(x, nmissdates, integer(1))
    } else {
      nmissdates(x)
    }
}
#' @rdname time_missing
#' @export
n_time_missing <- function(x, time_by = NULL, na.rm = TRUE){
  length(time_missing(x, time_by = time_by, na.rm = na.rm))
}
