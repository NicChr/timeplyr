#' Check for missing dates
#'
#' @description `missing_dates()` returns missing dates between
#' the first and last date of a date/datetime vector.
#'
#' @param x A date or datetime vector, list, or data frame.
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
#' @rdname missing_dates
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
      d_seq <- time_expandv(d, time_by = list("days" = 1))
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
#' @rdname missing_dates
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
