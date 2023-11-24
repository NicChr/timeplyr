#' Efficient, simple and flexible ISO week calculation
#'
#' @description `iso_week()` is a flexible function to return formatted
#' ISO weeks, with optional ISO year and ISO day.
#' `isoday()` returns the day of the ISO week.
#'
#' @param x Date vector.
#' @param year Logical. If `TRUE` then ISO Year is returned
#' along with the ISO week.
#' @param day Logical. If `TRUE` then day of the week is returned
#' with the ISO week, starting at 1, Monday, and ending at 7, Sunday.

#' @returns
#' An ISO week vector of class `character`.

#' @examples
#' library(timeplyr)
#' library(lubridate)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' iso_week(today())
#' iso_week(today(), day = TRUE)
#' iso_week(today(), year = FALSE, day = TRUE)
#' iso_week(today(), year = FALSE, day = FALSE)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
iso_week <- function(x, year = TRUE, day = FALSE){
  w <- lubridate::isoweek(x)
  if (year){
    y <- lubridate::isoyear(x)
  }
  if (day){
    d <- isoday(x)
  }
  if (year && day){
    out <- sprintf("%.4d-W%02d-%d", y, w, d)
  } else if (year && !day){
    out <- sprintf("%.4d-W%02d", y, w)
  } else if (!year && day){
    out <- sprintf("W%02d-%d", w, d)
  } else {
    out <- sprintf("W%02d", w)
  }
  out[is.na(w)] <- NA_character_
  out
}
#' @rdname iso_week
#' @export
isoday <- function(x){
  out <- data.table::wday(x) - 1L
  out[collapse::whichv(out, 0L)] <- 7L
  out
}
# Integer isoweek
# isoweek <- function(x){
#   x <- as.POSIXlt(x)
#   y <- x$year + 1900L
#   m <- x$mon + 1L
#   d <- x$mday
#   wday <- x$wday
#   wday[cpp_which(wday == 0L)] <- 7L
#   date <- as_int_date(lubridate::make_date(y, m, d))
#   date <- date + (4L - wday)
#   jan1 <- as.integer(lubridate::make_date(as.POSIXlt(date)$year + 1900L, 1L, 1L))
#   1L + (as.integer(date) - jan1) %/% 7L
# }
