#' Fast greatest common divisor of time differences
#'
#' @param x Time variable. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`, `yearmon`, or `yearqtr`.
#' @param time_type If "auto", `periods` are used if `x` is a Date and
#' durations are used if `x` is a datetime.
#' Otherwise numeric differences are calculated.
#' @param is_sorted Is `x` sorted? If `FALSE`, the default,
#' `sort(unique(x))` is calculated, otherwise `unique(x)` is used.
#' The order of successive time differences may impact the result.
#' @param tol Tolerance of comparison. The time differences are rounded
#' using `digits = ceiling(abs(log10(tol)))` to try and avoid
#' precision issues.
#' @return A double vector of length 1 or length 0 if `length(x)` is 0.
#' @examples
#' library(timeplyr)
#' time_diff_gcd(1:10)
#' time_diff_gcd(seq(0, 1, 0.2))
#'
#' time_diff_gcd(time_seq(Sys.Date(), Sys.Date() + 100, time_by = "3 days"))
#' time_diff_gcd(time_seq(Sys.time(), Sys.time() + 10^3, time_by = "125 seconds"))
#'
#' # Detects monthly granularity
#' dplyr::near(time_diff_gcd(time(AirPassengers)), 1/12)
#' @export
time_diff_gcd <- function(x, time_type = c("auto", "duration", "period"),
                          is_sorted = FALSE,
                          tol = sqrt(.Machine$double.eps)){
  if (!is_sorted){
    is_sorted <- isTRUE(!is.unsorted(x))
  }
  x <- collapse::funique(x, sort = !is_sorted)
  x <- x[!is.na(x)]
  if (length(x) == 0L){
    gcd_diff <- numeric(0)
  } else if (length(x) == 1L){
    gcd_diff <- 1
  } else {
    if (is_date(x)){
      tby <- "days"
    } else if (is_datetime(x)){
      tby <- "seconds"
    } else {
      tby <- 1
    }
    tdiff <- time_elapsed(x, rolling = TRUE,
                          time_by = tby,
                          time_type = time_type,
                          fill = NA, g = NULL,
                          na_skip = FALSE)
    # tdiff <- fdiff2(tdiff)
    log10_tol <- ceiling(abs(log10(tol)))
    tdiff <- round(abs(tdiff[-1L]), digits = log10_tol)
    tdiff <- collapse::funique(tdiff, sort = FALSE)
    # tdiff[double_equal(tdiff, 0, tol = tol)] <- max(tdiff)
    tdiff <- tdiff[double_gt(tdiff, 0, tol = tol)]
    if (length(tdiff) == 0L){
      return(10^(-log10_tol))
    }
    gcd_diff <- collapse::vgcd(tdiff)
  }
  gcd_diff
}
