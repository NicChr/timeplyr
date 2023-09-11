#' Fast greatest common divisor of time differences
#'
#' @param x Time variable. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`, `yearmon`, or `yearqtr`.
#' @param time_by Time unit (default is 1). \cr
#' Must be one of the following:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
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
#' library(lubridate)
#' time_diff_gcd(1:10)
#' time_diff_gcd(seq(0, 1, 0.2))
#'
#' time_diff_gcd(time_seq(today(), today() + 100, time_by = "3 days"))
#' time_diff_gcd(time_seq(now(), len = 10^2, time_by = "125 seconds"))
#'
#' # Monthly gcd using lubridate periods
#' quarter_seq <- time_seq(today(), len = 24, time_by = months(4))
#' time_diff_gcd(quarter_seq, time_by = months(1))
#' time_diff_gcd(quarter_seq, time_by = "months", time_type = "duration")
#'
#' # Detects monthly granularity
#' dplyr::near(time_diff_gcd(time(AirPassengers)), 1/12, tol = 1e-06)
#' @export
time_diff_gcd <- function(x, time_by = 1,
                          time_type = c("auto", "duration", "period"),
                          is_sorted = FALSE,
                          tol = sqrt(.Machine$double.eps)){
  if (!is_sorted){
    is_sorted <- is_sorted(x)
  }
  x <- collapse::funique(x, sort = !is_sorted)
  all_na <- collapse::allNA(x)
  x <- x[!is.na(x)]
  if (all_na){
    return(NA_real_)
  }
  if (length(x) == 0L){
    return(numeric(0))
  }
  if (length(x) == 1L){
    return(1)
  }
  tdiff <- time_elapsed(x, rolling = FALSE,
                        time_by = time_by,
                        time_type = time_type,
                        g = NULL,
                        na_skip = FALSE)
  tdiff <- collapse::fdiff.default(tdiff, fill = Inf, n = 1)
  log10_tol <- ceiling(abs(log10(tol)))
  tdiff <- collapse::funique.default(
    round(
      abs(tdiff), digits = log10_tol
    )
  )
  tdiff <- tdiff[double_gt(tdiff, 0, tol = tol)]
  if (length(tdiff) == 1 && tdiff == Inf){
    return(10^(-log10_tol))
  }
  collapse::vgcd(tdiff)
}
time_diff_gcd2 <- function(x, time_type = c("auto", "duration", "period"),
                          is_sorted = FALSE,
                          tol = sqrt(.Machine$double.eps)){
  if (!is_sorted){
    is_sorted <- is_sorted(x)
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
    tdiff <- abs(tdiff[-1L])
    if (!is.integer(tdiff)){
      tdiff <- round(tdiff, digits = log10_tol)
    }
    tdiff <- collapse::funique(tdiff, sort = FALSE)
    tdiff <- tdiff[double_gt(tdiff, 0, tol = tol)]
    if (length(tdiff) == 0L){
      return(10^(-log10_tol))
    }
    gcd_diff <- collapse::vgcd(tdiff)
  }
  gcd_diff
}

# Very simple version
# time_diff_gcd <- function(x, is_sorted = FALSE, tol = sqrt(.Machine$double.eps)){
#   if (!is_sorted){
#     is_sorted <- is_sorted(x)
#   }
#   x <- collapse::funique(x, sort = !is_sorted)
#   all_na <- collapse::allNA(x)
#   x <- x[!is.na(x)]
#   if (all_na){
#     return(NA_real_)
#   }
#   if (length(x) == 0L){
#     return(numeric(0))
#   }
#   if (length(x) == 1L){
#     return(1)
#   }
#   tdiff <- collapse::fdiff.default(time_as_number(x), n = 1)
#   log10_tol <- ceiling(abs(log10(tol)))
#   tdiff <- collapse::funique.default(
#     round(
#       abs(tdiff), digits = log10_tol
#     )
#   )
#   tdiff <- tdiff[double_gt(tdiff, 0, tol = tol)]
#   if (length(tdiff) == 1 && tdiff == Inf){
#     return(10^(-log10_tol))
#   }
#   collapse::vgcd(tdiff)
# }
