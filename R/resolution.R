#' Time resolution & granularity
#'
#' @description
#' The definitions of resolution and granularity may evolve over time but
#' currently the resolution defines the smallest timespan
#' that differentiates two non-fractional instances in time.
#' The granularity defines the smallest common timespan.
#' A practical example would be when using dates to record data with a monthly
#' frequency. In this case the granularity is 1 month, whereas the resolution
#' of the data type `Date` is 1 day. Therefore the resolution depends
#' only on the data type whereas the granularity depends on the frequency
#' with which the data is recorded.
#'
#'
#' @param x Time vector. \cr
#' E.g. a `Date`, `POSIXt`, `numeric` or any time-based vector.
#' @param ... Further arguments passed to methods.
#'
#' @details
#' For dates and date-times, the argument `exact = TRUE`
#' can be used to detect monthly/yearly granularity.
#' In some cases this can be slow and memory-intensive so
#' it is advised to set this to `FALSE` in these cases.
#'
#' The default for dates is `exact = TRUE` whereas the default
#' for date-times is `exact = FALSE`.
#'
#'
#' @returns
#' A [timespan] object.
#'
#' @rdname resolution
#' @export
resolution <- function(x, ...){
  UseMethod("resolution")
}
#' @export
resolution.integer <- function(x, ...){
  new_timespan(NA_character_, 1L)
}
#' @export
resolution.numeric <- function(x, ...){
  new_timespan(NA_character_, 1)
}
#' @export
resolution.Date <- function(x, ...){
  new_timespan("days", `storage.mode<-`(1L, storage.mode(x)))
}
#' @export
resolution.POSIXt <- function(x, ...){
  new_timespan("seconds", `storage.mode<-`(1L, storage.mode(x)))
}
#' @export
resolution.year_month <- function(x, ...){
  new_timespan(NA_character_, 1L)
}
#' @export
resolution.year_quarter <- function(x, ...){
  new_timespan(NA_character_, 1L)
}
#' @export
resolution.yearmon <- function(x, ...){
  new_timespan(NA_character_, 1/12)
}
#' @export
resolution.yearqtr <- function(x, ...){
  new_timespan(NA_character_, 1/4)
}

#' @rdname resolution
#' @export
granularity <- function(x, ...){
  UseMethod("granularity")
}
#' @export
granularity.numeric <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @export
granularity.Date <- function(x, exact = TRUE, ...){

  # Use unique dates as most analyses won't involve many unique dates
  # and so this is usually more efficient

  if (length(x) >= 1e04){
    x <- collapse::funique(x)
  }
  if (exact){
    td <- time_elapsed(x, rolling = FALSE, new_timespan("months"), na_skip = TRUE)
    is_whole_num <- is_whole_number(td, na.rm = TRUE)
    if (!is_whole_num){
      gcd_delta <- gcd_time_diff(unclass(x))
      out_unit <- "days"
    } else {
      gcd_delta <- gcd_time_diff(td)
      out_unit <- "months"
    }
  } else {
    gcd_delta <- gcd_time_diff(unclass(x))
    out_unit <- "days"
  }
  new_timespan(out_unit, gcd_delta)
}
#' @export
granularity.POSIXt <- function(x, exact = FALSE, ...){
  if (exact){
    td <- time_elapsed(x, rolling = FALSE, new_timespan("months"), na_skip = TRUE)
    is_whole_num <- is_whole_number(td, na.rm = TRUE)
    if (!is_whole_num){
      gcd_delta <- gcd_time_diff(unclass(x))
      out_unit <- "seconds"
    } else {
      gcd_delta <- gcd_time_diff(td)
      out_unit <- "months"
    }
  } else {
    gcd_delta <- gcd_time_diff(unclass(x))
    out_unit <- "seconds"
  }
  new_timespan(out_unit, gcd_delta)
}
#' @export
granularity.year_month <- function(x, ...){
  if (length(x) >= 1e04){
    x <- collapse::funique(x)
  }
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @export
granularity.year_quarter <- function(x, ...){
  if (length(x) >= 1e04){
    x <- collapse::funique(x)
  }
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @export
granularity.yearmon <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @export
granularity.yearqtr <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}

