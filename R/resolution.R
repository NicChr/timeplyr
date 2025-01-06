#' Time resolution & granularity
#'
#' @description
#' The definitions of resolution and granularity may evolve over time but
#' currently the resolution defines the smallest timespan
#' that differentiates two non-fractional instances in time.
#' The granularity defines the smallest common timespan.
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
granularity.Date <- function(x, exact = length(x) <= 10000, ...){
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
granularity.POSIXt <- function(x, exact = length(x) <= 10000, ...){
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
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @export
granularity.year_quarter <- function(x, ...){
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

time_gcd_diff2 <- function(x, tol = sqrt(.Machine$double.eps)){
  x <- collapse::funique(x, sort = FALSE)
  if (is_time(x)){
    unit <- get_time_unit(x)
    for (per in c("years", "months", "weeks")){
      tdiff <- time_elapsed(x, rolling = FALSE, time_by = per, na_skip = TRUE)
      is_whole_num <- is_whole_number(tdiff, na.rm = TRUE)
      if (is_whole_num){
        unit <- per
        break
      }
    }
    if (!is_whole_num){
      tdiff <- time_elapsed(x, rolling = TRUE, time_by = unit, na_skip = TRUE)
    } else {
      tdiff <- roll_diff(tdiff, fill = 0)
    }
  } else {
    unit <- "numeric"
    tdiff <- time_elapsed(x, rolling = TRUE,
                          time_by = 1L,
                          time_type = time_type,
                          g = NULL,
                          na_skip = TRUE)
  }
  gcd <- cheapr::gcd(tdiff, tol = tol, na_rm = TRUE, round = FALSE)
  add_names(list(gcd), unit)
}
