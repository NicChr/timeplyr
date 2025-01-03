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
granularity.Date <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan("days", gcd_diff)
}
#' @export
granularity.POSIXt <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan("seconds", gcd_diff)
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
