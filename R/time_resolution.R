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
#' @rdname time_resolution
#' @export
time_resolution <- function(x, ...){
  UseMethod("time_resolution")
}
#' @export
time_resolution.integer <- function(x, ...){
  new_timespan(NA_character_, 1L)
}
#' @export
time_resolution.numeric <- function(x, ...){
  new_timespan(NA_character_, 1)
}
#' @export
time_resolution.Date <- function(x, ...){
  new_timespan("days", 1L)
}
#' @export
time_resolution.POSIXt <- function(x, ...){
  new_timespan("seconds", 1L)
}
#' @export
time_resolution.year_month <- function(x, ...){
  new_timespan(NA_character_, 1L)
}
#' @export
time_resolution.year_quarter <- function(x, ...){
  new_timespan(NA_character_, 1L)
}
#' @export
time_resolution.yearmon <- function(x, ...){
  new_timespan(NA_character_, 1/12)
}
#' @export
time_resolution.yearqtr <- function(x, ...){
  new_timespan(NA_character_, 1/4)
}

#' @rdname time_resolution
#' @export
time_granularity <- function(x, ...){
  UseMethod("time_granularity")
}
#' @export
time_granularity.numeric <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @export
time_granularity.Date <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan("days", gcd_diff)
}
#' @export
time_granularity.POSIXt <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan("seconds", gcd_diff)
}
#' @export
time_granularity.year_month <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @export
time_granularity.year_quarter <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @export
time_granularity.yearmon <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @export
time_granularity.yearqtr <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
