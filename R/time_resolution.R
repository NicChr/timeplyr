#' @rdname time_resolution
#' @export
time_resolution <- function(x, ...){
  UseMethod("time_resolution")
}
#' @rdname time_resolution
#' @export
time_resolution.integer <- function(x, ...){
  new_timespan(NA_character_, 1L)
}
#' @rdname time_resolution
#' @export
time_resolution.numeric <- function(x, ...){
  new_timespan(NA_character_, NA_real_)
}
#' @rdname time_resolution
#' @export
time_resolution.Date <- function(x, ...){
  new_timespan("days", 1L)
}
#' @rdname time_resolution
#' @export
time_resolution.POSIXt <- function(x, ...){
  new_timespan("seconds", 1L)
}
#' @rdname time_resolution
#' @export
time_resolution.year_month <- function(x, ...){
  new_timespan(NA_character_, 1L)
}
#' @rdname time_resolution
#' @export
time_resolution.year_quarter <- function(x, ...){
  new_timespan(NA_character_, 1L)
}
#' @rdname time_resolution
#' @export
time_resolution.yearmon <- function(x, ...){
  new_timespan(NA_character_, 1/12)
}
#' @rdname time_resolution
#' @export
time_resolution.yearqtr <- function(x, ...){
  new_timespan(NA_character_, 1/4)
}

#' @rdname time_resolution
#' @export
time_granularity <- function(x, ...){
  UseMethod("time_granularity")
}
#' @rdname time_resolution
#' @export
time_granularity.numeric <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @rdname time_resolution
#' @export
time_granularity.Date <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan("days", gcd_diff)
}
#' @rdname time_resolution
#' @export
time_granularity.POSIXt <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan("seconds", gcd_diff)
}
#' @rdname time_resolution
#' @export
time_granularity.year_month <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @rdname time_resolution
#' @export
time_granularity.year_quarter <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @rdname time_resolution
#' @export
time_granularity.yearmon <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
#' @rdname time_resolution
#' @export
time_granularity.yearqtr <- function(x, ...){
  gcd_diff <- gcd_time_diff(unclass(x))
  new_timespan(NA_character_, gcd_diff)
}
