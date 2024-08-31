#' Time interval utilities
#'
#' @param x A 'time_interval'.
#' @param na_rm Should `NA` values be removed? Default is `TRUE`.
#' @param ... Additional arguments passed onto `time_diff`.
#'
#' @seealso [time_interval]
#'
#' @rdname interval_utils
#' @export
interval_start <- function(x){
  UseMethod("interval_start")
}
# interval_start.default <- function(x){
#   x
# }
#' @export
interval_start.time_interval <- function(x){
  unclass(x)[["start"]]
}
#' @export
interval_start.Interval <- function(x){
  attr(x, "start", TRUE)
}
#' @rdname interval_utils
#' @export
interval_end <- function(x){
  UseMethod("interval_end")
}
#' @export
interval_end.time_interval <- function(x){
  unclass(x)[["end"]]
}
#' @export
interval_end.Interval <- function(x){
  interval_start(x) + strip_attrs(unclass(x))
}
#' @rdname interval_utils
#' @export
interval_count <- function(x){
  UseMethod("interval_count")
}
#' @export
interval_count.time_interval <- function(x){
  new_tbl(interval = x) %>%
    fastplyr::f_count(.cols = 1L, order = TRUE)
  # intervals <- as.data.frame(x)
  # fastplyr::f_count(intervals, .cols = 1:2, order = TRUE)
}
#' @rdname interval_utils
#' @export
interval_range <- function(x, na_rm = TRUE){
  UseMethod("interval_range")
}
#' @export
interval_range.time_interval <- function(x, na_rm = TRUE){
  start <- interval_start(x)
  end <- interval_end(x)
  new_time_interval(collapse::fmin(start, na.rm = na_rm),
                    collapse::fmax(end, na.rm = na_rm))
}
#' @rdname interval_utils
#' @export
interval_length <- function(x, ...){
  UseMethod("interval_length")
}
#' @export
interval_length.time_interval <- function(x, ...){
  start <- interval_start(x)
  end <- interval_end(x)
  time_diff(start, end, ...)
}

