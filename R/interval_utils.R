#' Time interval utilities
#'
#' @param x A [time_interval].
#'
#' @seealso [time_interval]
#'
#' @rdname interval_utils
#' @export
interval_start <- function(x){
  UseMethod("interval_start")
}
#' @export
interval_start.time_interval <- function(x){
  out <- x
  attr(out, "timespan") <- NULL
  attr(out, "old_class") <- NULL
  class(out) <- attr(x, "old_class")
  out
}
#' @export
interval_start.Interval <- function(x){
  attr(x, "start", TRUE)
}
#' @rdname interval_utils
#' @export
interval_width <- function(x){
  UseMethod("interval_width")
}
#' @export
interval_width.time_interval <- function(x){
  attr(x, "timespan")
}
#' @rdname interval_utils
#' @export
interval_end <- function(x){
  UseMethod("interval_end")
}
#' @export
interval_end.time_interval <- function(x){
 time_add(interval_start(x), interval_width(x))

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
    fastplyr::f_count(.cols = 1L, .order = TRUE)
}
interval_range <- function(x){
  rng <- collapse::frange(x, na.rm = TRUE)
  c(interval_start(rng[1]), interval_end(rng[2]))
}

