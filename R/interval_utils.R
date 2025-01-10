#' Time interval utilities
#'
#' @param x A [time_interval].
#'
#' @seealso [time_interval]
#'
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

