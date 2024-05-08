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
    fcount(.cols = 1L, order = TRUE)
  # intervals <- as.data.frame(x)
  # fcount(intervals, .cols = 1:2, order = TRUE)
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
# interval_overlaps.time_interval <- function(x, na_rm = TRUE){
#   interval_tbl <- as.data.frame(x)
#   interval_dt <- df_as_dt(interval_tbl, .copy = FALSE)
#   group_id <- group2(x, starts = TRUE)
#   # set_add_cols(interval_dt, add_names(
#   #   list(group2(x, starts = TRUE)),
#   #   ".group.id"
#   # ))
#   unique_dt <- interval_dt[attr(group_id, "starts")]
#   joined_dt <- dplyr::cross_join(unique_dt, unique_dt)
#
#   setkeyv2(unique_dt, c("start", "end"))
#   data.table::foverlaps(unique_dt, unique_dt)
# }

