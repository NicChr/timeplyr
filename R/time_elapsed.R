#' Fast grouped time elapsed
#'
#' @description Calculate how much time has passed
#' on a rolling or cumulative basis. \cr
#'
#' @param x Time vector. \cr
#' E.g. a `Date`, `POSIXt`, `numeric` or any time-based vector.
#' @param timespan [timespan].
#' @param g Object to be used for grouping `x`, passed onto `collapse::GRP()`.
#' @param rolling If `TRUE` (the default) then lagged
#' time differences are calculated on a rolling basis,
#' essentially like `diff()`. \cr
#' If `FALSE` then time differences compared to the index (first) time
#' are calculated.
#' @param fill When `rolling = TRUE`, this is the value that fills
#' the first elapsed time. The default is `NA`.
#' @param na_skip Should `NA` values be skipped? Default is `TRUE`.
#'
#' @details
#' `time_elapsed()` is quite efficient when there are many groups,
#' especially if your data is sorted in order of those groups.
#' In the case that `g` is supplied, it is most efficient when your data is
#' sorted by `g` .
#' When `na_skip` is `TRUE` and `rolling` is also `TRUE`, `NA` values are simply
#' skipped and hence the time differences between the current value and the
#' previous non-NA value are calculated. For example,
#' `c(3, 4, 6, NA, NA, 9)` becomes `c(NA, 1, 2, NA, NA, 3)`. \cr
#' When `na_skip` is `TRUE` and `rolling` is `FALSE`, time differences between
#' the current value and the first non-NA value of the series are calculated.
#' For example,
#' `c(NA, NA, 3, 4, 6, NA, 8)` becomes `c(NA, NA, 0, 1, 3, NA, 5)`.
#'
#' @returns
#' A numeric vector the same length as `x`.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' x <- time_seq(today(), length.out = 25, time = "3 days")
#' time_elapsed(x)
#' time_elapsed(x, "days", rolling = FALSE)
#'
#' # Grouped example
#' set.seed(99)
#' g <- sample.int(3, 25, TRUE)
#'
#' time_elapsed(x, "days", g = g)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#' }
#' @export
time_elapsed <- function(x, timespan = granularity(x), g = NULL,
                         rolling = TRUE, fill = NA,
                         na_skip = TRUE){
  check_is_time_or_num(x)
  timespan <- timespan(timespan)
  check_length(fill, 1)
  needs_fill <- !is.na(fill)
  has_groups <- !is.null(g)
  if (rolling){
    g <- GRP2(g, sort = TRUE, return.groups = FALSE, return.order = TRUE)
    o <- GRP_order(g)
    if (has_groups){
      sorted_group_starts <- attr(o, "starts")
    } else {
      sorted_group_starts <- min(length(x), 1L)
    }
    if (na_skip){
      x_filled <- roll_na_fill(x, g = g)
      x_lag <- roll_lag(x_filled, g = g)
      if (needs_fill){
        sorted_group_starts <- sorted_group_starts +
          fnmiss(x_lag, g = g, use.g.names = FALSE) - 1L
      }
    } else {
      x_lag <- roll_lag(x, g = g)
    }
    out <- time_diff(x_lag, x, timespan)
    if (needs_fill){
      if (has_groups){
        out[o[sorted_group_starts]] <- fill
      } else {
        out[sorted_group_starts] <- fill
      }
    }
  } else {
    g <- GRP2(g, sort = TRUE, return.groups = FALSE, return.order = FALSE)
    # Index time
    first_time <- gfirst(x, g = g, na.rm = na_skip)
    out <- time_diff(first_time, x, timespan)
  }
  out
}

