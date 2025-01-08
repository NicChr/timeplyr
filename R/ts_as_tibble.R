#' Turn `ts` into a `tibble`
#'
#' @description While a method already exists in the `tibble` package,
#' this method works differently in 2 ways:
#'
#' * The time variable associated with the time-series is also returned.
#' * The returned `tibble` is always in long format, even when the time-series
#'   is multivariate.
#'
#' @param x An object of class `ts`, `mts`, `zoo`, `xts` or `timeSeries`.
#' @param name Name of the output time column.
#' @param value Name of the output value column.
#' @param group Name of the output group column
#' when there are multiple series.
#'
#' @returns
#' A 2-column `tibble` containing the time index and values for each
#' time index. In the case where there are multiple series, this becomes
#' a 3-column `tibble` with an additional "group" column added.
#'
#' @seealso [time_ggplot]
#'
#' @examples
#' library(timeplyr)
#' library(ggplot2)
#' library(dplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # Using the examples from ?ts
#'
#' # Univariate
#' uts <- ts(cumsum(1 + round(rnorm(100), 2)),
#'           start = c(1954, 7), frequency = 12)
#' uts_tbl <- ts_as_tibble(uts)
#'
#' ## Multivariate
#' mts <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
#' mts_tbl <- ts_as_tibble(mts)
#'
#' uts_tbl %>%
#'   time_ggplot(time, value)
#'
#' mts_tbl %>%
#'   time_ggplot(time, value, group, facet = TRUE)
#'
#' # zoo example
#' x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14) - 1
#' x <- zoo::zoo(rnorm(5), x.Date)
#' ts_as_tibble(x)
#' x <- zoo::zoo(matrix(1:12, 4, 3), as.Date("2003-01-01") + 0:3)
#' ts_as_tibble(x)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname ts_as_tibble
#' @export
ts_as_tibble <- function(x, name = "time", value = "value", group = "group"){
 UseMethod("ts_as_tibble")
}
#' @rdname ts_as_tibble
#' @export
ts_as_tibble.default <- function(x, name = "time", value = "value", group = "group"){
  time <- as.vector(stats::time(x))
  ncol <- ncol(x)
  groups <- rep(colnames(x), each = length(time))
  if (!is.null(ncol)){
    if (is.null(groups)){
      groups <- rep(seq_len(ncol), each = length(time))
    }
    time <- rep(time, times = ncol)
  }
  fastplyr::new_tbl(!!group := groups,
                    !!name := time,
                    !!value := as.vector(x))
}
#' @rdname ts_as_tibble
#' @export
ts_as_tibble.mts <- ts_as_tibble.default
#' @rdname ts_as_tibble
#' @export
ts_as_tibble.xts <- function(x, name = "time", value = "value", group = "group"){
  time <- attr(x, "index")
  class_match <- match(attr(time, "tclass"), c("POSIXt", "Date"))
  is_datetime <- isTRUE(1L %in% class_match)
  is_date <- isTRUE(2L %in% class_match)
  if (is_date || is_datetime){
    tzone <- attr(time, "tzone")
    has_tz <- !is.null(tzone)
    attributes(time) <- NULL
    if (has_tz){
      time <- lubridate::as_datetime(time, tz = tzone)
    } else {
      time <- lubridate::as_datetime(time)
    }
    if (is_date){
      time <- lubridate::as_date(time)
    }
  } else {
    time <- as.numeric(time)
  }
  ncol <- ncol(x)
  groups <- rep(colnames(x), each = length(time))
  if (is.null(groups)){
    groups <- rep(seq_len(ncol), each = length(time))
  }
  time <- rep(time, times = ncol)
  out <- list(groups, time, as.vector(x))
  not_null <- cpp_list_which_not_null(out)
  out_nms <- c(group, name, value)[not_null]
  list_as_tbl(add_names(.subset(out, not_null), out_nms))
}
#' @rdname ts_as_tibble
#' @export
ts_as_tibble.zoo <- function(x, name = "time", value = "value", group = "group"){
  time <- attr(x, "index")
  ncol <- ncol(x)
  groups <- rep(colnames(x), each = length(time))
  if (!is.null(ncol)){
    if (is.null(groups)){
      groups <- rep(seq_len(ncol), each = length(time))
    }
    time <- rep(time, times = ncol)
  }
  fastplyr::new_tbl(!!group := groups,
                    !!name := time,
                    !!value := as.vector(x))
}
#' @rdname ts_as_tibble
#' @export
ts_as_tibble.timeSeries <- function(x, name = "time", value = "value", group = "group"){
  x <- unclass(x)
  time <- lubridate::as_datetime(attr(x, "positions"))
  ncol <- ncol(x)
  groups <- rep(colnames(x), each = length(time))
  if (is.null(groups)){
    groups <- rep(seq_len(ncol), each = length(time))
  }
  time <- rep(time, times = ncol)
  fastplyr::new_tbl(!!group := groups,
                    !!name := time,
                    !!value := as.vector(x))
}
