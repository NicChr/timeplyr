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
#' @return A 2-column `tibble` containing the time index and values for each
#' time index. In the case where there are multiple series, this becomes
#' a 3-column `tibble` with an additional "group" column added.
#' @seealso \link[timeplyr]{time_ggplot}
#' @examples
#' library(timeplyr)
#' library(ggplot2)
#' library(dplyr)
#'
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
#' \dontrun{
#' # xts example
#' data(sample_matrix, package = "xts")
#' sample.xts <- xts::as.xts(sample_matrix)
#' sample.xts %>%
#'   ts_as_tibble() %>%
#'   time_ggplot(time, value, group)
#' # zoo example
#' x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14) - 1
#' x <- zoo::zoo(rnorm(5), x.Date)
#' ts_as_tibble(x)
#' x <- zoo::zoo(matrix(1:12, 4, 3), as.Date("2003-01-01") + 0:3)
#' ts_as_tibble(x)
#' # timeSeries example
#' timeSeries::MSFT %>%
#'   ts_as_tibble() %>%
#'   time_ggplot(time, value, group, facet = TRUE)
#' }
#' @export
#' @rdname ts_as_tibble
ts_as_tibble <- function(x, name = "time", value = "value", group = "group"){
 UseMethod("ts_as_tibble")
}
#' @export
#' @rdname ts_as_tibble
ts_as_tibble.default <- function(x, name = "time", value = "value", group = "group"){
  tsp <- stats::tsp(stats::hasTsp(x))
  start <- tsp[1L]
  end <- tsp[2L]
  freq <- tsp[3L]
  time <- seq(from = start,
              to = end,
              by = 1/freq)
  groups <- NULL
  ncol <- ncol(x)
  if (!is.null(ncol)){
    groups <- rep(colnames(x), each = length(time))
    if (is.null(groups)){
      groups <- rep(seq_len(ncol), each = length(time))
    }
    time <- rep(time, times = ncol)
  }
  dplyr::tibble(!!group := groups,
                !!name := time,
                !!value := as.vector(x))
}
#' @export
#' @rdname ts_as_tibble
ts_as_tibble.mts <- function(x, name = "time", value = "value", group = "group"){
  tsp <- stats::tsp(stats::hasTsp(x))
  start <- tsp[1L]
  end <- tsp[2L]
  freq <- tsp[3L]
  time <- seq(from = start,
              to = end,
              by = 1/freq)
  ncol <- ncol(x)
  groups <- rep(colnames(x), each = length(time))
  if (is.null(groups)){
    groups <- rep(seq_len(ncol), each = length(time))
  }
  time <- rep(time, times = ncol)
  dplyr::tibble(!!group := groups,
                !!name := time,
                !!value := as.vector(x))
}
#' @export
#' @rdname ts_as_tibble
ts_as_tibble.xts <- function(x, name = "time", value = "value", group = "group"){
  time <- attr(x, "index")
  class_match <- match(attr(time, "tclass"), c("POSIXt", "Date"))
  is_datetime <- isTRUE(1L %in% class_match)
  is_date <- isTRUE(2L %in% class_match)
  if (is_date || is_datetime){
    if (!is.null(attr(time, "tzone"))){
      time <- lubridate::as_datetime(time, tz = attr(time, "tzone"))
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
  dplyr::tibble(!!group := groups,
                !!name := time,
                !!value := as.vector(x))
}
#' @export
#' @rdname ts_as_tibble
ts_as_tibble.zoo <- function(x, name = "time", value = "value", group = "group"){
  time <- attr(x, "index")
  ncol <- ncol(x)
  groups <- NULL
  if (!is.null(ncol)){
    groups <- rep(colnames(x), each = length(time))
    if (is.null(groups)){
      groups <- rep(seq_len(ncol), each = length(time))
    }
    time <- rep(time, times = ncol)
  }
  dplyr::tibble(!!group := groups,
                !!name := time,
                !!value := as.vector(x))
}
#' @export
#' @rdname ts_as_tibble
ts_as_tibble.timeSeries <- function(x, name = "time", value = "value", group = "group"){
  x <- unclass(x)
  time <- lubridate::as_datetime(attr(x, "positions"))
  ncol <- ncol(x)
  groups <- rep(colnames(x), each = length(time))
  if (is.null(groups)){
    groups <- rep(seq_len(ncol), each = length(time))
  }
  time <- rep(time, times = ncol)
  dplyr::tibble(!!group := groups,
                !!name := time,
                !!value := as.vector(x))
}
# ts_as_tibble <- function(x, name = "time", value = "value", group = "group"){
#   tsp <- stats::tsp(stats::hasTsp(x))
#   start <- tsp[1L]
#   end <- tsp[2L]
#   freq <- tsp[3L]
#   time <- seq(from = start,
#               to = end,
#               by = 1/freq)
#   groups <- NULL
#   # If multivariate, repeat time sequence and group names
#   if (inherits(x, "mts")){
#     ncol <- ncol(x)
#     groups <- rep(colnames(x), each = length(time))
#     if (is.null(groups)){
#       groups <- rep(seq_len(ncol), each = length(time))
#     }
#     time <- rep(time, times = ncol)
#   } else if (inherits(x, "xts")){
#     time <- attr(x, "index")
#     if ("POSIXt" %in% attr(time, "tclass")){
#       if (!is.null(attr(time, "tzone"))){
#         time <- lubridate::as_datetime(time,
#                                        tz = attr(time, "tzone"))
#       } else {
#         time <- lubridate::as_datetime(time)
#       }
#     } else if ("Date" %in% attr(time, "tclass")){
#       time <- lubridate::as_date(time)
#     } else {
#       time <- as.numeric(time)
#     }
#     ncol <- ncol(x)
#     groups <- rep(colnames(x), each = length(time))
#     if (is.null(groups)){
#       groups <- rep(seq_len(ncol), each = length(time))
#     }
#     time <- rep(time, times = ncol)
#   } else if (inherits(x, "timeSeries")){
#     x <- unclass(x)
#     time <- lubridate::as_datetime(attr(x, "positions"))
#     ncol <- ncol(x)
#     groups <- rep(colnames(x), each = length(time))
#     if (is.null(groups)){
#       groups <- rep(seq_len(ncol), each = length(time))
#     }
#     time <- rep(time, times = ncol)
#   } else if (inherits(x, "zoo")){
#     time <- attr(x, "index")
#     ncol <- ncol(x)
#     if (!is.null(ncol)){
#       groups <- rep(colnames(x), each = length(time))
#       if (is.null(groups)){
#        groups <- rep(seq_len(ncol), each = length(time))
#       }
#       time <- rep(time, times = ncol)
#     }
#   }
#   dplyr::tibble(!!group := groups,
#                 !!name := time,
#                 !!value := as.vector(x))
# }
