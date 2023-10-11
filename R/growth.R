#' Rolling basic growth
#'
#' @description Calculate basic growth calculations on a rolling basis.
#' `growth()` calculates the percent change between the totals of two numeric vectors
#' when they're of equal length, otherwise the percent change between the means.
#' `rolling_growth()` does the same calculation on 1 numeric vector, on a rolling basis.
#' Pairs of windows of length `n`, lagged by the value specified by `lag` are compared in
#' a similar manner.
#' When `lag = n` then `data.table::frollsum()` is used,
#' otherwise `data.table::frollmean()` is used.
#'
#' @param x Numeric vector.
#' @param y numeric vector
#' @param n Rolling window size, default is 1.
#' @param lag Lag of basic growth comparison, default is the rolling window size.
#' @param partial Should rates be calculated outwith the window using partial windows?
#' If `TRUE` (the default), (n - 1) pairs of equally-sized rolling windows are compared,
#' their size increasing by 1 up to size n, at which point the rest of the window pairs are
#' all of size n. If `FALSE` all window-pairs will be of size n.
#' @param offset Numeric vector of values to use as offset, e.g. population sizes or exposure times.
#' @param weights Importance weights. These can either be
#' length 1 or the same length as x.
#' Currently, no normalisation of weights occurs.
#' @param na.rm Should missing values be removed when calculating window? Defaults to \code{FALSE}.
#' @param inf_fill Numeric value to replace \code{Inf} values with. Default behaviour is to keep \code{Inf} values.
#' @param log If `TRUE` Growth (relative change) in total and mean events will be
#'  calculated on the log-scale.
#' @param ... Further arguments to be passed on to `frollmean`.
#'
#' @returns
#' `growth` returns a `numeric(1)` and `rolling_growth`
#' returns a `numeric(length(x))`.
#'
#' @examples
#' library(timeplyr)
#' \dontshow{
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' set.seed(42)
#' # Growth rate is 6% per day
#' x <- 10 * (1.06)^(0:25)
#'
#' # Simple growth from one day to the next
#' rolling_growth(x, n = 1)
#'
#' # Growth comparing rolling 3 day cumulative
#' rolling_growth(x, n = 3)
#'
#' # Growth comparing rolling 3 day cumulative, lagged by 1 day
#' rolling_growth(x, n = 3, lag = 1)
#'
#' # Growth comparing windows of equal size
#' rolling_growth(x, n = 3, partial = FALSE)
#'
#' # Seven day moving average growth
#' roll_mean(rolling_growth(x), window = 7, partial = FALSE)
#'
#' @rdname growth
#' @export
growth <- function(x, y, na.rm = FALSE, log = FALSE, inf_fill = NULL){
  x_len <- length(x)
  y_len <- length(y)
  if (x_len == y_len && !na.rm){
    numerator <- sum(y)
    denominator <- sum(x)
  } else {
    numerator <- mean(y, na.rm = na.rm)
    denominator <- mean(x, na.rm = na.rm)
  }
  if (numerator == 0 && denominator == 0) return(1)
  if (log){
    gr <- exp(( log(numerator) - log(denominator)))
  } else {
    gr <- ((numerator / denominator))
  }
  if (is.infinite(gr) && !is.null(inf_fill)){
    if (is.na(inf_fill)) inf_fill <- NA_real_
    # Any growth change from 0 is replaced with inf_fill
    gr <- inf_fill
  }
  gr
}
#' @rdname growth
#' @export
rolling_growth <- function(x, n = 1, lag = n, na.rm = FALSE, partial = TRUE,
                           offset = NULL,
                           weights = NULL,
                           inf_fill = NULL, log = FALSE, ...){
  stopifnot(length(n) == 1)
  stopifnot(length(lag) == 1)
  stopifnot(n >= 1)
  n <- min(length(x), n)
  lag <- min(length(x), lag)
  if (!na.rm && is.null(weights) && is.null(offset)){
    roll <- function(...) frollsum3(...)
  } else {
    roll <- function(...) frollmean3(...)
  }
  x_na <- which(is.na(x))
  if (!is.null(weights)){
    if (length(x_na) > 0L) weights[x_na] <- NA_real_
  }

  if (!is.null(offset)){
    if (length(x_na) > 0L) offset[x_na] <- NA_real_
  }
  if (partial){
    window <- window_seq(k = n, n = length(x), partial = TRUE)
    # Partial window is shifted according to lag value
    window_lagged <- collapse::flag(window, n = lag)
    # Running mean with lagged partial window
    numerator <- roll(x = x, n = window_lagged, weights = weights,
                      align = "right",
                      na.rm = na.rm, adaptive = TRUE, ...)
    offset_numerator <- roll(x = offset, n = window_lagged, weights = weights,
                             align = "right",
                             na.rm = na.rm, adaptive = TRUE, ...)
    # Lagged running mean as denominator
    denominator <- collapse::flag(roll(x = x, n = window, weights = weights,
                                       align = "right", na.rm = na.rm,
                                       adaptive = TRUE, ...),
                                  n = lag)
    offset_denominator <- data.table::shift(roll(x = offset, n = window, weights = weights,
                                              align = "right", na.rm = na.rm,
                                              adaptive = TRUE, ...), n = lag,
                                            type = "lag")
  } else {
    numerator <- roll(x = x, n = n, weights = weights,
                      align = "right", na.rm = na.rm,
                      adaptive = FALSE, ...)
    denominator <- collapse::flag(numerator, n = lag)
    offset_numerator <- roll(x = offset, n = n, weights = weights,
                             align = "right", na.rm = na.rm,
                             adaptive = FALSE, ...)
    offset_denominator <- data.table::shift(offset_numerator, n = lag, type = "lag")
  }
  if (log){
    if (!is.null(offset)){
      numerator <- log(numerator) - log(offset_numerator)
      denominator <- log(denominator) - log(offset_denominator)
    } else {
      numerator <- log(numerator)
      denominator <- log(denominator)
    }
    # Growth of value compared to lagged value
    growth <- exp(numerator - denominator)
  } else {
    if (!is.null(offset)){
      numerator <- numerator / offset_numerator
      denominator <- denominator / offset_denominator
    }
    # Growth of value compared to lagged value
    growth <- numerator / denominator
    # 0/0 = NaN and assume 0 to 0 events represents no growth, i.e GR = 1.
    setv(growth, which(numerator == 0 & denominator == 0), 1, vind1 = TRUE)
  }
  # NA/0 remains NA
  if (!is.null(inf_fill)){
    if (is.na(inf_fill)) inf_fill <- NA_real_
    # Any growth change from 0 is replaced with inf_fill
    setv(growth, which(is.infinite(growth)), inf_fill, vind1 = TRUE)
  }
  growth
}
# roll_growth <- function(x, window = 1,
#                         lag = window, na.rm = FALSE, partial = TRUE,
#                         offset = NULL,
#                         weights = NULL,
#                         inf_fill = NULL, log = FALSE, ...){
#   if (length(window) != 1L){
#     stop("window must be of length 1")
#   }
#   if (window < 1){
#     stop("window must be >= 1")
#   }
#   if (length(lag) != 1L){
#     stop("lag must be of length 1")
#   }
#   n <- min(length(x), window)
#   lag <- min(length(x), lag)
#   if (!na.rm && is.null(weights) && is.null(offset)){
#     roll <- function(...) frollsum3(...)
#   } else {
#     roll <- function(...) frollmean3(...)
#   }
#   has_na <- anyNA(x)
#   if (has_na){
#     which_na <- which(is.na(x))
#   }
#   if (!is.null(weights) && has_na){
#     weights[x_na] <- NA_real_
#   }
#   if (!is.null(offset) && has_na){
#     offset[x_na] <- NA_real_
#   }
#   if (partial){
#     window <- window_seq(k = n, n = length(x), partial = TRUE)
#     # Partial window is shifted according to lag value
#     window_lagged <- collapse::flag(window, n = lag)
#     # Running mean with lagged partial window
#     numerator <- roll(x = x, n = window_lagged, weights = weights,
#                       align = "right",
#                       na.rm = na.rm, adaptive = TRUE, ...)
#     offset_numerator <- roll(x = offset, n = window_lagged, weights = weights,
#                              align = "right",
#                              na.rm = na.rm, adaptive = TRUE, ...)
#     # Lagged running mean as denominator
#     denominator <- collapse::flag(roll(x = x, n = window, weights = weights,
#                                        align = "right", na.rm = na.rm,
#                                        adaptive = TRUE, ...),
#                                   n = lag)
#     offset_denominator <- flag2(roll(x = offset, n = window, weights = weights,
#                                      align = "right", na.rm = na.rm,
#                                      adaptive = TRUE, ...), n = lag)
#   } else {
#     numerator <- roll(x = x, n = n, weights = weights,
#                       align = "right", na.rm = na.rm,
#                       adaptive = FALSE, ...)
#     denominator <- collapse::flag(numerator, n = lag)
#     offset_numerator <- roll(x = offset, n = n, weights = weights,
#                              align = "right", na.rm = na.rm,
#                              adaptive = FALSE, ...)
#     offset_denominator <- flag2(offset_numerator, n = lag)
#   }
#   if (log){
#     if (!is.null(offset)){
#       numerator <- log(numerator) - log(offset_numerator)
#       denominator <- log(denominator) - log(offset_denominator)
#     } else {
#       numerator <- log(numerator)
#       denominator <- log(denominator)
#     }
#     # Growth of value compared to lagged value
#     growth <- exp(numerator - denominator)
#   } else {
#     if (!is.null(offset)){
#       numerator <- numerator / offset_numerator
#       denominator <- denominator / offset_denominator
#     }
#     # Growth of value compared to lagged value
#     growth <- numerator / denominator
#     # 0/0 = NaN and assume 0 to 0 events represents no growth, i.e GR = 1.
#     growth[which(numerator == 0 & denominator == 0)] <- 1
#   }
#   # NA/0 remains NA
#   if (!is.null(inf_fill)){
#     growth[is.infinite(growth)] <- inf_fill
#   }
#   growth
# }
