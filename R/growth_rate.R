#' Fast Growth Rates
#'
#' @description Calculate the rate of percentage
#' change per unit time.
#'
#' @param x Numeric vector.
#' @param na.rm Should missing values be removed when calculating window?
#' Defaults to \code{FALSE}. When `na.rm = TRUE` the size of the rolling windows
#' are adjusted to the number of non-`NA` values in each window.
#' @param log If `TRUE` then growth rates are calculated on the log-scale.
#' @param inf_fill Numeric value to replace \code{Inf} values with.
#' Default behaviour is to keep \code{Inf} values.
#' @param window Size of the rolling window.
#' This must be of length 1 or the same length as x.
#' The default calculates a simple rolling growth rate of `x`
#' compared to the first value.
#' You can specify a vector of window sizes as well, allowing for
#' a flexible calculation.
#' @param partial Should rates be calculated outwith the window
#' using partial windows? Default is \code{TRUE}.
#' @param n Window size (\bold{deprecated}).
#' @return `growth_rate` returns a `numeric(1)` and `roll_growth_rate`
#' returns a `numeric(length(x))`.
#' @details
#' This metric is equivalent to the compound annual growth rate (CAGR): doi.org/10.1002/smr.1847
#' It is assumed that `x` is a vector of values with
#' a corresponding time index that increases regularly with no gaps or missing values.
#' The output is to be interpreted as the average percent change per unit time.
#' This can also be calculated using the geometric mean of percent changes.
#' `growth_rate` calculates the growth rate of a numeric vector by comparing
#' the first and last values while `roll_growth_rate` does the same but on a
#' rolling basis.
#' The identity \cr `tail(roll_growth_rate(x, window = length(x)), 1) == growth_rate(x)`
#' should always hold.
#' @rdname growth_rate
#' @export
growth_rate <- function(x,
                        na.rm = FALSE, log = FALSE,
                        inf_fill = NULL){
  if (na.rm){
    x <- collapse::na_rm(x)
  }
  n <- length(x)
  x_n <- x[n]
  x_1 <- x[min(n, 1L)]
  if (isTRUE(x_n == 0 && x_1 == 0)) return(1)
  if (log){
    gr <- exp(( log(x_n) - log(x_1) ) / (n - 1))
  } else {
    gr <- ( (x_n / x_1) ^ (1 / (n - 1)) )
  }
  if (!is.null(inf_fill)){
    gr[is.infinite(gr)] <- inf_fill
  }
  gr
}
#' @rdname growth_rate
#' @export
roll_growth_rate <- function(x, window = length(x),
                             partial = TRUE,
                             na.rm = FALSE,
                             log = FALSE,
                             inf_fill = NULL){
  x_len <- length(x)
  window_len <- length(window)
  if (!window_len %in% c(1L, x_len)){
    stop("window must be of length 1 or length(x)")
  }
  window[window > x_len] <- x_len
  window <- as.integer(window)
  window[window < 1L] <- 1L
  if (length(window) == 0L){
    window <- 1L
  }
  if (length(window) > 1L){
    x_lagged <- roll_lag(x, lag = window - 1L, check = FALSE)
  } else {
    x_lagged <- collapse::flag(x, n = window - 1L)
    if (partial){
      x_lagged[seq_len(window)] <- x[min(x_len, 1L)]
      window <- window_sequence(x_len, k = window,
                                partial = TRUE, ascending = TRUE)
    }
  }
  if (na.rm){
    window <- data.table::frollsum(!is.na(x), n = window,
                                   adaptive = partial,
                                   algo = "fast",
                                   align = "right")
  }
  if (log){
    window_denom <- (window - 1L)
    gr <- exp(( log(x) - log(x_lagged) ) / window_denom)
    gr[which(window_denom == 0L)] <- 1
  } else {
    gr <- ( (x / x_lagged) ^ (1 / (window - 1L)) )
    gr[which(x == 0 & x_lagged == 0)] <- 1
  }
  if (!is.null(inf_fill)){
    # Any growth change from 0 is replaced with inf_fill
    gr[is.infinite(gr)] <- inf_fill
  }
  gr
}
#' @rdname growth_rate
#' @export
rolling_growth_rate <- function(x, n = length(x),
                                partial = TRUE,
                                na.rm = FALSE,
                                log = FALSE, inf_fill = NULL){
  .Deprecated("roll_growth_rate")
  x_len <- length(x)
  n_len <- length(n)
  stopifnot(n_len <= 1)
  n <- as.integer(n)
  n <- max(1L, n)
  n <- min(x_len, n)
  lag <- max(0L, n - 1L)
  # if (!isTRUE(n_len == 1 || n_len == x_len)){
  #   stop("n must be of length 1 or length(x)")
  # }
  # if (n_len == x_len){
  #   x_lagged <- runner::lag_run(x, lag = n - 1)
  # } else
  if (partial){
    x_lagged <- collapse::flag(x, n = lag)
    setv(x_lagged, seq_len(n), vec_head(x), vind1 = TRUE)
    n <- window_seq(k = n, n = x_len, partial = TRUE)
    # x_lagged <- runner::lag_run(x, lag = n - 1)
  } else {
    x_lagged <- collapse::flag(x, n = lag)
    n <- rep_len(n, x_len)
  }
  if (na.rm){
    n <- frollsum3(!is.na(x), n = n, adaptive = TRUE)
  }
  if (log){
    gr <- exp(( log(x) - log(x_lagged) ) / (n - 1))
    setv(gr, which((n - 1) == 0), 1, vind1 = TRUE)
  } else {
    gr <- ( (x / x_lagged) ^ (1 / (n - 1)) )
    setv(gr, which(x == 0 & x_lagged == 0), 1, vind1 = TRUE)
  }
  which_inf <- which(is.infinite(gr))
  if (length(which_inf) > 0 && !is.null(inf_fill)){
    if (is.na(inf_fill)) inf_fill <- NA_real_
    # Any growth change from 0 is replaced with inf_fill
    setv(gr, which_inf, inf_fill, vind1 = TRUE)
  }
  gr
}
# Working fast function that accepts a window arg of length 1
# roll_growth_rate2 <- function(x, window = length(x),
#                              partial = TRUE,
#                              na.rm = FALSE,
#                              log = FALSE,
#                              inf_fill = NULL){
#   x_len <- length(x)
#   window_len <- length(window)
#   if (window_len != 1L){
#     stop("window must be of length 1")
#   }
#   window <- as.integer(window)
#   window <- max(1L, window)
#   window <- min(x_len, window)
#   lag <- max(0L, window - 1L)
#   x_lagged <- collapse::flag(x, n = lag)
#   if (partial){
#     x_lagged[seq_len(window)] <- x[min(length(x), 1L)]
#     window <- window_sequence(x_len, k = window,
#                               partial = TRUE, ascending = TRUE)
#   }
#   if (na.rm){
#     window <- data.table::frollsum(!is.na(x), n = window,
#                                    adaptive = partial,
#                                    algo = "fast",
#                                    align = "right")
#   }
#   if (log){
#     window_denom <- (window - 1L)
#     gr <- exp(( log(x) - log(x_lagged) ) / window_denom)
#     gr[which(window_denom == 0L)] <- 1
#   } else {
#     gr <- ( (x / x_lagged) ^ (1 / (window - 1L)) )
#     gr[which(x == 0 & x_lagged == 0)] <- 1
#   }
#   if (!is.null(inf_fill)){
#     # Any growth change from 0 is replaced with inf_fill
#     gr[is.infinite(gr)] <- inf_fill
#   }
#   gr
# }
