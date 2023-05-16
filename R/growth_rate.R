#' Fast Growth Rates
#'
#' @description Calculate the rate of percentage change per unit (time) of a numeric vector.
#' This metric is equivalent to the compound annual growth rate (CAGR): doi.org/10.1002/smr.1847
#' It is assumed that `x` is a vector of values corresponding to regular units of time with no gaps.
#' The output is to be interpreted as the average percent change per unit time.
#' This can also be calculated using the geometric mean of percent changes.
#'
#' @param x Numeric vector.
#' @param na.rm Should missing values be removed when calculating window?
#' Defaults to \code{FALSE}. When `na.rm = TRUE` the size of the rolling windows
#' are adjusted to the number of non-`NA` values in each window.
#' @param log If `TRUE` then growth rates are calculated on the log-scale.
#' @param inf_fill Numeric value to replace \code{Inf} values with.
#' Default behaviour is to keep \code{Inf} values.
#' @param n Rolling window size of length 1.
#' The default is `n = length(x)`
#' which calculates a simple prolling growth rate of `x`
#' compared to the starting value.
#' When `n = 2`, this is equivalent to a rolling basic growth calculation.
#' @param partial Should rates be calculated outwith the window
#' using partial windows? Default is \code{TRUE}.
#' @rdname growth_rate
#' @export
growth_rate <- function(x,
                        na.rm = FALSE, log = FALSE,
                        inf_fill = NULL){
  x_not_na <- collapse::whichNA(x, invert = TRUE)
  if (na.rm) x <- x[x_not_na]
  n <- length(x)
  x_n <- x[n]
  x_1 <- x[min(n, 1L)]
  if (isTRUE(x_n == 0 && x_1 == 0)) return(1)
  if (log){
    gr <- exp(( log(x_n) - log(x_1) ) / (n - 1))
  } else {
    gr <- ( (x_n / x_1) ^ (1 / (n - 1)) )
  }
  if (is.infinite(gr) && !is.null(inf_fill)){
    if (is.na(inf_fill)) inf_fill <- NA_real_
    # Any growth change from 0 is replaced with inf_fill
    gr <- inf_fill
  }
  gr
}
#' @rdname growth_rate
#' @export
rolling_growth_rate <- function(x, n = length(x),
                                partial = TRUE,
                                na.rm = FALSE,
                                log = FALSE, inf_fill = NULL){
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
