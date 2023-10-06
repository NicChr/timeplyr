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
#' @seealso [time_roll_growth_rate]
#' @examples
#' library(timeplyr)
#'
#' set.seed(42)
#' initial_investment <- 100
#' years <- 1990:2000
#' # Assume a rate of 8% increase with noise
#' relative_increases <- 1.08 + rnorm(10, sd = 0.005)
#'
#' assets <- Reduce(`*`, relative_increases, init = initial_investment, accumulate = TRUE)
#' assets
#'
#' # Note that this is approximately 8%
#' growth_rate(assets)
#'
#' # We can also calculate the growth rate via geometric mean
#'
#' rel_diff <- exp(diff(log(assets)))
#' all.equal(rel_diff, relative_increases)
#'
#' geometric_mean(rel_diff) == growth_rate(assets)
#'
#' # Weighted growth rate
#'
#' w <- c(rnorm(5)^2, rnorm(5)^4)
#' geometric_mean(rel_diff, weights = w)
#'
#' # Rolling growth rate over the last n years
#' roll_growth_rate(assets)
#'
#' # The same but using geometric means
#' exp(roll_mean(log(c(NA, rel_diff))))
#'
#' # Rolling growth rate over the last 5 years
#' roll_growth_rate(assets, window = 5)
#' roll_growth_rate(assets, window = 5, partial = FALSE)
#'
#' ## Rolling growth rate with gaps in time
#'
#' years2 <- c(1990, 1993, 1994, 1997, 1998, 2000)
#' assets2 <- assets[years %in% years2]
#'
#' # Below does not incorporate time gaps into growth rate calculation
#' # But includes helpful warning
#' time_roll_growth_rate(assets2, window = 5, time = years2)
#' # Time step allows us to calculate correct rates across time gaps
#' time_roll_growth_rate(assets2, window = 5, time = years2, time_step = 1) # Time aware
#'
#' @rdname growth_rate
#' @export
growth_rate <- function(x, na.rm = FALSE, log = FALSE, inf_fill = NULL){
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
roll_growth_rate <- function(x, window = Inf,
                             partial = TRUE,
                             na.rm = FALSE,
                             log = FALSE,
                             inf_fill = NULL){
  x_len <- length(x)
  window_len <- length(window)
  if (!window_len %in% c(1L, x_len)){
    stop("window must be of length 1 or length(x)")
  }
  window[window > x_len] <- x_len + 1L
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
      x_lagged[seq_len(min(window, x_len))] <- x[min(x_len, 1L)]
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
