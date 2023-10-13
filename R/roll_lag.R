#' Fast rolling lag
#'
#' @description An efficient rolling lag.
#'
#' `lag_seq()` is a helper that creates a safe vector of lags.
#'
#' @param x A vector.
#' @param lag A vector of lags to be applied to `x` on a rolling basis.
#' @param check Should safety checks be done for the specified lag  against `x`?
#'
#' The default is `TRUE` but
#' you can set this to `FALSE` if you need this to be speed-performant.
#'
#' @details
#' This function is simple and fast. It should generally be equivalent to
#' `diag(matrix(unlist(data.table::shift(x, lag)), ncol = length(x)))`. \cr
#' where `length(lag) = length(x)`.
#'
#' @returns
#' A lagged vector of `x` the same length as `x`.
#'
#' @examples
#' library(timeplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' x <- 1:10
#'
#' roll_lag(x, 2) # Lag
#' roll_lag(x, -2) # Lead
#'
#'
#' roll_lag(x, lag_seq(x, 2), check = FALSE) # Lag
#' roll_lag(x, lag_seq(x, -2), check = FALSE) # Lead
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname roll_lag
#' @export
roll_lag <- function(x, lag = 1L, check = TRUE){
  if (check){
    N <- length(x)
    lag_size <- length(lag)
    if (length(lag) == 1L && N == 0L){
      lag <- lag[0L]
    }
    if (length(lag) != 1L && length(lag) != N){
      stop("lag must be of length 1 or length(x)")
    }
  }
  lagged_indices <- seq_along(x) - lag
  if (check){
    lagged_indices[lagged_indices < 1L] <- NA_integer_
  }
  x[lagged_indices]
}
#' @rdname roll_lag
#' @export
lag_seq <- function(x, lag = 1L){
  check_length(lag, 1L)
  if (lag >= 0){
    lag_sequence(length(x), k = lag)
  } else {
    -lead_sequence(length(x), k = -lag)
  }
}
