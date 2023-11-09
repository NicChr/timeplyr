#' Fast rolling lag
#'
#' @description An efficient rolling lag.
#'
#' `lag_seq()` is a helper that creates a safe vector of lags.
#'
#' @param x A vector.
#' @param lag A vector of lags to be applied to `x` on a rolling basis.
#' @param check Should safety checks be done for the specified lag  against `x`?
#' @param size Length of sequence.
#' @param partial If `TRUE`, a partial lag sequence is created. The default
#' is `FALSE`.
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
#' roll_lag(x, lag_seq(length(x), 2), check = FALSE) # Lag
#' roll_lag(x, lag_seq(length(x), -2), check = FALSE) # Lead
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname roll_lag
#' @export
roll_lag <- function(x, lag = 1L, check = TRUE){
  UseMethod("roll_lag")
}
#' @export
roll_lag.default <- function(x, lag = 1L, check = TRUE){
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
    lagged_indices[cpp_which(lagged_indices < 1L)] <- NA_integer_
  }
  x[lagged_indices]
}
#' @export
roll_lag.data.frame <- function(x, lag = 1L, check = TRUE){
  if (check){
    N <- df_nrow(x)
    lag_size <- length(lag)
    if (length(lag) == 1L && N == 0L){
      lag <- lag[0L]
    }
    if (length(lag) != 1L && length(lag) != N){
      stop("lag must be of length 1 or nrow(x)")
    }
  }
  lagged_indices <- df_seq_along(x) - lag
  if (check){
    lagged_indices[cpp_which(lagged_indices < 1L)] <- NA_integer_
  }
  df_row_slice(x, lagged_indices)
}
#' @rdname roll_lag
#' @export
lag_seq <- function(size, lag = 1L, partial = FALSE){
  check_length(lag, 1L)
  if (lag >= 0){
    lag_sequence(size, k = lag, partial = partial)
  } else {
    -lead_sequence(size, k = -lag, partial = partial)
  }
}
