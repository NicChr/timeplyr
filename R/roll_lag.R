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
#' @details This function is simple and fast. It should generally be equivalent
#' to
#' `diag(matrix(unlist(data.table::shift(x, lag)), ncol = length(x)))`.
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
# lagged_indices <- seq_along(x)
# collapse::setop(lagged_indices, "-", lag)
# setv(lagged_indices, which(lagged_indices < 1L), N + 1L,
#      vind1 = TRUE)
#' @rdname roll_lag
#' @export
lag_seq <- function(x, lag = 1L){
  n <- length(x)
  if (length(lag) != 1L) stop("lag must be of length 1.")
  lag <- min(lag, n) # Bound lag to <= n
  lag <- max(lag, 0L) # Bound lag to >= 0
  p_seq <- seq_len(lag) # Partial window sequence
  out <- rep_len(lag, n)
  # Replace partial part with partial sequence
  out[p_seq] <- NA_integer_
  out
}
