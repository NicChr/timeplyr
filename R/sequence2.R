#' Extension to `base::sequence()` that handles decimals
#' @param nvec Vector of sequence lengths.
#' @param from Start of sequence(s).
#' @param by Unit increment of sequence(s).
#' @examples
#' library(timeplyr)
#' sequence(1:3)
#' sequence2(1:3)
#'
#' sequence(1:3, by = 0.1)
#' sequence2(1:3, by = 0.1)
#'
#' sequence(c(3, 2), by = c(-0.1, 0.1))
#' sequence2(c(3, 2), by = c(-0.1, 0.1))
#' @export
sequence2 <- function(nvec, from = 1, by = 1){
  out_len <- sum(nvec)
  g_len <- length(nvec)
  # Recycle
  by <- rep_len(by, g_len)
  from <- rep_len(from, g_len)
  # Expand
  by <- rep(by, times = nvec)
  from <- rep(from, times = nvec)
  # Arithmetic
  g_add <- sequence(nvec, from = 1L, by = 1L) - 1L
  from + (g_add * by)
}
