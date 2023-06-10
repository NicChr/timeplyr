#' Extension to `base::sequence()` that handles decimals
#'
#' `seq_v()` is a vectorised version of `seq()` that strictly accepts
#' only the arguments `from`, `to` and `by`.
#' `seq_id()` is a helper function to efficiently return unique IDs for
#' each sequence.
#'
#' @param nvec Vector of sequence lengths.
#' @param from Start of sequence(s).
#' @param to End of sequence(s).
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
#' @rdname sequence2
#' @export
sequence2 <- function(nvec, from = 1L, by = 1L){
  out_len <- sum(nvec)
  out_is_int <- out_len <= .Machine$integer.max
  if (is.integer(from) &&
      is.integer(by) &&
      out_is_int){
    return(sequence(nvec = nvec, from = from, by = by))
  }
  g_len <- length(nvec)
  if (length(from) > 1L){
    # Recycle
    from <- rep_len(from, g_len)
    # Expand
    from <- rep.int(from, times = nvec)
  }
  if (length(by) > 1L){
    # Recycle
    by <- rep_len(by, g_len)
    # Expand
    by <- rep.int(by, times = nvec)
  }
  # Arithmetic
  if (out_is_int){
    g_add <- sequence(nvec, from = 0L, by = 1L)
  } else {
    g <- seq_id(nvec)
    g_add <- fcumsum(seq_ones(out_len),
                     check.o = FALSE,
                     na.rm = FALSE,
                     g = g) - 1
  }
  from + (g_add * by)
}
#' @rdname sequence2
#' @export
seq_id <- function(nvec){
  rep.int(seq_along(nvec), times = nvec)
}
#' @rdname sequence2
#' @export
seq_v <- function(from = 1L, to = 1L, by = 1L){
  # size <- seq_size(from = from, to = to, by = by)
  # sequence2(size, from = from, by = by)
  sequence2( ( (to - from) / by) + 1L, from = from, by = by)
}
seq_size <- function(from, to, by = 1L){
  out <- ( (to - from) / by ) + 1L
  out[by == 0 & from == to] <- 1
  out
}
# Low-level vectorised seq (only integers)
seqv.int <- function(from = 1L, to = 1L, by = 1L){
  sequence( ( (to - from) / by) + 1L, from = from, by = by)
}

