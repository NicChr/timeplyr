#' Extension to `base::sequence()`
#'
#' @description
#' Like `sequence()` but it accepts decimal increments.
#'
#' @param nvec Vector of sequence lengths.
#' @param from Start of sequence(s).
#' @param to End of sequence(s).
#' @param by Unit increment of sequence(s).
#'
#' @returns
#' `sequence2` and `seq_id` return a vector `length(sum(nvec))`. \cr
#' `seq_v` returns a vector of size `sum((to - from) / (by + 1))`
#'
#' @details
#' `sequence2()` works in the same as `sequence()` but can accept
#' non-whole number `by` values.
#' It also doesn't recycle `from` and `to`, in the same as `sequence()`. \cr
#' `seq_v()` is a vectorised version of `seq()` that strictly accepts
#' only the arguments `from`, `to` and `by`. \cr
#' `seq_id()` is a helper function to efficiently return unique IDs for
#' each sequence.
#'
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
  out_is_int <- isTRUE(is_integerable(out_len))
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
    g_add <- collapse::fcumsum(seq_ones(out_len),
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
  out <- abs(( (to - from) / by ))
  out[by == 0 & from == to] <- 0
  as.integer(out) + 1L
}
# Low-level vectorised seq (only integers)
seqv.int <- function(from = 1L, to = 1L, by = 1L){
  sequence( ( (to - from) / by) + 1L, from = from, by = by)
}

