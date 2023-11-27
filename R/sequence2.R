#' Extension to `base::sequence()`
#'
#' @description
#' Like [sequence()] but it accepts decimal increments.
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
#' `sequence2()` works in the same way as `sequence()` but can accept
#' non-integer `by` values.
#' It also recycles `from` and `to`, in the same way as `sequence()`. \cr
#' If any of the sequences contain values > .Machine$integer.max,
#' then the result will always be a double vector.
#'
#' `from` can be also be a date, date-time, or any object that supports
#' addition and multiplication.
#'
#' `seq_v()` is a vectorised version of `seq()` that strictly accepts
#' only the arguments `from`, `to` and `by`. \cr
#' `seq_size()` returns the sequence sizes.
#' `seq_id()` is a helper function to efficiently return unique IDs for
#' each sequence.
#'
#' @examples
#' library(timeplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' sequence(1:3)
#' sequence2(1:3)
#'
#' sequence(1:3, by = 0.1)
#' sequence2(1:3, by = 0.1)
#'
#' sequence(c(3, 2), by = c(-0.1, 0.1))
#' sequence2(c(3, 2), by = c(-0.1, 0.1))
#'
#' seq_v(1, 10, by = c(1, 0.5))
#' # Same as below
#' c(seq(1, 10, 1), seq(1, 10, 0.5))
#'
#' # Programmers may use seq_size() to determine final sequence lengths
#'
#' sizes <- seq_size(1, 10, by = c(1, 0.5))
#' print(paste(c("sequence sizes: (", sizes, ") total size:", sum(sizes)),
#'             collapse = " "))
#'
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname sequence2
#' @export
sequence2 <- function(nvec, from = 1L, by = 1L){
  # Sequence end values
  # If these cant be integers, then we need to work with doubles
  seq_ends <- unclass(from) + (by * (pmax.int(nvec - 1, 0)))
  out_maybe_int <- all_integerable(seq_ends)
  # If from/by are integers and all sequence values < 2^31 then use sequence
  out_is_int <- is.integer(from) && !is.object(from) && is.integer(by) && out_maybe_int
  if (out_is_int){
    return(sequence(nvec = nvec, from = from, by = by))
  }
  if (is.object(from)){
    g_len <- length(nvec)
    if (length(from) > 1L){
      # Recycle
      from <- rep_len(from, g_len)
      # Expand
      from <- rep(from, times = nvec)
    }
    if (length(by) > 1L){
      # Recycle
      by <- rep_len(by, g_len)
      # Expand
      by <- rep(by, times = nvec)
    }
    # Arithmetic
    if (is.integer(by)){
      g_add <- integer_sequence(nvec, from = 0L, by = 1L)
    } else {
      g_add <- double_sequence(nvec, from = 0, by = 1)
    }
    from + (g_add * by)
  } else {
    double_sequence(nvec, from = from, by = by)
  }
}
# Like base::sequence() but c++
integer_sequence <- function(size, from = 1L, by = 1L){
  cpp_int_sequence(as.integer(size), as.integer(from), as.integer(by))
}
# Like base::sequence() but with support for double increments and long vectors
double_sequence <- function(size, from = 1, by = 1){
  cpp_dbl_sequence(as.integer(size), as.double(from), as.double(by))
}
#' @rdname sequence2
#' @export
seq_id <- function(nvec){
  rep.int(seq_along(nvec), times = nvec)
}
#' @rdname sequence2
#' @export
seq_v <- function(from = 1L, to = 1L, by = 1L){
  out_size <- seq_size(from = from, to = to, by = by)
  sequence2(out_size, from = from, by = by)
}
#' @rdname sequence2
#' @export
seq_size <- function(from, to, by = 1L){
  del <- to - from
  if (is.integer(by) && allv2(by, 1L)){
    size <- del
  } else {
    size <- del / by
    size[collapse::whichv(del, 0)] <- 0
  }
  size_rng <- collapse::frange(size, na.rm = TRUE)
  if (isTRUE(any(size_rng < 0))){
    stop("At least 1 sequence length is negative, please check the sign of by")
  }
  if (is.integer(size)){
    size + 1L
  } else if (length(size) == 0 || all(is_integerable(abs(size_rng) + 1), na.rm = TRUE)){
    as.integer(size + 1e-10) + 1L
  } else {
    trunc(size + 1e-10) + 1
  }
}
seq_tbl <- function(from = 1L, to = 1L, by = 1L){
  size <- seq_size(from = from, to = to, by = by)
  seq_out <- sequence2(size, from = from, by = by)
  seq_id <- seq_id(size)
  new_tbl(id = seq_id, x = seq_out)
}
