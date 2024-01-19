#' Fast rolling grouped lags and differences
#'
#' @description
#' Inspired by 'collapse', `roll_lag` and `roll_diff` operate similarly to
#' `flag` and `fdiff`.
#'
#' @param x A vector.
#' @param n Lag. Either length 1 or the same length as `x.`
#' This can also be negative.
#' @param g Grouping vector. This can be a vector, data frame or `GRP` object.
#' @param fill Value to fill the first `n` elements.
#' @param size Size of lag sequence.
#' @param partial If `TRUE`, the sequence will increment from 0 up to the
#' lag value. When calculating differences this can be useful,
#' as passing this lag sequence to
#' `roll_diff` will produce differences compared to the first value of `x`
#' for the first `n` differences.
#'
#' @details
#' While these may not be as fast the 'collapse' equivalents,
#' they are adequately fast and efficient. \cr
#' A key difference between `roll_lag` and `flag` is that `g` does not need
#' to be sorted for the result to be correct. \cr
#' Furthermore, a vector of lags can be supplied for a custom rolling lag.
#' In this case, groups are ignored. \cr
#' For time-based lags, see [time_lag].
#'
#' `lag_`, `lead_` and `diff_` are wrappers around the 'c++' functions that offer
#' very low overhead of ~1 microsecond and thus are primarily for programmers.
#'
#' @returns
#' A vector the same length as `x`.
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
#' roll_lag(x) # Lag
#' roll_lag(x, -1) # Lead
#' roll_diff(x) # Lag diff
#' roll_diff(x, -1) # Lead diff
#'
#' # Using lag_seq()
#' roll_lag(x, lag_seq(length(x), 2))
#' roll_diff(x, lag_seq(length(x), 5, partial = TRUE))
#'
#' # Like diff() but x/y instead of x-y
#' quotient <- function(x, n = 1L){
#'   x / roll_lag(x, n)
#' }
#' # People often call this a growth rate
#' # but it's just a percentage difference
#' # See ?roll_growth_rate for growth rate calculations
#' quotient(1:10)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname roll_lag
#' @export
roll_lag <- function(x, n = 1L, g = NULL, fill = NULL){
  UseMethod("roll_lag")
}
#' @export
roll_lag.default <- function(x, n = 1L, g = NULL, fill = NULL){
  if (!length(n) %in% c(1, length(x))){
    stop("n must be of length 1 or length(x)")
  }
  if (length(n) == 1){
    out <- flag2(x, n = n, g = g, fill = fill)
  } else {
    lagged_indices <- seq_along(x) - n
    out <- x[lagged_indices]
    if (!is.null(fill)){
      check_length(fill, 1)
      out[cpp_which(is.na(lagged_indices))] <- cast2(fill, out)
    }
  }
  out
}
#' @export
roll_lag.data.frame <- function(x, n = 1L, g = NULL, fill = NULL){
  N <- df_nrow(x)
  if (!length(n) %in% c(1, N)){
    stop("n must be of length 1 or nrow(x)")
  }
  row_seq <- seq_len(N)
  if (length(n) == 1){
    lag_s <- flag2(row_seq, n, g = g)
  } else {
    lag_s <- row_seq - n
  }
  out <- df_row_slice(x, lag_s)
  if (!is.null(fill)){
    check_length(fill, 1)
    which_fill <- cpp_which(is.na(lag_s))
    for (i in seq_len(df_ncol(out))){
      out[[i]][which_fill] <- cast2(fill, out[[i]])
    }
  }
  out
}
#' @export
roll_lag.time_interval <- function(x, n = 1L, g = NULL, fill = NULL){
  out <- roll_lag(as.data.frame(x), n = n, g = g, fill = fill)
  out <- unclass(out)
  new_time_interval(out[[1L]], out[[2L]])
}
roll_lag.vctrs_rcrd <- function(x, n = 1L, g = NULL, fill = NULL){
  out <- roll_lag(list_to_data_frame(unclass(x)), n = n, g = g, fill = fill)
  out <- unclass(out)
  attr(out, "row.names") <- NULL
  class(out) <- class(x)
  out
}
#' @rdname roll_lag
#' @export
roll_diff <- function(x, n = 1L, g = NULL, fill = NULL){
  UseMethod("roll_diff")
}
#' @export
roll_diff.default <- function(x, n = 1L, g = NULL, fill = NULL){
  if (!length(n) %in% c(1, length(x))){
    stop("n must be of length 1 or length(x)")
  }
  if (length(n) == 1){
    out <- fdiff2(x, n = n, g = g, fill = fill)
  } else {
    lagged_indices <- seq_along(x) - n
    out <- unclass(x) - unclass(x)[lagged_indices]
    out <- strip_attrs(out)
    if (!is.null(fill)){
      check_length(fill, 1)
      out[cpp_which(is.na(lagged_indices))] <- cast2(fill, out)
    }
  }
  out
}
#' @export
roll_diff.data.frame <- function(x, n = 1L, g = NULL, fill = NULL){
  N <- df_nrow(x)
  if (!length(n) %in% c(1, N)){
    stop("n must be of length 1 or nrow(x)")
  }
  row_seq <- seq_len(N)
  if (length(n) == 1){
    lag_s <- flag2(row_seq, n, g = g)
  } else {
    lag_s <- row_seq - n
  }
  out <- x
  for (i in seq_len(df_ncol(out))){
    x_lag <- unclass(out[[i]])[lag_s]
    out[[i]] <- unclass(out[[i]]) - x_lag
  }
  if (!is.null(fill)){
    check_length(fill, 1)
    which_fill <- cpp_which(is.na(lag_s))
    for (i in seq_len(df_ncol(out))){
      out[[i]][which_fill] <- cast2(fill, out[[i]])
    }
  }
  out
}
#' @export
roll_diff.time_interval <- function(x, n = 1L, g = NULL, fill = NULL){
  roll_diff(as.data.frame(x, n = n, g = g, fill = fill))
}
#' @rdname roll_lag
#' @export
lag_seq <- function(size, n = 1L, partial = FALSE){
  check_length(n, 1)
  if (n >= 0){
    lag_sequence(as.integer(size), k = n, partial = partial)
  } else {
    -lead_sequence(as.integer(size), k = -n, partial = partial)
  }
}
#' @rdname roll_lag
#' @export
lag_ <- function(x, n = 1L, fill = NA){
  .Call(`_timeplyr_cpp_roll_lag`, x, n, fill)
}
#' @rdname roll_lag
#' @export
lead_ <- function(x, n = 1L, fill = NA){
  .Call(`_timeplyr_cpp_roll_lead`, x, n, fill)
}
#' @rdname roll_lag
#' @export
diff_ <- function(x, n = 1L, fill = NA){
  .Call(`_timeplyr_cpp_roll_diff`, x, n, fill)
}

