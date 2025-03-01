#' Fast rolling grouped lags and differences
#'
#' @description
#' Inspired by 'collapse', `roll_lag` and `roll_diff` operate similarly to
#' `flag` and `fdiff`.
#'
#' @param x A vector or data frame.
#' @param n Lag. This will be recycled to match the length of x and can be negative.
#' @param g Grouping vector. This can be a vector, data frame or `GRP` object.
#' @param fill Value to fill the first `n` elements.
#' @param differences Number indicating the number of times to recursively apply
#' the differencing algorithm. If `length(n) == 1`, i.e the lag is a scalar integer,
#' an optimised method is used which avoids recursion entirely.
#' If `length(n) != 1` then simply recursion is used.
#' @param ... Arguments passed onto appropriate method.
#' @param order Optionally specify an ordering with which to
#' apply the lags/differences.
#' This is useful for example when applying lags chronologically
#' using an unsorted time variable.
#' @param run_lengths Optional integer vector of run lengths that defines
#' the size of each lag run. For example, supplying `c(5, 5)` applies
#' lags to the first 5 elements and then essentially resets the bounds and
#' applies lags to the next 5 elements as if
#' they were an entirely separate and standalone vector. \cr
#' This is particularly useful in conjunction with the order argument to
#' perform a by-group lag.
#'
#' @details
#' While these may not be as fast the 'collapse' equivalents,
#' they are adequately fast and efficient. \cr
#' A key difference between `roll_lag` and `flag` is that `g` does not need
#' to be sorted for the result to be correct. \cr
#' Furthermore, a vector of lags can be supplied for a custom rolling lag.
#'
#' `roll_diff()` silently returns `NA` when there is integer overflow.
#' Both `roll_lag()` and `roll_diff()` apply recursively to list elements.
#'
#' @returns
#' A vector the same length as `x`.
#'
#' @examples
#' library(timeplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' x <- 1:10
#'
#' roll_lag(x) # Lag
#' roll_lag(x, -1) # Lead
#' roll_diff(x) # Lag diff
#' roll_diff(x, -1) # Lead diff
#'
#' # Using cheapr::lag_sequence()
#' # Differences lagged at 5, first 5 differences are compared to x[1]
#' roll_diff(x, cheapr::lag_sequence(length(x), 5, partial = TRUE))
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
#' }
#' @rdname roll_lag
#' @export
roll_lag <- function(x, n = 1L, ...){
  UseMethod("roll_lag")
}
#' @rdname roll_lag
#' @export
roll_lag.default <- function(x, n = 1L, g = NULL, fill = NULL, ...){
  order_counts <- group_order_and_counts(g)
  o <- order_counts[["order"]]
  rl <- order_counts[["sizes"]]
  if (is.null(o) && is.null(rl) && length(n) == 1L){
    cheapr::lag_(x, n, fill = fill, recursive = TRUE)
  } else {
    cheapr::lag2_(x, n, order = o, run_lengths = rl,
                  fill = fill, recursive = TRUE)
  }
}
#' @rdname roll_lag
#' @export
roll_lag.ts <- function(x, n = 1L, g = NULL, fill = NULL, ...){
  ncols <- ncol(x)
  nrows <- nrow(x)
  if (is.array(x) && ncols > 1){
    group_sizes <- rep(nrows, ncols)
    col_groups <- cheapr::seq_id(group_sizes)
    if (is.null(g)){
      # Take advantage of the fact that seq_id() produces sorted group IDs
      g <- sorted_group_id_to_GRP(col_groups, n_groups = ncols, group_sizes = group_sizes,
                                  group.starts = FALSE)
    } else {
      user_groups <- rep(fastplyr::group_id(g), ncols)
      g <- GRP2(fastplyr::list_tidy(col_groups, user_groups, .keep_null = FALSE))
    }
  }
  out <- roll_lag.default(x, n = n, g = g, fill = fill, ...)
  attributes(out) <- attributes(x)
  out
}
#' @rdname roll_lag
#' @export
roll_lag.zoo <- roll_lag.ts

#' @rdname roll_lag
#' @export
roll_diff <- function(x, n = 1L, ...){
  UseMethod("roll_diff")
}
#' @rdname roll_lag
#' @export
roll_diff.default <- function(x, n = 1L, g = NULL, fill = NULL, differences = 1L, ...){
  order_counts <- group_order_and_counts(g)
  diff_(x, n, order = order_counts[["order"]],
        run_lengths = order_counts[["sizes"]],
        fill = fill,
        differences = differences)
}
#' @rdname roll_lag
#' @export
roll_diff.ts <- function(x, n = 1L, g = NULL, fill = NULL, differences = 1L, ...){
  ncols <- ncol(x)
  nrows <- nrow(x)
  if (is.array(x) && ncols > 1){

    ## You can speed this up in C but not enough justification to do so.

    ncols <- ncol(x)
    nrows <- nrow(x)
    group_sizes <- rep(nrows, ncols)
    col_groups <- cheapr::seq_id(group_sizes)
    if (is.null(g)){
      # Take advantage of the fact that seq_id() produces sorted group IDs
      g <- sorted_group_id_to_GRP(col_groups, n_groups = ncols, group_sizes = group_sizes,
                                  group.starts = FALSE)
    } else {
      user_groups <- rep(fastplyr::group_id(g), ncols)
      g <- GRP2(fastplyr::list_tidy(col_groups, user_groups, .keep_null = FALSE))
    }
  }
  out <- roll_diff.default(x, n = n, g = g,
                           fill = fill, differences = differences, ...)
  attributes(out) <- attributes(x)
  out
}
#' @rdname roll_lag
#' @export
roll_diff.zoo <- roll_diff.ts

#' @rdname roll_lag
#' @export
diff_ <- function(x, n = 1L, differences = 1L, order = NULL, run_lengths = NULL, fill = NULL){
  .Call(`_timeplyr_cpp_diff`, x, n, order, run_lengths, fill, differences)
}
