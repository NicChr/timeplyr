#' Double floating point comparison functions
#'
#' @description Fast and efficient methods for comparing double floating point numbers
#' using relative differences.
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @param tol A vector of tolerances.
#'
#' @return A logical vector.
#'
#' @details
#' When either `x[i]` or `y[i]` contain a number very close to zero,
#' absolute differences are used, otherwise relative differences are used.
#'
#' The output is commutative, which means the order of arguments don't matter
#' whereas this is not the case for `all.equal.numeric()`.
#'
#'
#' The calculation is done in C++ and is quite efficient.
#' Recycling follows the usual R rules and is done without allocating
#' additional memory.
#'
#' @rdname doubles
#' @export
double_equal <- function(x, y, tol = sqrt(.Machine$double.eps)){
  if (is.integer(x) && is.integer(y)){
    x == y
  } else {
    cpp_double_equal_vectorised(as.double(x), as.double(y), as.double(tol))
  }
}
#' @rdname doubles
#' @export
double_gt <- function(x, y, tol = sqrt(.Machine$double.eps)){
  if (is.integer(x) && is.integer(y)){
    x > y
  } else {
    cpp_double_gt_vectorised(as.double(x), as.double(y), as.double(tol))
  }
}
#' @rdname doubles
#' @export
double_gte <- function(x, y, tol = sqrt(.Machine$double.eps)){
  if (is.integer(x) && is.integer(y)){
    x >= y
  } else {
    cpp_double_gte_vectorised(as.double(x), as.double(y), as.double(tol))
  }
}
#' @rdname doubles
#' @export
double_lt <- function(x, y, tol = sqrt(.Machine$double.eps)){
  if (is.integer(x) && is.integer(y)){
    x < y
  } else {
    cpp_double_lt_vectorised(as.double(x), as.double(y), as.double(tol))
  }
}
#' @rdname doubles
#' @export
double_lte <- function(x, y, tol = sqrt(.Machine$double.eps)){
  if (is.integer(x) && is.integer(y)){
    x <= y
  } else {
    cpp_double_lte_vectorised(as.double(x), as.double(y), as.double(tol))
  }
}
