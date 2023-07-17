#' Are all numbers whole numbers?
#'
#' @param x A numeric vector.
#' @param na.rm Should `NA` values be removed before calculation?
#' Default is `TRUE`.
#' @param tol tolerance value such that a whole number satisfies the condition
#' `abs(round(x) - x) < tol`. \cr
#' The default is `sqrt(.Machine$double.eps)`.
#' @return A logical vector of length 1.
#' @details
#' This is a very efficient function that returns `FALSE` if any number
#' is not a whole-number and `TRUE` if all of them are.
#' It is efficient because the loop is written using `Rcpp` and
#' returns `FALSE` at the first instance of
#' a decimal number. \cr
#' Inspired by the discussion in this thread:
#' \href{https://stackoverflow.com/questions/3476782/check-if-the-number-is-integer/76655734}{check-if-the-number-is-integer}
#' @examples
#' \dontrun{
#' library(timeplyr)
#' library(bench)
#'
#' # Has built-in tolerance
#' sqrt(2)^2 %% 1 == 0
#' is_whole_number(sqrt(2)^2)
#'
#' x1 <- c(0.02, 0:10^7)
#' x2 <- c(0:10^7, 0.02)
#'
#' mark(e1 = all(x1 %% 1 == 0),
#'      e2 = is_whole_number(x1))
#'
#' mark(e1 = all(x2 %% 1 == 0),
#'      e2 = is_whole_number(x2))
#'
#' x3 <- round(rnorm(10^6))
#'
#' mark(e1 = all(x3 %% 1 == 0),
#'      e2 = is_whole_number(x3))
#' }
#' @export
is_whole_number <- function(x, na.rm = TRUE, tol = sqrt(.Machine$double.eps)){
  if (is.integer(x)){
    return(TRUE)
  }
  if (!typeof(x) %in% c("integer", "double", "logical")){
    stop("x must be a number")
  }
  if (!na.rm){
    if (anyNA(x)){
      return(NA)
    }
  }
  .Call(`_timeplyr_is_whole_num`, x, tol)
  # is_whole_num(x, tol = tol)
}
