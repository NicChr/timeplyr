#' Are all numbers whole numbers?
#'
#' @param x A numeric vector.
#' @param na.rm Should `NA` values be removed before calculation?
#' Default is `TRUE`.
#' @param tol tolerance value such that a whole number satisfies the condition
#' `abs(round(x) - x) < tol`. \cr
#' The default is `sqrt(.Machine$double.eps)`.
#'
#' @returns
#' A logical vector of length 1.
#'
#' @details
#' This is a very efficient function that returns `FALSE` if any number
#' is not a whole-number and `TRUE` if all of them are.
#'
#' Inspired by the discussion in this thread:
#' \href{https://stackoverflow.com/questions/3476782/check-if-the-number-is-integer/76655734}{check-if-the-number-is-integer}
#'
#' @examples
#' library(timeplyr)
#' \dontshow{
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # Has built-in tolerance
#' sqrt(2)^2 %% 1 == 0
#' is_whole_number(sqrt(2)^2)
#'
#' is_whole_number(1)
#' is_whole_number(1.2)
#'
#' x1 <- c(0.02, 0:10^5)
#' x2 <- c(0:10^5, 0.02)
#'
#' is_whole_number(x1)
#' is_whole_number(x2)
#'
#' @export
is_whole_number <- function(x, na.rm = TRUE, tol = sqrt(.Machine$double.eps)){
  if (is.integer(x)){
    return(TRUE)
  }
  if (!is.numeric(x)){
    return(FALSE)
  }
  if (!na.rm){
    if (anyNA(x)){
      return(NA)
    }
  }
  .Call(`_timeplyr_is_whole_num`, x, tol)
}
