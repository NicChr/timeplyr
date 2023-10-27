#' Are all numbers whole numbers?
#'
#' @param x A numeric vector.
#' @param tol tolerance value. \cr
#' The default is `.Machine$double.eps`, essentially the lowest possible tolerance.
#' A more typical tolerance for double floating point comparisons in other comparisons
#' is `sqrt(.Machine$double.eps)`.
#' @param na.rm Should `NA` values be removed before calculation?
#' Default is `TRUE`.
#'
#' @returns
#' A logical vector of length 1.
#'
#' @details
#' This is a very efficient function that returns `FALSE` if any number
#' is not a whole-number and `TRUE` if all of them are.
#'
#' ## Method
#' `x` is defined as a whole number vector
#' if all numbers satisfy `abs(x - round(x)) < tol`.
#'
#' ## `NA` handling
#' `NA` values are handled in a custom way. \cr
#' If `x` is an integer, `TRUE` is always returned even if `x` has missing values. \cr
#' If `x` has both missing values and decimal numbers, `FALSE` is always returned. \cr
#' If `x` has missing values, and only whole numbers and `na.rm = FALSE`, then
#' `NA` is returned. \cr
#' Basically `NA` is only returned if `na.rm = FALSE` and
#' `x` is a double vector of only whole numbers and `NA` values.
#'
#'
#' Inspired by the discussion in this thread:
#' \href{https://stackoverflow.com/questions/3476782/check-if-the-number-is-integer/76655734}{check-if-the-number-is-integer}
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
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
#' # Somewhat more strict than all.equal
#'
#' all.equal(10^9 + 0.0001, round(10^9 + 0.0001))
#' double_equal(10^9 + 0.0001, round(10^9 + 0.0001))
#' is_whole_number(10^9 + 0.0001)
#'
#' # For a vectorised version, use double_equal(x, round(x))
#'
#' x <- sqrt(1:10)^2
#' double_equal(x, round(x))
#' double_equal(x, round(x), tol = c(0, 1 * 10^-(0:8)))
#'
#' # strict zero tolerance
#' is_whole_number(1.0000000001, tol = .Machine$double.eps)
#'
#' # Can safely be used to select whole number variables
#' starwars %>%
#'   select(where(is_whole_number))
#'
#' # To reduce the size of any data frame one can use the below code
#'
#' df <- starwars %>%
#'   mutate(across(where(is_whole_number), as.integer))
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
is_whole_number <- function(x, tol = .Machine$double.eps, na.rm = TRUE){
  is.numeric(x) && cpp_is_whole_num(x, tol = as.double(tol), na_rm = na.rm)
}
