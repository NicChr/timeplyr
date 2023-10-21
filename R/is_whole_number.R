#' Are all numbers whole numbers?
#'
#' @param x A numeric vector.
#' @param na.rm Should `NA` values be removed before calculation?
#' Default is `TRUE`.
#' @param tol tolerance value. \cr
#' The default is `sqrt(.Machine$double.eps)`.
#'
#' @returns
#' A logical vector of length 1.
#'
#' @details
#' This is a very efficient function that returns `FALSE` if any number
#' is not a whole-number and `TRUE` if all of them are.
#'
#' ## Method
#' `x[i]` is a whole number if both the `rel_diff(x[i], round(x[i])) < tol` and
#' `abs_diff(x[i], round(x[i])) < tol` are satisfied where `rel_diff` is the relative difference
#' and `abs_diff` is the absolute difference for all `i >= 1 and i <= length(x)`.
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
is_whole_number <- function(x, na.rm = TRUE, tol = sqrt(.Machine$double.eps)){
  if (is.integer(x)){
    return(TRUE)
  }
  if (!na.rm && anyNA(x)){
    return(NA)
  }
  is.numeric(x) && is_whole_num(x, tol = as.double(tol))
}
