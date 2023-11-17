#' Greatest common divisor and smallest common multiple
#'
#' @description
#' Fast greatest common divisor and smallest common multiple
#' using the Euclidean algorithm.
#'
#' `gcd()` returns the greatest common divisor. \cr
#' `scm()` returns the smallest common multiple. \cr
#' `gcd_diff()` returns the greatest common divisor of numeric differences.
#'
#' @param x A [numeric] vector.
#' @param tol Tolerance. This must
#' be a single positive number strictly less than 1.
#' @param na_rm If `TRUE` the default, `NA` values are ignored.
#' @param round If `TRUE` the output is rounded as
#' `round(gcd, digits)` where digits is
#' `ceiling(abs(log10(tol))) + 1`. \cr
#' This can potentially reduce floating point errors on
#' further calculations. \cr
#' The default is `FALSE`.
#' @param break_early This is experimental and
#' applies only to floating-point numbers.
#' When `TRUE` the algorithm will end once `gcd > 0 && gcd < 2 * tol`.
#' This can offer a tremendous speed improvement.
#' If `FALSE` the algorithm finishes once it has gone through all elements of `x`.
#' The default is `TRUE`. \cr
#' For integers, the algorithm always breaks early once `gcd > 0 && gcd <= 1`.
#' @param lag Lag of differences.
#' @param fill Value to initialise the algorithm for `gcd_diff()`.
#'
#' @returns
#' A number representing the GCD or SCM.
#'
#' @details
#'
#' ## Method
#'
#' ### GCD
#' The GCD is calculated using a binary function that takes input
#' `GCD(gcd, x[i + 1])` where the output of this function is passed as input
#' back into the same function iteratively along the length of `x`.
#' The first gcd value is `x[1]`.
#'
#' Zeroes are handled in the following way: \cr
#' `GCD(0, 0) = 0` \cr
#' `GCD(a, 0) = a` \cr
#'
#' This has the nice property that zeroes are essentially ignored.
#'
#' ### SCM
#'
#' This is calculated using the GCD and the formula is: \cr
#' `SCM(x, y) = (abs(x) / GCD(x, y) ) * abs(y)`
#'
#'
#' If you want to calculate the gcd & lcm for 2 values
#' instead of a vector of values,
#' use the internal functions `cpp_gcd2` and `cpp_lcm2`.
#' You can then easily write a vectorised method using these.
#'
#' @examples
#' library(timeplyr)
#' library(bench)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' gcd(c(0, 5, 25))
#' mark(gcd(c(0, 5, 25)))
#'
#' x <- rnorm(10^6)
#' gcd(x)
#' gcd(x, round = TRUE)
#' mark(gcd(x))
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#' }
#' @rdname gcd
#' @export
gcd <- function(x, tol = sqrt(.Machine$double.eps),
                na_rm = TRUE, round = FALSE,
                break_early = TRUE) {
  .Call(`_timeplyr_cpp_gcd`, x,
        as.double(tol),
        na_rm,
        1L,
        break_early,
        round)
}
#' @rdname gcd
#' @export
scm <- function(x, tol = sqrt(.Machine$double.eps),
                na_rm = TRUE, round = FALSE) {
  .Call(`_timeplyr_cpp_lcm`, x,
        as.double(tol),
        na_rm,
        round)
}
#' @rdname gcd
#' @export
gcd_diff <- function(x, lag = 1L, fill = NA,
                     tol = sqrt(.Machine$double.eps),
                     na_rm = TRUE, round = FALSE,
                     break_early = TRUE){
  .Call(`_timeplyr_cpp_gcd`,
        cpp_roll_diff(x, k = lag, fill = fill),
        as.double(tol),
        na_rm,
        1L,
        break_early,
        round)
}
