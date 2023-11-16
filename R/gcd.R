#' Greatest common divisor
#'
#' @description
#' Fast greatest common divisor using the Euclidean algorithm.
#'
#' @param x A [numeric] vector.
#' @param tol Tolerance. Be careful as this must
#' be a single positive number strictly less than 1.
#' @param na_rm If `TRUE` the default, `NA` values are ignored.
#' @param round If `TRUE` the gcd output is rounded as
#' `round(gcd, digits)` where digits is
#' `ceiling(abs(log10(tol))) + 1`. \cr
#' This can potentially reduce floating point errors on
#' further calculations. \cr
#' The default is `FALSE`.
#'
#' @returns
#' A number representing the GCD.
#'
#' @details
#' The GCD is calculated using a binary function that takes input
#' `GCD(gcd, x[i + 1])` where the output of this function which is passed
#' back into the same function  iteratively along the vector `x`.
#' The first gcd value is `x[1]`.
#'
#' Zeroes are handled in the following way: \cr
#' `GCD(0, 0) = 0` \cr
#' `GCD(a, 0) = a` \cr
#'
#' This has the nice property that zeroes are essentially ignored.
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
#' @export
gcd <- function(x, tol = sqrt(.Machine$double.eps),
                na_rm = TRUE, round = FALSE) {
  .Call(`_timeplyr_cpp_gcd`, x,
        as.double(tol),
        na_rm,
        start = 1L,
        break_early = FALSE,
        round = round)
}
