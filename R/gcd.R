#' Greatest common divisor
#'
#' @description
#' Fast greatest common divisor using the Euclidean algorithm.
#'
#' @param x A [numeric] vector.
#' @param tol Tolerance. Be careful as this must
#' be a single positive number strictly less than 1.
#' @param na_rm If `TRUE` the default, `NA` values are ignored.
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
#' gcd(c(5, 25))
#' mark(gcd(c(5, 25)))
#'
#' x <- rnorm(10^6)
#' mark(gcd(x, tol = 1e-10))
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#' }
#' @export
gcd <- function(x, tol = sqrt(.Machine$double.eps), na_rm = TRUE) {
  .Call(`_timeplyr_cpp_gcd`, as.double(x),
        as.double(tol),
        na_rm,
        start = 1L,
        break_early = FALSE)
}
