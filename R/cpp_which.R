#' Efficient alternative to `which()`
#'
#' @description
#' Exactly the same as `which()` but more memory efficient.
#'
#' @param x A [logical] vector.
#' @param invert If `TRUE`, indices of values that are not `TRUE` are returned
#' (including `NA`). If `FALSE` (the default), only `TRUE` indices are returned.
#'
#' @returns
#' An unnamed integer vector.
#'
#' @details
#' This implementation is similar in speed to `which()`
#' but usually more memory efficient.
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
#' x <- sample(c(TRUE, FALSE), 1e06, TRUE)
#' x[sample.int(1e06, round(1e06/3))] <- NA
#'
#' mark(cpp_which(TRUE), which(TRUE))
#' mark(cpp_which(FALSE), which(FALSE))
#' mark(cpp_which(logical()), which(logical()))
#' mark(cpp_which(x), which(x), iterations = 20)
#' mark(cpp_which(x, invert = TRUE), which(!x %in% TRUE), iterations = 20)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#' }
#'
#' @export
cpp_which <- function(x, invert = FALSE){
  .Call(`_timeplyr_cpp_which_`, x, invert)
}
