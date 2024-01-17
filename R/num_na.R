#' Fast number of missing values
#'
#' @description
#' A faster and more efficient alternative to `sum(is.na(x))`. \cr
#' Long vectors, i.e vectors with length >= 2^31 are also supported.
#'
#' @param x A vector.
#'
#' @returns
#' Number of `NA` values.
#'
#' @details
#' If `x` is a data frame, `num_na()` counts the number of empty rows with
#' only missing values.
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
#' flights <- nycflights13::flights
#'
#' # num_na is more efficient than using `sum(is.na())`
#' mark(vapply(flights, num_na, integer(1)),
#'      vapply(flights, function(x) sum(is.na(x)), integer(1)),
#'      iterations = 10)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
num_na <- function(x){
  .Call(`_timeplyr_cpp_num_na`, x)
}
