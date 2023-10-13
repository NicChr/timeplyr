#' Helpers to sort variables in ascending or descending order
#'
#' @description An alternative to `dplyr::desc()` which is much faster
#' for character vectors and factors.
#'
#' @param x Vector.
#'
#' @returns
#' A numeric vector that can be ordered in ascending or descending order. \cr
#' Useful in `dplyr::arrange()` or `farrange()`.
#'
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(nycflights13)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' flights %>%
#'   fdistinct(dest) %>%
#'   farrange(desc(dest))
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname desc
#' @export
asc <- function(x){
  if (is_s3_numeric(x)){
    xtfrm(x)
  } else {
    qg_to_integer(qG2(x, sort = TRUE, na.exclude = TRUE))
    # -vctrs::vec_rank(x, ties = "min", incomplete = "na")
  }
}
#' @rdname desc
#' @export
desc <- function(x){
  -asc(x)
}
