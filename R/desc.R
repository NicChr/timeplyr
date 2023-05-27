#' Ascending & Descending order of vector
#'
#' @description An alternate to `dplyr::desc()` which is much faster
#' for character vectors and factors.
#'
#' @param x Vector.
#' @return
#' A numeric vector that can be ordered in ascending or descending order. \cr
#' Useful in `dplyr::arrange()` or `farrange()`.
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(nycflights13)
#' data(flights)
#'
#' flights %>%
#'   fdistinct(dest) %>%
#'   farrange(desc(dest))
#' \dontrun{
#' library(bench)
#' mark(order(dplyr::desc(flights$tailnum)),
#'      order(timeplyr::desc(flights$tailnum)))
#' }
#' @rdname desc
#' @export
asc <- function(x){
  if (is_time_or_num(x)){
    xtfrm(x)
  } else {
    as.integer(qG2(x, sort = TRUE, na.exclude = TRUE))
    # -vctrs::vec_rank(x, ties = "min", incomplete = "na")
  }
}
#' @rdname desc
#' @export
desc <- function(x){
  -asc(x)
}
