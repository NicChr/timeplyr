#' Ascending & Descending order of vector
#'
#' @param x Vector.
#' @return
#' A numeric vector that orders in ascending or descending order. \cr
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
#' @rdname desc
#' @export
asc <- function(x){
  if (inherits(x, c("integer", "numeric",
                    "Date", "POSIXt", "POSIXct", "POSIXlt"))){
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
