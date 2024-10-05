#' These functions have been superseded by fastplyr functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `r lifecycle::badge("superseded")`
#'
#' These functions can now be found in fastplyr. \cr
#' They are no longer recommended in this package and thus have been both
#' deprecated and superseded.
#'
#' @param x A vector, data frame or `GRP` object.
#' @param ascending When `ascending = TRUE` the row IDs are in
#' increasing order. When `ascending = FALSE` the row IDs are in
#' decreasing order.
#'
#' @details
#' `frowid()` is like `data.table::rowid()` but uses
#' an alternative method for calculating row numbers.
#' When `x` is a collapse `GRP` object, it is considerably faster.
#' It is also faster for character vectors.
#'
#' @returns
#' An integer vector of row IDs.
#'
#' @rdname frowid
#' @export
frowid <- function(x, ascending = TRUE){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "frowid()",
    with = "fastplyr::row_id()"
  )
  fastplyr::row_id(x, ascending = ascending)
}
