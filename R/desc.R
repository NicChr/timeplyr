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
#' @param x Vector.
#'
#' @returns
#' A numeric vector that can be ordered in ascending or descending order. \cr
#' Useful in `dplyr::arrange()` or `fastplyr::f_arrange()`.
#'
#' @rdname desc
#' @export
asc <- function(x){
  -fastplyr::desc(x)
}
#' @rdname desc
#' @export
desc <- function(x){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "desc()",
    with = "fastplyr::desc()"
  )
  fastplyr::desc(x)
}
