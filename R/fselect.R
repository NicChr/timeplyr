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
#' @param data A data frame.
#' @param ... Variables to select using `tidy-select`.
#' See `?dplyr::select` for more info.
#' @param .cols (Optional) faster alternative to `...` that accepts
#' a named character vector or numeric vector. \cr
#' No checks on duplicates column names are done when using `.cols`. \cr
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @returns
#' A `data.frame` of selected columns.
#'
#' @export
fselect <- function(data, ..., .cols = NULL){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "fselect()",
    with = "fastplyr::f_select()"
  )
  fastplyr::f_select(data, ..., .cols = .cols)
}
#' @rdname fselect
#' @export
frename <- function(data, ..., .cols = NULL){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "frename()",
    with = "fastplyr::f_rename()"
  )
  fastplyr::f_rename(data, ..., .cols = .cols)
}
