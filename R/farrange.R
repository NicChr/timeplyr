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
#' @param ... Variables to arrange by.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using `tidyselect`.
#' @param .by_group If `TRUE` the sorting will be first done by the group
#' variables.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @returns
#' A sorted `data.frame`.
#'
#' @export
farrange <- function(data, ..., .by = NULL, .by_group = FALSE,
                     .cols = NULL){
  lifecycle::deprecate_warn(
    when = "0.8.2",
    what = "farrange()",
    with = "fastplyr::f_arrange()"
  )
  fastplyr::f_arrange(
    data, ...,
    .by_group = .by_group,
    .by = {{ .by }},
    .cols = .cols
  )
}
