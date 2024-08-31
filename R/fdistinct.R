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
#' @param ... Variables used to find distinct rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
#' @param sort Should result be sorted? Default is `FALSE`.
#' When `order = FALSE` this option has no effect on the result.
#' @param order Should the groups be calculated as ordered groups?
#' Setting to `TRUE` may sometimes offer a speed benefit, but usually this
#' is not the case. The default is `FALSE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @returns
#' A `data.frame` of distinct groups.
#'
#' @export
fdistinct <- function(data, ..., .keep_all = FALSE,
                       sort = FALSE, order = sort,
                      .by = NULL, .cols = NULL){
  lifecycle::deprecate_warn(
    when = "0.8.2",
    what = "fdistinct()",
    with = "fastplyr::f_distinct()"
  )
  fastplyr::f_distinct(
    data, ...,
    .keep_all = .keep_all,
    sort = sort,
    order = order,
    .by = {{ .by }},
    .cols = .cols
  )
}
