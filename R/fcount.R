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
#' @param ... Variables to group by.
#' @param wt Frequency weights.
#'   Can be `NULL` or a variable:
#'
#'   * If `NULL` (the default), counts the number of rows in each group.
#'   * If a variable, computes `sum(wt)` for each group.
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param order Should the groups be calculated as ordered groups?
#' If `FALSE`, this will return the groups in order of first appearance,
#' and in many cases is faster.
#' If `TRUE` (the default), the groups are returned in sorted order,
#' exactly the same way as `dplyr::count`.
#' @param name The name of the new column in the output.
#'  If there's already a column called `n`,
#'  it will use `nn`.
#'  If there's a column called `n` and `n`n,
#'  it'll use `nnn`, and so on, adding `n`s until it gets a new name.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @returns
#' A `data.frame` of frequency counts by group.
#'
#' @rdname f_count
#' @export
fcount <- function(data, ..., wt = NULL, sort = FALSE,
                   order = df_group_by_order_default(data),
                   name = NULL, .by = NULL, .cols = NULL){
  lifecycle::deprecate_warn(
    when = "0.8.2",
    what = "fcount()",
    with = "fastplyr::f_count()"
  )
  fastplyr::f_count(
    data, ..., wt = {{ wt }},
    sort = sort,
    order = order,
    .by = {{ .by }},
    .cols = .cols
  )
}

#' @rdname f_count
#' @export
fadd_count <- function(data, ..., wt = NULL, sort = FALSE,
                       order = df_group_by_order_default(data),
                       name = NULL, .by = NULL, .cols = NULL){
  lifecycle::deprecate_warn(
    when = "0.8.2",
    what = "fadd_count()",
    with = "fastplyr::f_add_count()"
  )
  fastplyr::f_add_count(
    data, ..., wt = {{ wt }},
    sort = sort,
    order = order,
    .by = {{ .by }},
    .cols = .cols,
    name = name
  )
}
