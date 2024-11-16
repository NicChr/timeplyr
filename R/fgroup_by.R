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
#' @param data data frame.
#' @param ... Variables to group by.
#' @param .add Should groups be added to existing groups?
#' Default is `FALSE`.
#' @param order Should groups be ordered? If `FALSE`
#' groups will be ordered based on first-appearance. \cr
#' Typically, setting order to `FALSE` is faster.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using `tidyselect`.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .drop Should unused factor levels be dropped? Default is `TRUE`.
#'
#' @returns
#' A `grouped_df`.
#'
#' @export
fgroup_by <- function(data, ..., .add = FALSE,
                      order = df_group_by_order_default(data),
                      .by = NULL, .cols = NULL,
                      .drop = df_group_by_drop_default(data)){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "fgroup_by()",
    with = "fastplyr::f_group_by()"
  )
  fastplyr::f_group_by(
    data, ...,
    .add = .add,
    .drop = .drop,
    .order = order,
    .by = {{ .by }},
    .cols = .cols
  )
}
