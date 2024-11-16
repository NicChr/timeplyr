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
#'
#' @param data A data frame.
#' @param ... Variables to expand.
#' @param expand_type `r lifecycle::badge("deprecated")`
#' Use `fastplyr::crossing()` and `fastplyr::nesting()`.
#' @param fill A named list containing value-name pairs
#' to fill the named implicit missing values.
#' @param sort Logical. If `TRUE` expanded/completed variables are sorted.
#' The default is `FALSE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#'
#' @returns
#' A `data.frame` of expanded groups.
#' @rdname fexpand
#' @export
fexpand <- function(data, ..., expand_type = NULL,
                    sort = FALSE,
                    .by = NULL){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "fexpand()",
    with = "fastplyr::f_expand()"
  )
  if (!is.null(expand_type)){
    lifecycle::deprecate_soft(
      "0.9.0",
      "time_expand(expand_type)"
    )
  }
  fastplyr::f_expand(data, ..., .sort = sort, .by = {{ .by }})
}
#' @rdname fexpand
#' @export
fcomplete <- function(data, ..., expand_type = NULL,
                      sort = FALSE, .by = NULL,
                      fill = NA){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "fcomplete()",
    with = "fastplyr::f_complete()"
  )
  if (!is.null(expand_type)){
    lifecycle::deprecate_soft(
      "0.8.2",
      "time_expand(expand_type)"
    )
  }
  fastplyr::f_complete(data, ..., .sort = sort, .by = {{ .by }})
}
