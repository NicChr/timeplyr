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
#' @param ... Variables used to find duplicate rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
#' @param .both_ways If `TRUE` then duplicates and non-duplicate first instances
#' are retained. The default is `FALSE` which returns only duplicate rows. \cr
#' Setting this to `TRUE` can be particularly useful when examining
#' the differences between duplicate rows.
#' @param .add_count If `TRUE` then a count column is added to denote the
#' number of duplicates (including first non-duplicate instance).
#' The naming convention of this column follows `dplyr::add_count()`.
#' @param .drop_empty If `TRUE` then empty rows with all `NA` values are removed.
#' The default is `FALSE`.
#' @param sort Should result be sorted?
#' If `FALSE` (the default), then rows are returned in the exact same order as
#' they appear in the data.
#' If `TRUE` then the duplicate rows are sorted.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @returns
#' A `data.frame` of duplicate rows.
#'
#' @rdname duplicate_rows
#' @export
duplicate_rows <- function(data, ..., .keep_all = FALSE,
                           .both_ways = FALSE, .add_count = FALSE,
                           .drop_empty = FALSE, sort = FALSE,
                           .by = NULL, .cols = NULL){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "duplicate_rows()",
    with = "fastplyr::f_duplicates()"
  )
  fastplyr::f_duplicates(
    data, ...,
    .keep_all = .keep_all,
    .both_ways = .both_ways,
    .add_count = .add_count,
    .drop_empty = .drop_empty,
    sort = sort,
    .by = {{ .by }},
    .cols = .cols
  )
}
