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
#' @param data Data frame
#' @param ... See `?dplyr::slice` for details.
#' @param sort_groups If `TRUE` (the default) the by-group slices will be
#' done in order of the sorted groups.
#' If `FALSE` the group order is determined by first-appearance in the data.
#' @param keep_order Should the sliced data frame be returned in its original order?
#' The default is `FALSE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param n Number of rows.
#' @param prop Proportion of rows.
#' @param order_by Variables to order by.
#' @param with_ties Should ties be kept together? The default is `TRUE`.
#' @param na_rm Should missing values in `fslice_max()` and `fslice_min()` be removed?
#' The default is `FALSE`.
#' @param replace Should `fslice_sample()` sample with or without replacement?
#' Default is `FALSE`, without replacement.
#' @param weights Probability weights used in `fslice_sample()`.
#' @param seed Seed number defining RNG state.
#' If supplied, this is only applied \bold{locally} within the function
#' and the seed state isn't retained after sampling.
#' To clarify, whatever seed state was in place before the function call,
#' is restored to ensure seed continuity.
#' If left `NULL` (the default), then the seed is never modified.
#'
#' @returns
#' A `data.frame` of specified rows.
#'
#' @rdname fslice
#' @export
fslice <- function(data, ..., .by = NULL,
                   keep_order = FALSE, sort_groups = TRUE){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "fslice()",
    with = "fastplyr::f_slice()"
  )
  fastplyr::f_slice(data, i = c(...),
                    .by = {{ .by }},
                    keep_order = keep_order)
}
#' @rdname fslice
#' @export
fslice_head <- function(data, ..., n, prop, .by = NULL,
                        keep_order = FALSE, sort_groups = TRUE){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "fslice_head()",
    with = "fastplyr::f_slice_head()"
  )
  rlang::check_dots_empty0(...)
  fastplyr::f_slice_head(
    data, n = n, prop = prop,
    .by = {{ .by }},
    keep_order = keep_order
  )
}
#' @rdname fslice
#' @export
fslice_tail <- function(data, ..., n, prop, .by = NULL,
                        keep_order = FALSE, sort_groups = TRUE){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "fslice_tail()",
    with = "fastplyr::f_slice_tail()"
  )
  rlang::check_dots_empty0(...)
  fastplyr::f_slice_tail(
    data, n = n, prop = prop,
    .by = {{ .by }},
    keep_order = keep_order
  )
}
#' @rdname fslice
#' @export
fslice_min <- function(data, order_by, ..., n, prop, .by = NULL,
                       with_ties = TRUE, na_rm = FALSE,
                       keep_order = FALSE, sort_groups = TRUE){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "fslice_min()",
    with = "fastplyr::f_slice_min()"
  )
  rlang::check_dots_empty0(...)
  fastplyr::f_slice_min(
    data, order_by = {{ order_by }},
    n = n, prop = prop,
    .by = {{ .by }},
    keep_order = keep_order,
    with_ties = with_ties
  )
}
#' @rdname fslice
#' @export
fslice_max <- function(data, order_by, ..., n, prop, .by = NULL,
                       with_ties = TRUE, na_rm = FALSE,
                       keep_order = FALSE, sort_groups = TRUE){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "fslice_max()",
    with = "fastplyr::f_slice_max()"
  )
  rlang::check_dots_empty0(...)
  fastplyr::f_slice_max(
    data, order_by = {{ order_by }},
    n = n, prop = prop,
    .by = {{ .by }},
    keep_order = keep_order,
    with_ties = with_ties
  )
}
#' @rdname fslice
#' @export
fslice_sample <- function(data, n, replace = FALSE, prop,
                          .by = NULL,
                          keep_order = FALSE, sort_groups = TRUE,
                          weights = NULL, seed = NULL){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "fslice_sample()",
    with = "fastplyr::f_slice_sample()"
  )
  if (is.null(seed)){
    fastplyr::f_slice_sample(
      data, replace = replace,
      n = n, prop = prop,
      .by = {{ .by }},
      keep_order = keep_order,
      weights = {{ weights }}
    )
  } else {
    cheapr::with_local_seed({
      fastplyr::f_slice_sample(
        data, replace = replace,
        n = n, prop = prop,
        .by = {{ .by }},
        keep_order = keep_order,
        weights = {{ weights }}
      )
    }, .seed = seed)
  }
}
