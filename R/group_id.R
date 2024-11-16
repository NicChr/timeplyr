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
#' @param data A data frame or vector.
#' @param ... Additional groups using tidy `data-masking` rules. \cr
#' To specify groups using `tidyselect`, simply use the `.by` argument.
#' @param order Should the groups be ordered?
#' \bold{THE PHYSICAL ORDER OF THE DATA IS NOT CHANGED.} \cr
#' When order is `TRUE` (the default) the group IDs will be
#' ordered but not sorted.\cr
#' The expression
#' \preformatted{
#' identical(order(x, na.last = TRUE),
#'           order(group_id(x, order = TRUE)))
#' }
#' or in the case of a data frame
#' \preformatted{
#' identical(order(x1, x2, x3, na.last = TRUE),
#'           order(group_id(data, x1, x2, x3, order = TRUE)))
#' }
#' should always hold.\cr
#' If `FALSE` the order of the group IDs will be based on first appearance.
#' @param ascending Should the group order be ascending or descending?
#' The default is `TRUE`. \cr
#' For `row_id()` this determines if the row IDs are increasing or decreasing. \cr
#' \bold{NOTE} - When `order = FALSE`, the `ascending` argument is
#' ignored. This is something that will be fixed in a later version.
#' @param .by Alternative way of supplying groups using `tidyselect` notation.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .name Name of the added ID column which should be a
#' character vector of length 1.
#' If `.name = NULL` (the default),
#' `add_group_id()` will add a column named "group_id",
#' and if one already exists, a unique name will be used.
#' @param as_qg Should the group IDs be returned as a
#' collapse "qG" class? The default (`FALSE`) always returns
#' an integer vector.
#'
#' @returns
#' An integer vector.
#'
#' @rdname group_id
#' @export
group_id <- function(data, ...,
                     order = TRUE,
                     ascending = TRUE,
                     .by = NULL, .cols = NULL,
                     as_qg = FALSE){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "group_id()",
    with = "fastplyr::group_id()"
  )
  fastplyr::group_id(
    data, .order = order,
    ascending = ascending, as_qg = as_qg
  )
}
#' @rdname group_id
#' @export
add_group_id <- function(data, ...,
                         order = TRUE,
                         ascending = TRUE,
                         .by = NULL, .cols = NULL,
                         .name = NULL,
                         as_qg = FALSE){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "add_group_id()",
    with = "fastplyr::add_group_id()"
  )
  fastplyr::add_group_id(
    data, ...,
    .order = order,
    .ascending = ascending,
    as_qg = as_qg,
    .by = {{ .by }},
    .cols = .cols,
    .name = .name
  )
}
#' @rdname group_id
#' @export
row_id <- function(data, ..., ascending = TRUE,
                   .by = NULL, .cols = NULL){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "row_id()",
    with = "fastplyr::row_id()"
  )
  fastplyr::row_id(data, ascending = ascending)
}
#' @rdname group_id
#' @export
add_row_id <- function(data, ..., ascending = TRUE,
                       .by = NULL, .cols = NULL,
                       .name = NULL){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "add_row_id()",
    with = "fastplyr::add_row_id()"
  )
  fastplyr::add_row_id(
    data, ...,
    .ascending = ascending,
    .by = {{ .by }},
    .cols = .cols,
    .name = .name
  )
}
#' @rdname group_id
#' @export
group_order <- function(data, ..., ascending = TRUE,
                        .by = NULL, .cols = NULL){
  UseMethod("group_order")
}
#' @export
group_order.default <- function(data, ..., ascending = TRUE){
  radixorderv2(data, decreasing = !ascending,
               na.last = TRUE, starts = FALSE,
               group.sizes = FALSE, sort = TRUE)
  # Alternate method
  # g <- GRP2(df_ungroup(data),
  #           sort = TRUE,
  #           decreasing = !ascending,
  #           na.last = TRUE,
  #           return.groups = FALSE,
  #           return.order = TRUE,
  #           method = "auto",
  #           call = FALSE)
  # out <- g[["order"]]
  # for (a in names(attributes(out))){
  #   attr(out, a) <- NULL
  # }
  # if (is.null(out)){
  #   radix_order(GRP_group_id(g))
  # } else {
  #  out
  # }
}
#' @export
group_order.data.frame <- function(data, ..., ascending = TRUE,
                                   .by = NULL, .cols = NULL){
  N <- df_nrow(data)
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = FALSE)
  all_groups <- group_info[["all_groups"]]
  if (length(all_groups) == 0L){
    if (ascending){
      out <- seq_len(N)
    } else {
      out <- seq.int(from = N,
                     to = min(N, 1L),
                     by = -1L)
    }
  } else {
    out <- radixorderv2(fastplyr::f_select(group_info[["data"]], .cols = all_groups),
                                   decreasing = !ascending,
                                   na.last = TRUE, starts = FALSE,
                                   group.sizes = FALSE, sort = TRUE)
  }
  out
}
#' @export
group_order.grouped_df <- group_order.data.frame
#' @rdname group_id
#' @export
add_group_order <- function(data, ..., ascending = TRUE,
                            .by = NULL, .cols = NULL,
                            .name = NULL){
  if (is.null(.name)){
    .name <- unique_col_name(names(data), "group_order")
  }
  df_add_cols(data, add_names(list(group_order(data, ...,
                                               .by = {{ .by }}, .cols = .cols,
                                               ascending = ascending)),
                              .name))
}
old_group_id <- function(data, ...,
                         order = TRUE,
                         ascending = TRUE,
                         .by = NULL, .cols = NULL,
                         .name = NULL,
                         as_qg = FALSE){
  fastplyr::add_group_id(
    data, ...,
    .order = order,
    .ascending = ascending,
    .by = {{ .by }},
    .cols = .cols,
    .name = ".internal.temp.group.id",
    as_qg = as_qg
  )[[".internal.temp.group.id"]]
}
