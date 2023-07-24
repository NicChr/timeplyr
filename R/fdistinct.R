#' Find distinct rows
#'
#' @description Like `dplyr::distinct()` but faster when lots of
#' groups are involved.
#' @param data A data frame.
#' @param ... Variables used to find distinct rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
#' @param sort Should result be sorted? Default is `FALSE`.
#' When `order = FALSE` this option has no effect on the result.
#' @param order Should the groups be calculated as ordered groups?
#' This can be beneficial speed-wise to set to `TRUE`
#' when the data are large and messy. The default is `FALSE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @return A data frame of unique groups.
#' @details See `?group_collapse` for a more flexible method.
#' @seealso \link[timeplyr]{group_collapse}
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(ggplot2)
#'
#' mpg %>%
#'   distinct(manufacturer)
#' mpg %>%
#'   fdistinct(manufacturer)
#'
#' @export
fdistinct <- function(data, ..., .keep_all = FALSE,
                      sort = FALSE, order = sort,
                      .by = NULL, .cols = NULL){
  n_dots <- dots_length(...)
  group_info <- group_info(data, ..., .by = {{ .by }},
                           .cols = .cols,
                           ungroup = TRUE,
                           rename = TRUE)
  all_groups <- group_info[["all_groups"]]
  out <- group_info[["data"]]
  if (n_dots == 0 && is.null(.cols)){
    dup_vars <- names(out)
    out_vars <- dup_vars
  } else {
    dup_vars <- all_groups
    if (.keep_all){
      out_vars <- names(out)
    } else {
      out_vars <- dup_vars
    }
  }
  if (length(group_info[["extra_groups"]]) == 0L){
   out <- data
  }
  out <- fselect(out, .cols = out_vars)
  order_sort_alike <- !order || (order && sort)
  g <- df_to_GRP(out, .cols = dup_vars, order = order,
                 return.order = FALSE,
                 return.groups = order_sort_alike)
  GRP_has_groups <- !is.null(GRP_groups(g))
  use_GRP_groups <- order_sort_alike && GRP_has_groups && !.keep_all
  if (use_GRP_groups){
    out <- GRP_groups(g)
  } else {
    if (order_sort_alike){
      unique_locs <- GRP_starts(g)
    } else {
      unique_locs <- collapse::whichv(frowid(out, g = g), 1L)
    }
    out <- df_row_slice(out, unique_locs, reconstruct = FALSE)
  }
  out <- fselect(out, .cols = out_vars)
  df_reconstruct(out, data)
}
# Simplest version
# fdistinct <- function(data, ..., .keep_all = FALSE,
#                       .by = NULL, .cols = NULL){
#   n_dots <- dots_length(...)
#   group_info <- group_info(data, ..., .by = {{ .by }},
#                            .cols = .cols,
#                            ungroup = TRUE,
#                            rename = TRUE)
#   all_groups <- group_info[["all_groups"]]
#   out <- group_info[["data"]]
#   if (n_dots == 0 && is.null(.cols)){
#     dup_vars <- names(out)
#     out_vars <- dup_vars
#   } else {
#     dup_vars <- all_groups
#     if (.keep_all){
#       out_vars <- names(out)
#     } else {
#       out_vars <- dup_vars
#     }
#   }
#   if (length(group_info[["extra_groups"]]) == 0L){
#     out <- data
#   }
#   out <- fselect(out, .cols = out_vars)
#   g <- df_to_GRP(out, .cols = dup_vars,
#                  return.order = FALSE,
#                  return.groups = FALSE)
#   unique_locs <- collapse::whichv(frowid(out, g = g), 1L)
#   out <- df_row_slice(out, unique_locs, reconstruct = FALSE)
#   df_reconstruct(out, data)
# }
# Slightly simpler version of above
# fdistinct <- function(data, ..., .keep_all = FALSE,
#                       sort = FALSE, order = sort,
#                       .by = NULL, .cols = NULL){
#   n_dots <- dots_length(...)
#   group_info <- group_info(data, ..., .by = {{ .by }},
#                            .cols = .cols,
#                            ungroup = TRUE,
#                            rename = TRUE)
#   all_groups <- group_info[["all_groups"]]
#   out <- group_info[["data"]]
#   if (n_dots == 0 && is.null(.cols)){
#     dup_vars <- names(out)
#     out_vars <- dup_vars
#   } else {
#     dup_vars <- all_groups
#     if (.keep_all){
#       out_vars <- names(out)
#     } else {
#       out_vars <- dup_vars
#     }
#   }
#   if (length(group_info[["extra_groups"]]) == 0L){
#     out <- data
#   }
#   out <- fselect(out, .cols = out_vars)
#   g <- df_to_GRP(out, .cols = dup_vars, order = order,
#                  return.order = FALSE,
#                  return.groups = FALSE)
#   if (sort){
#     unique_locs <- GRP_starts(g)
#   } else {
#     unique_locs <- collapse::whichv(frowid(out, g = g), 1L)
#   }
#   out <- df_row_slice(out, unique_locs, reconstruct = FALSE)
#   df_reconstruct(out, data)
# }
