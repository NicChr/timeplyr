#' Find distinct rows
#'
#' @description Like `dplyr::distinct()` but faster when lots of
#' groups are involved.
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
#' @seealso [group_collapse] [duplicate_rows]
#'
#' @returns
#' A `data.frame` of distinct groups.
#'
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(ggplot2)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' mpg %>%
#'   distinct(manufacturer)
#' mpg %>%
#'   fdistinct(manufacturer)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
fdistinct <- function(data, ..., .keep_all = FALSE,
                       sort = FALSE, order = sort,
                      .by = NULL, .cols = NULL){
  n_dots <- dots_length(...)
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
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
  if (length(group_info[["extra_groups"]]) == 0L && !group_info[["groups_changed"]]){
    out <- data
  }
  out <- fselect(out, .cols = out_vars)
  # Using sort algorithm but returning order-of-first appearance groups
  if (order && !sort){
    unique_locs <- which_val(frowid(fselect(out, .cols = dup_vars)), 1L)
    slice <- !(length(unique_locs) == df_nrow(out) && is_sorted(unique_locs))
  } else {
    if (order && sort){
      o <- radixorderv2(fselect(out, .cols = dup_vars), starts = TRUE)
      unique_locs <- o[attr(o, "starts")]
      slice <- !(length(unique_locs) == df_nrow(out) &&
                   isTRUE(attr(o, "sorted")))
    } else {
      groups <- group2(fselect(out, .cols = dup_vars), starts = TRUE)
      unique_locs <- attr(groups, "starts")
      slice <- !(length(unique_locs) == df_nrow(out))
    }
  }
  if (slice){
    out <- cheapr::sset(out, unique_locs)
  }
  df_reconstruct(out, data)
}
