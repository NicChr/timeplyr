#' Find distinct rows
#'
#' @description Like `dplyr::distinct()` but faster when lots of
#' groups are involved.
#' @param data A data frame.
#' @param ... Variables used to find distinct rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @export
fdistinct <- function(data, ..., .keep_all = FALSE, .by = NULL){
  out <- data %>%
    safe_ungroup() %>%
    dplyr::mutate(!!!enquos(...))
  group_info <- get_group_info(data, !!!enquos(...), type = "data-mask",
                               .by = {{ .by }})
  group_vars <- group_info[["dplyr_groups"]]
  extra_groups <- group_info[["extra_groups"]]
  all_groups <- group_info[["all_groups"]]
  # Check if group id colname exists
  grp_nm <- new_var_nm(names(data), "group_id")
  if (dots_length(...) == 0L){
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
  out <- out %>%
    dplyr::select(all_of(out_vars))
  g <- group_id(out, all_of(dup_vars),
                 sort = FALSE,
                 as_qg = FALSE)
  out <- out[!collapse::fduplicated(g, all = FALSE), , drop = FALSE]
  df_reconstruct(out, data)
}
