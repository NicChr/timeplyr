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
  n_dots <- dots_length(...)
  out <- safe_ungroup(data)
  if (n_dots > 0){
    out <- dplyr::mutate(out, !!!enquos(...))
  }
  group_info <- get_group_info(data, !!!enquos(...),
                               type = "data-mask",
                               .by = {{ .by }})
  group_vars <- group_info[["dplyr_groups"]]
  extra_groups <- group_info[["extra_groups"]]
  all_groups <- group_info[["all_groups"]]
  if (n_dots == 0){
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
  out <- collapse::fselect(out, out_vars)
  # out <- fslice(out, vctrs::vec_unique_loc(collapse::fselect(out, dup_vars)))
  g <- group_id(out, .by = all_of(dup_vars),
                 order = TRUE,
                 as_qg = TRUE)
  out <- df_row_slice(out, which(!collapse::fduplicated(g)),
                      reconstruct = FALSE)
  df_reconstruct(out, data)
}
