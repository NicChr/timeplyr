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
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @export
fdistinct <- function(data, ..., .keep_all = FALSE,
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
  out <- fselect(out, .cols = out_vars)
  # # out <- fslice(out, vctrs::vec_unique_loc(collapse::fselect(out, dup_vars)))
  # g <- group_id(out, .by = all_of(dup_vars),
  #                order = TRUE,
  #                as_qg = TRUE)
  # out <- df_row_slice(out, which(!collapse::fduplicated(g)),
  #                     reconstruct = FALSE)
  out <- df_row_slice(out,
                      which(growid(fselect(out, .cols = dup_vars)) == 1L),
                      reconstruct = FALSE)
  df_reconstruct(out, data)
}
