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
  template <- data[1, , drop = FALSE]
  if (inherits(data, "grouped_df")){
    data <- dplyr::ungroup(data)
  }
  if (n_dots > 0){
    data <- data %>%
      dplyr::mutate(!!!enquos(...))
  }
  group_info <- get_group_info(template, !!!enquos(...), type = "data-mask",
                               .by = {{ .by }})
  group_vars <- group_info[["dplyr_groups"]]
  extra_groups <- group_info[["extra_groups"]]
  all_groups <- group_info[["all_groups"]]
  if (n_dots == 0L){
    dup_vars <- names(data)
    out_vars <- dup_vars
  } else {
    dup_vars <- all_groups
    if (.keep_all){
      out_vars <- names(data)
    } else {
      out_vars <- dup_vars
    }
  }
  data <- collapse::fselect(data, out_vars)
  # out <- fslice(data, vctrs::vec_unique_loc(collapse::fselect(data, dup_vars)))
  g <- group_id(data, all_of(dup_vars),
                 sort = TRUE,
                 as_qg = FALSE)
  data <- dplyr::filter(data, !collapse::fduplicated(g))
  df_reconstruct(data, template)
}
