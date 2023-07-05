#' Find distinct rows
#'
#' @description Like `dplyr::distinct()` but faster when lots of
#' groups are involved.
#' @param data A data frame.
#' @param ... Variables used to find distinct rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
#' @param sort Should result be sorted? Default is `FALSE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @export
fdistinct <- function(data, ..., .keep_all = FALSE,
                      sort = FALSE,
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
  g <- df_to_GRP(out, .cols = dup_vars)
  if (sort){
    iunique <- GRP_starts(g)
  } else {
    # The reason for using row IDs when groups are sorted
    # is that frowid() utilises sequence() in this scenario
    # Which is very efficient
    # You can use either method irregardless though which is nice.
    if (GRP_is_sorted(g)){
      if (length(dup_vars) == 0L){
        g <- NULL
      }
      iunique <- collapse::whichv(frowid(out, g = g), 1L)
    } else {
      iunique <- collapse::whichv(GRP_duplicated(g), FALSE)
    }
  }
  out <- df_row_slice(out, iunique, reconstruct = FALSE)
  df_reconstruct(out, data)
}
