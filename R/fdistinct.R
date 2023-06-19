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
  out <- collapse::fselect(out, out_vars)
  # Method 1 - Using vctrs
  # # out <- fslice(out, vctrs::vec_unique_loc(collapse::fselect(out, dup_vars)))
  # Method 2 - Using group ID integer + duplicated()
  # g <- group_id(out, .cols = dup_vars,
  #                order = TRUE,
  #                as_qg = TRUE)
  # out <- df_row_slice(out, which(!collapse::fduplicated(g)),
  #                     reconstruct = FALSE)
  # Method 3 - Using GRP() + group_starts, though things need to be reordered
  # g <- GRP2(fselect(out, .cols = dup_vars),
  #           sort = TRUE,
  #           return.groups = FALSE, call = FALSE,
  #           return.order = FALSE)
  # i <- GRP_starts(g)[collapse::funique(g[["group.id"]], sort = FALSE)]
  # out <- df_row_slice(out, i, reconstruct = FALSE)
  # Method 4 - Using grouped row numbers
  # out <- df_row_slice(out,
  #                     growid(fselect(out, .cols = dup_vars)) == 1L,
  #                     reconstruct = FALSE)
  g <- GRP2(collapse::fselect(out, dup_vars),
            sort = TRUE,
            return.groups = TRUE, call = FALSE,
            return.order = TRUE)
  # iunique <- fcumsum(seq_ones(nrow2(out)), g = g, na.rm = FALSE) == 1L
  if (sort){
    iunique <- GRP_starts(g)
  } else {
    iunique <- collapse::whichv(frowid(out, g = g), 1L)
  }
  out <- df_row_slice(out, iunique, reconstruct = FALSE)
  df_reconstruct(out, data)
}
