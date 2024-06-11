ffilter <- function(data, ..., .by = NULL){
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                ungroup = TRUE,
                                rename = TRUE)
  filter_cols <- group_info[["extra_groups"]]
  filter_df <- fselect(group_info[["data"]], .cols = filter_cols)
  if (df_ncol(filter_df) < 1){
    data
  } else {
    stopifnot(all(vapply(filter_df, is.logical, FALSE)))
    df_row_slice(data, Reduce(`&`, filter_df))
  }
}
