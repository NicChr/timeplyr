ffilter <- function(data, ..., .by = NULL){
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                ungroup = TRUE,
                                rename = TRUE)
  filter_cols <- group_info[["extra_groups"]]
  filter_df <- fselect(group_info[["data"]], .cols = filter_cols)
  filter_ncol <- df_ncol(filter_df)
  if (filter_ncol < 1){
    out <- data
  } else if (filter_ncol == 1){
    out <- df_row_slice(data, filter_df[[1L]])
  } else {
    out <- df_row_slice(data, Reduce(`&`, filter_df))
  }
  out
}
