##### Data frame helpers #####


get_from_package <- function(x, package){
  get(x, asNamespace(package), inherits = FALSE)
}
# Internal helpers from fastplyr

df_nrow <- get_from_package("df_nrow", "fastplyr")
df_ncol <- get_from_package("df_ncol", "fastplyr")
df_seq_along <- get_from_package("df_seq_along", "fastplyr")
group_vars <- get_from_package("group_vars", "fastplyr")
group_data <- get_from_package("group_data", "fastplyr")
get_groups <- get_from_package("get_groups", "fastplyr")
df_row_slice <- get_from_package("df_row_slice", "fastplyr")
df_add_cols <- get_from_package("df_add_cols", "fastplyr")
df_rm_cols <- get_from_package("df_rm_cols", "fastplyr")
df_rep <- get_from_package("df_rep", "fastplyr")
df_rep_each <- get_from_package("df_rep_each", "fastplyr")
df_nrow <- get_from_package("df_nrow", "fastplyr")
df_ungroup <- get_from_package("df_ungroup", "fastplyr")
df_init <- get_from_package("df_init", "fastplyr")
df_paste_names <- get_from_package("df_paste_names", "fastplyr")
df_group_by_drop_default <- get_from_package("df_group_by_drop_default", "fastplyr")
df_group_by_order_default <- get_from_package("df_group_by_order_default", "fastplyr")

reconstruct <- get_from_package("reconstruct", "fastplyr")

df_group_id <- function(data){
  fastplyr::add_group_id(data, .name = ".group.id")[[".group.id"]]
}

# Convenience function
is_df <- function(x){
  inherits(x, "data.frame")
}

check_is_df <- function(x){
  if (!is_df(x)){
    stop(paste(deparse1(substitute(x)), "must be a data.frame"))
  }
}
# list() that removes NULL elements
list3 <- function(...){
  list_rm_null(list(...))
}
# list to tibble/DT
# No checks are done so use with caution
# Cannot contain duplicate names
# or different length list elements
list_as_tbl <- function(x){
  df_as_tbl(list_as_df(x))
}

# Create new df with no name checks or length checks
# ..N is there purely to create an (n > 0) x 0 data frame
new_df <- cheapr::new_df
new_tbl <- fastplyr::new_tbl

df_as_df <- function(x){
  list_as_df(x)
}
# Faster as_tibble
df_as_tbl <- function(x){
  out <- list_as_df(x)
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}

# Reorder data frame to original order after having sorted it using a GRP
df_reorder <- function(data, g){
  df_row_slice(data, greorder2(df_seq_along(data, "rows"), g = g))
}

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.time_tbl_df <- function(data, template){
  reconstruct(template, data)
}
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.episodes_tbl_df <- function(data, template){
  reconstruct(template, data)
}
#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.time_tbl_df <- function(data, i, ..., .preserve = FALSE){
  df_row_slice(data, i)
}
#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.episodes_tbl_df <- function(data, i, ..., .preserve = FALSE){
  df_row_slice(data, i)
}

df_n_distinct <- function(data){
  GRP_n_groups(
    df_to_GRP(data, .cols = names(data),
              return.groups = FALSE, return.order = FALSE)
  )
}
