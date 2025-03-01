##### Data frame helpers #####

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
df_ungroup <- fastplyr::f_ungroup
df_init <- get_from_package("df_init", "fastplyr")
df_paste_names <- get_from_package("df_paste_names", "fastplyr")

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
    cli::cli_abort("{.arg x} must be a data.frame")
  }
}

# Reorder data frame to original order after having sorted it using a GRP
# df_reorder <- function(data, g){
#   df_row_slice(data, greorder2(df_seq_along(data, "rows"), g = g))
# }

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

# A `data.table::setorder()` that works for any data frame
# df_set_order <- function(x, .cols = names(x), .order = 1L){
#
#   ## Make sure this only works for data frames of simple vectors
#
#   group_vars <- group_vars(x)
#
#   temp_list <- cheapr::new_list(length(names(x)))
#   names(temp_list) <- names(x)
#   for (i in seq_along(temp_list)){
#     cpp_set_list_element(temp_list, i, x[[i]])
#   }
#
#   # setDT() creates a sort of shallow copy
#   # so we can't directly use it on x
#   data.table::setDT(temp_list)
#   data.table::setorderv(temp_list, cols = col_select_names(x, .cols), na.last = TRUE, order = .order)
#
#   # Add cols back to x by reference
#   # This ensures materialised ALTREP objects in temp_list
#   # are definitely copied back to x
#
#   for (i in seq_along(temp_list)){
#     cpp_set_list_element(x, i, temp_list[[i]])
#   }
#   if (length(group_vars) > 0){
#     # Add re-calculated group data
#     groups <- group_data(fastplyr::f_group_by(fastplyr::f_ungroup(x), .cols = group_vars))
#     set_add_attr(x, "groups", groups)
#   } else {
#     x
#   }
# }
