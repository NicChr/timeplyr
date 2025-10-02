##### Data frame helpers #####

# Internal helpers from fastplyr

df_nrow <- function(x){
  length(attr(x, "row.names", TRUE))
}
df_ncol <- function(x){
  length(attr(x, "names", TRUE))
}
df_seq_along <- function (data, along = "rows"){
  switch(along, rows = seq_len(df_nrow(data)), seq_len(df_ncol(data)))
}
get_groups <- get_from_package("get_groups", "fastplyr")
df_rm_cols <- function(data, cols){
  cheapr::df_modify(data, `names<-`(cheapr::new_list(length(cols)), cols))
}
df_paste_names <- function (data, sep = "_", .cols = names(data)){
  do.call(paste, c(fastplyr::f_select(data, .cols = .cols), list(sep = sep)))
}

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

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.time_tbl_df <- function(data, template){
  cheapr::rebuild(data, template)
}
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.episodes_tbl_df <- function(data, template){
  cheapr::rebuild(data, template)
}
#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.time_tbl_df <- function(data, i, ..., .preserve = FALSE){
  cheapr::sset_df(data, i)
}
#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.episodes_tbl_df <- function(data, i, ..., .preserve = FALSE){
  cheapr::sset_df(data, i)
}

df_n_distinct <- function(data){
  GRP_n_groups(
    df_to_GRP(data, .cols = names(data),
              return.groups = FALSE, return.order = FALSE)
  )
}


mutate_one <- function(.data, expr, .by = NULL){
  group_vars <- fastplyr::f_group_vars(.data)
  out <- fastplyr::f_mutate(
    .data, {{ expr }}, .by = {{ .by }}, .keep = "none"
  )
  fastplyr::f_select(fastplyr::f_ungroup(out), .cols = setdiff(names(out), group_vars))
}


# A `data.table::setorder()` that works for any data frame
# df_set_order <- function(x, .cols = names(x), .order = 1L){
#
#   # Add objects to list in-place
#   cpp_set_list_element <- get_from_package("cpp_set_list_element", "fastplyr")
#
#   ## Make sure this only works for data frames of simple vectors
#
#   group_vars <- fastplyr::f_group_vars(x)
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
#   data.table::setorderv(
#     temp_list,
#     cols = tidy_select_names(x, .cols = .cols),
#     na.last = TRUE,
#     order = .order
#   )
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
#     groups <- fastplyr::f_group_data(fastplyr::f_group_by(fastplyr::f_ungroup(x), .cols = group_vars))
#     cheapr::attrs_modify(x, groups = groups, .set = TRUE)
#   } else {
#     x
#   }
# }
