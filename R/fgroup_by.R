#' 'collapse' version of `dplyr::group_by()`
#'
#' @description
#' This works the exact same as `dplyr::group_by()` and typically
#' performs around the same speed but uses slightly less memory.
#'
#' @param data data frame.
#' @param ... Variables to group by.
#' @param .add Should groups be added to existing groups?
#' Default is `FALSE`.
#' @param order Should groups be ordered? If `FALSE`
#' groups will be ordered based on first-appearance.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using `tidyselect`.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .drop Should unused factor levels be dropped? Default is `TRUE`.
#'
#' @details
#' `fgroup_by()` works almost exactly like the 'dplyr' equivalent.
#' An attribute "sorted" (`TRUE` or `FALSE`) is added to the group data to
#' signify if the groups are sorted or not.
#'
#' @returns
#' A `grouped_df`.
#'
#' @export
fgroup_by <- function(data, ..., .add = FALSE,
                      order = TRUE,
                      .by = NULL, .cols = NULL,
                      .drop = TRUE){
  init_group_vars <- group_vars(data)
  group_info <- tidy_group_info(safe_ungroup(data), ...,
                                .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = TRUE)
  out <- group_info[["data"]]
  groups <- group_info[["all_groups"]]
  if (.add){
    if (length(groups) == 0){
      return(data)
    }
    groups <- unique(c(init_group_vars, groups))
  }
  if (length(groups) > 0L){
    groups <- group_collapse(out, .cols = groups,
                             order = order,
                             id = FALSE,
                             loc = TRUE, sort = TRUE,
                             size = FALSE,
                             start = FALSE, end = FALSE,
                             drop = .drop)
    groups <- frename(groups, .cols = c(".rows" = ".loc"))
    attr(groups, ".drop") <- .drop
    attr(groups, "sorted") <- order
    attr(out, "groups") <- groups
    class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  }
  out
}
# An simple implementations of dplyr::count (without weights)
# fcount2 <- function(data, ..., order = TRUE, .by = NULL, .cols = NULL){
#  out <- fgroup_by(data, ..., .cols = .cols, .by = {{ .by }}, order = order,
#                   .add = TRUE)
#  groups <- group_data(out)
#  out <- fselect(groups, .cols = setdiff2(names(groups), ".rows"))
#  counts <- collapse::vlengths(groups[[".rows"]])
#  out <- df_add_cols(out, list(n = counts))
#  df_reconstruct(out, data)
# }
# fadd_count2 <- function(data, ..., order = TRUE, .by = NULL, .cols = NULL){
#   out <- fgroup_by(data, ..., .cols = .cols, .by = {{ .by }}, order = order,
#                    .add = TRUE)
#   groups <- group_data(out)
#   group_ids <- df_group_id(out)
#   counts <- collapse::vlengths(groups[[".rows"]])[group_ids]
#   out <- df_add_cols(out, list(n = counts))
#   df_reconstruct(out, data)
# }
# fcount3 <- function(data, ..., order = TRUE, .by = NULL, .cols = NULL, name = NULL){
#   out <- group_collapse(data, ..., .cols = .cols, .by = {{ .by }},
#                         order = order,
#                         sort = TRUE,
#                         start = FALSE, end = FALSE, loc = FALSE, size = TRUE,
#                         id = FALSE)
#   count_nm <- name
#   if (is.null(count_nm)){
#     count_nm <- new_n_var_nm(out)
#   }
#   out <- frename(out, .cols = add_names(c(".size"), count_nm))
#   df_reconstruct(out, data)
# }
