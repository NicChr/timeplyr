#' `collapse` version of `dplyr::group_by()`
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
#' @param .drop Not currently supported.
#' Unused factor levels are always dropped.
#' @export
fgroup_by <- function(data, ..., .add = FALSE,
                      order = TRUE,
                      .by = NULL,
                      .drop = dplyr::group_by_drop_default(data)){
  n_dots <- dots_length(...)
  init_group_vars <- group_vars(data)
  data <- safe_ungroup(data)
  if (n_dots > 0){
    data <- dplyr::mutate(data, ...)
  }
  group_info <- get_group_info(data, ...,
                               .by = {{ .by }},
                               type = "data-mask")
  group_vars <- group_info[["dplyr_groups"]]
  data_vars <- group_info[["extra_groups"]]
  all_vars <- group_info[["all_groups"]]
  groups <- data_vars
  if (.add){
    groups <- c(init_group_vars, groups)
  }
  group_data <- group_collapse(data, .by = all_of(groups),
                               order = order,
                               loc = TRUE, sort = TRUE,
                               size = FALSE,
                               start = FALSE, end = FALSE)
  group_data[[".group"]] <- NULL
  names(group_data)[names(group_data) == ".loc"] <- ".rows"
  attr(data, "groups") <- group_data
  attr(attr(data, "groups"), ".drop") <- .drop
  attr(data, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  data
}
# Alternate version
# fgroup_by <- function(data, ..., .add = FALSE,
#                       order = TRUE,
#                       .by = NULL,
#                       .drop = dplyr::group_by_drop_default(data)){
#   n_dots <- dots_length(...)
#   if (!.add){
#     data <- safe_ungroup(data)
#   }
#   if (n_dots > 0){
#     data <- dplyr::mutate(data, ...,
#                           .by = {{ .by }})
#   }
#   group_info <- get_group_info(data, ...,
#                                .by = {{ .by }},
#                                type = "data-mask")
#   group_vars <- group_info[["dplyr_groups"]]
#   data_vars <- group_info[["extra_groups"]]
#   all_vars <- group_info[["all_groups"]]
#   group_data <- group_collapse(data, across(all_of(data_vars)),
#                                order = order,
#                                loc = TRUE, sort = TRUE,
#                                size = FALSE,
#                                start = FALSE, end = FALSE,
#                                .by = {{ .by }})
#   group_data[[".group"]] <- NULL
#   names(group_data)[names(group_data) == ".loc"] <- ".rows"
#   attr(data, "groups") <- group_data
#   attr(attr(data, "groups"), ".drop") <- .drop
#   attr(data, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")
#   data
# }
