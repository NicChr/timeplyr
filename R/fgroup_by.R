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
  group_info <- tidy_group_info(safe_ungroup(data), ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = TRUE)
  data <- group_info[["data"]]
  groups <- group_info[["all_groups"]]
  if (.add){
    groups <- c(init_group_vars, groups)
  }
  if (length(groups) > 0L){
    group_data <- group_collapse(data, .cols = groups,
                                 order = order,
                                 id = FALSE,
                                 loc = TRUE, sort = TRUE,
                                 size = FALSE,
                                 start = FALSE, end = FALSE,
                                 drop = .drop)
    group_data <- frename(group_data, .cols = c(".rows" = ".loc"))
    attr(group_data, ".drop") <- .drop
    attr(group_data, "sorted") <- order
    attr(data, "groups") <- group_data
    attr(data, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  }
  data
}
