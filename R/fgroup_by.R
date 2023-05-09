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
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .drop Not currently supported.
#' Unused factor levels are always dropped.
#' @export
fgroup_by <- function(data, ..., .add = FALSE,
                      order = TRUE,
                      .by = NULL, .cols = NULL,
                      .drop = dplyr::group_by_drop_default(data)){
  init_group_vars <- group_vars(data)
  group_info <- group_info(safe_ungroup(data), ..., .by = {{ .by }},
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
                                 start = FALSE, end = FALSE)
    names(group_data)[names(group_data) == ".loc"] <- ".rows"
    attr(data, "groups") <- group_data
    attr(attr(data, "groups"), ".drop") <- .drop
    attr(data, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  }
  data
}
