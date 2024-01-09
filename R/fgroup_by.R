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
#' groups will be ordered based on first-appearance. \cr
#' Typically, setting order to `FALSE` is faster.
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
                      order = df_group_by_order_default(data),
                      .by = NULL, .cols = NULL,
                      .drop = df_group_by_drop_default(data)){
  init_group_vars <- group_vars(data)
  group_info <- tidy_group_info(safe_ungroup(data), ...,
                                .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = TRUE)
  out <- group_info[["data"]]
  groups <- group_info[["all_groups"]]
  if (.add){
    # dont_reorder_groups <- order == df_group_by_order_default(data)
    # recalculate_groups <- !dont_reorder_groups || !(.drop == df_group_by_drop_default(data))
    # no_extra_groups <- length(groups) == 0 || (length(setdiff2(groups, init_group_vars)) == 0)
    # if (!recalculate_groups && no_extra_groups){
    order_unchanged <- order == df_group_by_order_default(data)
    drop_unchanged <- .drop == df_group_by_drop_default(data)
    no_extra_groups <- length(groups) == 0 || (length(setdiff2(groups, init_group_vars)) == 0)
    groups_unchanged <- all(group_info$address_equal[init_group_vars])
    if (order_unchanged && drop_unchanged && no_extra_groups && groups_unchanged){
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
                             .drop = .drop)
    groups <- frename(groups, .cols = c(".rows" = ".loc"))
    attr(groups, ".drop") <- .drop
    attr(groups, "sorted") <- order
    attr(out, "groups") <- groups
    class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  }
  out
}
