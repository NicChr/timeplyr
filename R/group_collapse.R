#' These functions have been superseded by fastplyr functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `r lifecycle::badge("superseded")`
#'
#' These functions can now be found in fastplyr. \cr
#' They are no longer recommended in this package and thus have been both
#' deprecated and superseded.
#'
#'
#' @param data A data frame or vector.
#' @param ... Additional groups using tidy `data-masking` rules. \cr
#' To specify groups using `tidyselect`, simply use the `.by` argument.
#' @param order Should the groups be ordered?
#' \bold{THE PHYSICAL ORDER OF THE DATA IS NOT CHANGED.} \cr
#' When order is `TRUE` (the default) the group IDs will be ordered but not sorted.
#' If `FALSE` the order of the group IDs will be based on first appearance.
#' @param sort Should the data frame be sorted by the groups?
#' @param ascending Should groups be ordered in ascending order?
#' Default is `TRUE` and only applies when `order = TRUE`.
#' @param .by Alternative way of supplying groups using `tidyselect` notation.
#' This is kept to be consistent with other functions.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param id Should group IDs be added? Default is `TRUE`.
#' @param size Should group sizes be added? Default is `TRUE`.
#' @param loc Should group locations be added? Default is `TRUE`.
#' @param start Should group start locations be added? Default is `TRUE`.
#' @param end Should group end locations be added? Default is `TRUE`.
#' @param .drop Should unused factor levels be dropped? Default is `TRUE`.
#'
#' @returns
#' A `tibble` of unique groups and an integer ID uniquely identifying each group.
#'
#' @rdname group_collapse
#' @export
group_collapse <- function(data, ..., order = TRUE, sort = FALSE,
                           ascending = TRUE,
                           .by = NULL, .cols = NULL,
                           id = TRUE,
                           size = TRUE, loc = TRUE,
                           # loc_order = TRUE,
                           start = TRUE, end = TRUE,
                           .drop = df_group_by_drop_default(data)){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "group_collapse()",
    details = "group_collapse has been deprecated and there is no alternative at the moment"
  )
  group_collapse <- get_from_package("group_collapse", "fastplyr")
  group_collapse(data, ..., order = order, sort = sort,
                 ascending = ascending, .by = {{ .by }},
                 .cols = .cols, id = id,
                 size = size, loc = loc,
                 start = start, end = end,
                 .drop = .drop)
}
