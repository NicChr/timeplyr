#' Alternative to `dplyr::group_keys()`
#'
#' @description
#' This works like `dplyr::group_keys()` but offers more
#' flexibility in how the groups are ordered/sorted.
#' It is similar to `dplyr::distinct()` but the result is never grouped.
#'
#' @param data Data frame.
#' @param ... Additional groups using tidy `data-masking` rules. \cr
#' To specify groups using `tidyselect`, simply use the `.by` argument.
#' @param order Should the groups be ordered?
#' \bold{THE PHYSICAL ORDER OF THE DATA IS NOT CHANGED.} \cr
#' When order is `TRUE` (the default) the group IDs will be ordered but not sorted.
#' If `FALSE` the order of the group IDs will be based on first appearance.
#' @param sort Should the data frame be sorted by the groups?
#' @param .by Alternative way of supplying groups using `tidyselect` notation.
#' This is kept to be consistent with other functions.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' iris %>%
#'   group_by(Species) %>%
#'   unique_groups()
#' iris %>%
#'   fslice_sample(seed = 920) %>%
#'   unique_groups(.by = Species, order = FALSE)
#' iris %>%
#'   unique_groups(across(contains("width"), round))
#' @export
unique_groups <- function(data, ..., order = TRUE, sort = order,
                          .by = NULL, .cols = NULL){
  group_collapse(data, ...,
                 order = order, sort = sort,
                 .by = {{ .by }}, .cols = .cols,
                 id = FALSE, loc = FALSE,
                 start = FALSE, end = FALSE,
                 size = FALSE)
}
