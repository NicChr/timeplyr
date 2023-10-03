#' A `collapse` version of `dplyr::arrange()`
#'
#' @description
#' This is a fast and near-identical alternative to `dplyr::arrange()`
#' using the `collapse` package.
#'
#' `desc()` is like `dplyr::desc()` but works faster when
#' called directly on vectors. \cr
#'
#' @param data A data frame.
#' @param ... Variables to arrange by.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using `tidyselect`.
#' @param .by_group If `TRUE` the sorting will be first done by the group
#' variables.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @details
#' `farrange()` is inspired by `collapse::roworder()` but also supports
#' `dplyr` style `data-masking` which makes it a
#' closer replacement to `dplyr::arrange()`.
#'
#' You can use `desc()` interchangeably with `dplyr` and `timeplyr`. \cr
#' `arrange(iris, desc(Species))` uses `dplyr`'s version. \cr
#' `farrange(iris, desc(Species))` uses `timeplyr`'s version.
#'
#' `farrange()` is faster when there are many groups or a large number of
#' rows.
#' @return A sorted `data.frame`.
#' @export
farrange <- function(data, ..., .by = NULL, .by_group = FALSE,
                     .cols = NULL){
  group_info <- group_info(if (.by_group){
    data
  } else {
    safe_ungroup(data)
  }, ..., .by = {{ .by }},
  .cols = .cols,
  ungroup = TRUE,
  rename = FALSE)
  dot_vars <- fpluck(group_info, "extra_groups")
  all_vars <- fpluck(group_info, "all_groups")
  if (length(all_vars) == 0L){
    return(data)
  }
  if (.by_group){
    order_vars <- all_vars
  } else {
    order_vars <- dot_vars
  }
  out_order <- radixorderv2(
    fselect(
      fpluck(group_info, "data"), .cols = order_vars
    ),
    decreasing = FALSE, na.last = TRUE, starts = FALSE,
    group.sizes = FALSE, sort = TRUE
  )
  sorted <- attr(out_order, "sorted")
  if (isTRUE(sorted)){
    data
  } else {
    df_row_slice(data, out_order)
  }
}
