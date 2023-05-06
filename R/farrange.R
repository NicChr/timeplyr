#' A `collapse` version of `dplyr::arrange()`
#'
#' @description
#' This is a fast and near-identical alternative to `dplyr::arrange()`
#' using the `collapse` package.
#'
#' `desc()` is like `dplyr::desc()` but works faster when
#' called directly on vectors. \cr
#' You can use `desc()` interchangeably with `dplyr` and `timeplyr`. \cr
#' `arrange(iris, desc(Species))` uses `dplyr`'s version. \cr
#' `farrange(iris, desc(Species))` uses `timeplyr`'s version.
#'
#' `farrange()` is faster when there are many groups or a large number of
#' rows.
#'
#' @param data A data frame.
#' @param ... Variables to group by.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .by_group If `TRUE` the sorting will be first done by the group
#' variables.
#' @export
farrange <- function(data, ..., .by = NULL, .by_group = FALSE){
  n_dots <- dots_length(...)
  group_vars <- get_groups(data, .by = {{ .by }})
  if ( ( length(group_vars) + n_dots ) == 0){
    data
  } else {
    out <- safe_ungroup(data)
    if (n_dots > 0){
      # Ungrouped mutate
      out <- mutate2(out, ...)
      dot_vars <- tidy_transform_names(data, ...)
    } else {
      dot_vars <- character(0)
    }
    if (.by_group){
      order_vars <- c(group_vars, dot_vars)
    } else {
      order_vars <- dot_vars
    }
    gorder <- group_order.default(collapse::fselect(out, order_vars))
    if (is_strictly_increasing(gorder)){
      data
    } else {
      df_row_slice(data, gorder, reconstruct = FALSE)
    }
  }
}
