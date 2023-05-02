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
  group_vars <- get_groups(data, .by = {{ .by }})
  if ( ( length(group_vars) + dots_length(...) ) == 0){
    data
  } else {
    # Ungrouped mutate
    out <- dplyr::mutate(safe_ungroup(data),
                         across(all_of(group_vars)), ...,
                         .keep = "none")
    if (.by_group){
      group_info <- get_group_info(data, ...,
                                   type = "data-mask",
                                   .by = {{ .by }})
      order_vars <- group_info[["all_groups"]]
    } else {
      group_info <- get_group_info(safe_ungroup(data), ...,
                                   type = "data-mask",
                                   .by = NULL)
      order_vars <- group_info[["extra_groups"]]
    }
    df_row_slice(data, group_order.default(collapse::fselect(out, order_vars)),
                 reconstruct = FALSE)
  }
}
