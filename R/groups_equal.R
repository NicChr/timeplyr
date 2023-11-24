#' Are groups equal?
#'
#' @description
#' This function is a very fast utility for quickly checking if
#' the group data between 2 data frames are identical.
#'
#' @param x A `grouped_df`.
#' @param y A `grouped_df`.
#'
#' @returns
#' A logical indicating whether the groups are identical or not.
#'
#' @examples
#' library(dplyr)
#' library(timeplyr)
#'
#' df <- iris %>%
#'   group_by(Species)
#' df2 <- iris %>%
#'   fslice_sample(seed = 1777) %>%
#'   group_by(Species)
#'
#' groups_equal(iris, iris)
#' groups_equal(df, df)
#' groups_equal(df, df2)
#'
#' library(bench)
#'
#' all_equal_grouped_df <- function(x, y){
#'   groups_equal(x, y) && all.equal(ungroup(x), ungroup(y))
#' }
#'
#' mark(all_equal_grouped_df(df, df2),
#'      isTRUE(all.equal(df, df2)),
#'      iterations = 20)
#'
#' mark(all_equal_grouped_df(df, df),
#'      isTRUE(all.equal(df, df)),
#'      iterations = 20)
#' @export
groups_equal <- function(x, y){
  groups1 <- attr(x, "groups")
  groups2 <- attr(y, "groups")
  group_vars1 <- group_vars(x)
  group_vars2 <- group_vars(y)
  out <- df_nrow(x) == df_nrow(y)
  if (out){
    out <- isTRUE(all.equal(names(groups1), names(groups2)))
  }
  if (out){
    out <- df_nrow(groups1) == df_nrow(groups2)
  }
  if (out){
    out <- cpp_group_data_rows_equal(groups1[[".rows"]], groups2[[".rows"]])
  }
  if (out){
    out <- is.null(groups1) && is.null(groups2) || (
      df_nrow(
        vctrs::vec_set_difference(
          fselect(groups1, .cols = group_vars1),
          fselect(groups2, .cols = group_vars2)
        )
      ) == 0L
    )
  }
  out
}
