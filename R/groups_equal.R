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
#' @export
groups_equal <- function(x, y){
  groups1 <- strip_attrs(attr(x, "groups"))
  groups2 <- strip_attrs(attr(y, "groups"))
  identical(groups1, groups2)
  # group_vars1 <- group_vars(x)
  # group_vars2 <- group_vars(y)
  # out <- df_nrow(x) == df_nrow(y)
  # if (out){
  #   out <- identical(group_vars1, group_vars2)
  # }
  # if (out){
  #   out <- df_nrow(groups1) == df_nrow(groups2)
  # }
  # if (out){
  #   out <- identical(groups1[[".rows"]], groups2[[".rows"]])
  #   # out <- cpp_group_data_rows_equal(groups1[[".rows"]], groups2[[".rows"]])
  # }
  # if (out && ( length(group_vars1) + length(group_vars2) ) > 0){
  #   for (a in setdiff2(names(attributes(groups1)), c("names", "row.names", "class", ".drop"))){
  #     attr(groups1, a) <- NULL
  #   }
  #   for (a in setdiff2(names(attributes(groups2)), c("names", "row.names", "class", ".drop"))){
  #     attr(groups2, a) <- NULL
  #   }
  #   out <- identical(groups1, groups2)
  #     # groups1 <- fselect(groups1, .cols = group_vars1)
  #     # groups2 <- fselect(groups2, .cols = group_vars1)
  #     # for (i in seq_len(df_ncol(groups1))){
  #     #   equal <- identical(groups1[[i]], groups2[[i]])
  #     #   if (!equal){
  #     #     break
  #     #   }
  #     # }
  #     # out <- equal
  #   }
  # out
}
