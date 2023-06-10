#' Fast `dplyr::select()`/`dplyr::rename()`
#'
#' @description `fselect()` operates the exact same way as `dplyr::select()` and
#' can be used naturally with `tidyselect` helpers.
#' It uses `collapse` to perform the actual selecting of variables and is
#' considerably faster than `dplyr` for selecting exact columns,
#' and even more so when supplying the `.cols` argument.
#'
#' @param data A data frame.
#' @param ... Variables to select using `tidyselect`.
#' See `?dplyr::select` for more info.
#' @param .cols (Optional) faster alternative to `...` that accepts
#' a named character vector or numeric vector. \cr
#' No checks on duplicates column names are done when using `.cols`. \cr
#' If speed is an expensive resource, it is recommended to use this.
#' @examples
#' \dontrun{
#'   library(timeplyr)
#'   library(dplyr)
#'   bench::mark(e1 = fselect(iris, Species, Sepal.Length),
#'               e2 = fselect(iris, .cols = c("Species", "Sepal.Length")),
#'               e3 = fselect(iris, all_of(c("Species", "Sepal.Length"))),
#'               e4 = select(iris, all_of(c("Species", "Sepal.Length"))))
#' }
#' @export
fselect <- function(data, ..., .cols = NULL){
  UseMethod("fselect")
}
#' @export
fselect.data.frame <- function(data, ..., .cols = NULL){
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  out <- collapse::ss(data, j = unname(pos))
  names(out) <- names(pos)
  out
}
#' @export
fselect.grouped_df <- function(data, ..., .cols = NULL){
  data_nms <- names(data)
  group_vars <- group_vars(data)
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  group_pos <- setnames(match(group_vars, data_nms),
                        group_vars)
  pos_nms <- names(pos)
  # Add group vars missed
  groups_missed <- setdiff(group_pos, pos)
  if (length(groups_missed) > 0L){
    text1 <- "Adding missing grouping variables: "
    message(
      paste0(text1,
             "'", paste(data_nms[groups_missed],
                        collapse = "', '"), "'")
    )
    pos <- c(groups_missed, pos)
    names(pos) <- c(data_nms[groups_missed], pos_nms)
  }
  renamed_groups <- pos[pos %in% group_pos &
                          !names(pos) %in% names(group_pos)]
  if (length(renamed_groups) > 0L){
    original_nms <- data_nms[unname(renamed_groups)]

    names(attr(data, "groups"))[
      match(original_nms,
            names(attr(data, "groups")))] <- names(renamed_groups)
  }
  groups <- group_data(data)
  out <- collapse::ss(safe_ungroup(data), j = unname(pos))
  names(out) <- names(pos)
  attr(out, "groups") <- groups
  class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  out
}
#' @rdname fselect
#' @export
frename <- function(data, ..., .cols = NULL){
  UseMethod("frename")
}
#' @export
frename.data.frame <- function(data, ..., .cols = NULL){
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  col_rename(data, .cols = pos)
}
#' @export
frename.grouped_df <- function(data, ..., .cols = NULL){
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  groups <- group_data(data)
  group_vars <- setdiff(names(groups), ".rows")
  # Rename data columns
  out <- col_rename(safe_ungroup(data), .cols = pos)
  # Rename group data columns
  groups <- col_rename(groups, .cols = pos[match(group_vars, names(data))])
  attr(out, "groups") <- groups
  class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  out
}
