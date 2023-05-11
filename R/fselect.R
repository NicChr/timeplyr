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
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
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
  data_nms <- names(data)
  group_vars <- group_vars(data)
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  if (length(group_vars) > 0L){
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
  }
  if (length(pos) == 0L){
    pos <- 0L
  }
  collapse::get_vars(data, vars = pos,
                     return = "data",
                     regex = FALSE,
                     rename = TRUE)
}
#' @export
frename <- function(data, ..., .cols = NULL){
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  col_rename(data, .cols = pos)
}
