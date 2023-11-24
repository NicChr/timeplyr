#' Fast `dplyr::select()`/`dplyr::rename()`
#'
#' @description
#' `fselect()` operates the exact same way as `dplyr::select()` and
#' can be used naturally with `tidy-select` helpers.
#' It uses collapse to perform the actual selecting of variables and is
#' considerably faster than dplyr for selecting exact columns,
#' and even more so when supplying the `.cols` argument.
#'
#' @param data A data frame.
#' @param ... Variables to select using `tidy-select`.
#' See `?dplyr::select` for more info.
#' @param .cols (Optional) faster alternative to `...` that accepts
#' a named character vector or numeric vector. \cr
#' No checks on duplicates column names are done when using `.cols`. \cr
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @returns
#' A `data.frame` of selected columns.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' df <- slice_head(iris, n = 5)
#' fselect(df, Species, SL = Sepal.Length)
#' fselect(df, .cols = c("Species", "Sepal.Length"))
#' fselect(df, all_of(c("Species", "Sepal.Length")))
#' fselect(df, 5, 1)
#' fselect(df, .cols = c(5, 1))
#' df %>%
#'   fselect(where(is.numeric))
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
fselect <- function(data, ..., .cols = NULL){
  UseMethod("fselect")
}
#' @export
fselect.data.frame <- function(data, ..., .cols = NULL){
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  out <- collapse::ss(data, j = unname(pos), check = FALSE)
  names(out) <- names(pos)
  out
}
#' @export
fselect.grouped_df <- function(data, ..., .cols = NULL){
  data_nms <- names(data)
  group_vars <- group_vars(data)
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  group_pos <- add_names(match(group_vars, data_nms), group_vars)
  pos_nms <- names(pos)
  # Add group vars missed
  groups_missed <- group_pos[match(group_pos, pos, 0L) == 0L]
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
  group_pos <- cpp_which(group_vars %in% names(data)[pos])
  names(group_pos) <- names(out)[cpp_which(names(out) %in% names(pos) &
                                             names(data) %in% group_vars)]
  groups <- col_rename(groups, .cols = group_pos)
  attr(out, "groups") <- groups
  class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  out
}
# This should be unecessary but data.table:::`names<-.data.table`
# Sometimes reduces the allocated column slots
#' @export
frename.data.table <- function(data, ..., .cols = NULL){
  out_allocated_length <- data.table::truelength(data) - collapse::fncol(data)
  out_allocated_length <- max(out_allocated_length, 0L)
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  data <- col_rename(data, .cols = pos)
  invisible(
    data.table::setalloccol(data, n = out_allocated_length)
  )
  data
}
