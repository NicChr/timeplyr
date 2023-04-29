#' Grouped `unique()`
#'
#' @description Return unique values by group.
#' When `g = NULL` this is equivalent to `unique()`.
#' For `lubridate` users, this is much faster than `unique()` for
#' intervals.
#'
#' For a more tidyverse friendly version for data frames, see `?fdistinct`.
#'
#' @param x A vector or data frame.
#' @param g Group IDs passed directly to `collapse::GRP()`.
#' This can be a vector, list or data frame.
#' @param sort Should unique values be sorted?
#' The default is `FALSE`.
#' @export
gunique <- function(x, g = NULL, sort = FALSE){
  if (is.null(g)){
    g <- GRP2(x, sort = TRUE,
              return.groups = TRUE, call = FALSE,
              return.order = FALSE)
  } else {
    g <- GRP2(dplyr::tibble(x, g), sort = TRUE,
              return.groups = TRUE, call = FALSE,
              return.order = FALSE)
  }
  if (sort){
    vctrs::vec_slice(x, g[["group.starts"]])
  } else {
    vctrs::vec_slice(x, which(growid(x, g = g) == 1))
  }
}
