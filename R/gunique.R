#' Grouped `unique()`
#'
#' @description Return unique values by group.
#' When `g = NULL` this is equivalent to `unique()`.
#' For `lubridate` users, this is much faster than `unique()` for
#' intervals.
#'
#' @param x A vector or data frame.
#' @param g Group IDs passed directly to `collapse::GRP()`.
#' This can be a vector, list or data frame.
#' @export
gunique <- function(x, g = NULL){
  if (is.null(g)){
    g <- GRP2(x, sort = TRUE,
                       return.groups = FALSE, call = FALSE,
              return.order = FALSE)
  } else {
    g <- GRP2(dplyr::tibble(x, g), sort = TRUE,
                       return.groups = FALSE, call = FALSE,
              return.order = FALSE)
  }
  subset(x, growid(x, g = g) == 1)
}
