#' Grouped `unique()`
#'
#' @description Return unique values by group.
#' When `g = NULL` this is equivalent to `unique()`.
#'
#' @param x A vector or data frame.
#' @param g Group IDs passed directly to `collapse::GRP()`.
#' This can be a vector, list or data frame.
#' @export
gunique <- function(x, g = NULL){
  if (is.null(g)){
    g <- GRP2(x, sort = TRUE,
                       return.groups = FALSE, call = FALSE)
  } else {
    g <- GRP2(data.frame(x, g), sort = TRUE,
                       return.groups = FALSE, call = FALSE)
  }
  if (isTRUE(nrow2(x) == 0 || ncol(x) == 0)){
    subset(x, growid(x, g = g) == 1)
  } else {
    collapse::fsubset(x, growid(x, g = g) == 1)
  }
}
