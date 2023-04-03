#' A wrapper around `do.call()` and `data.table::CJ()`
#'
#' @description This is a wrapper around `data.table::CJ()` that accepts
#' a list or data.frame as an argument.
#' It also uses collapse to retrieve unique values if `unique = TRUE`.
#' @param X A list or data frame.
#' @param sort Should the expansion be sorted? By default it is `FALSE`.
#' @param unique Should unique values across each column or list element
#' be taken? By default this is `TRUE`.
#' @param log_limit The maximum log10 limit for expanded number of rows.
#' Anything >= this results in an error.
#' @return A data.table object
#' @examples
#' library(timeplyr)
#' crossed_join(list(1:10, 11:20))
#' crossed_join(iris, sort = TRUE)
#' @export
crossed_join <- function(X, sort = FALSE, unique = TRUE, log_limit = 8){
  if (unique){
    X <- lapply(X, function(x) collapse::funique(x, sort = sort))
  }
  # Usually sorting or not sorting happens in the above line
  # But if not then, let data.table sort
  if (!unique && sort){
    dt_sort <- TRUE
  } else {
    dt_sort <- FALSE
  }
  expanded_n <- prod(collapse::vlengths(X))
  if (log10(expanded_n) >= log_limit) {
    stop("Requested expansion results in >= ",
         expanded_n,
         " rows, aborting.")
  }
  do.call(CJ, args = c(X, list(sorted = dt_sort, unique = FALSE)))
}
