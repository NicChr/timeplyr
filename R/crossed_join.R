#' A `do.call()` and `data.table::CJ()` method
#'
#' @description This function operates like `do.call(CJ, ...)` and accepts
#' a list or data.frame as an argument. \cr
#' It has less overhead for small joins, especially when `unique = FALSE` and
#' `as_dt = FALSE`. \cr
#' `NA`s are by default sorted last.
#' @param X A list or data frame.
#' @param sort Should the expansion be sorted? By default it is `FALSE`.
#' @param unique Should unique values across each column or list element
#' be taken? By default this is `TRUE`.
#' @param as_dt Should result be a `data.table`?
#' By default this is `TRUE`. If `FALSE` a list is returned.
#' @param strings_as_factors Should strings be converted to factors before
#' expansion? The default is `FALSE` but setting to `TRUE` can offer
#' a significant speed improvement.
#' @param log_limit The maximum log10 limit for expanded number of rows.
#' Anything >= this results in an error.
#' @return A data.table or list object.
#' @examples
#' library(timeplyr)
#' crossed_join(list(1:10, 11:20))
#' crossed_join(iris, sort = TRUE)
#' @export
crossed_join <- function(X, sort = FALSE, unique = TRUE,
                         as_dt = TRUE,
                         strings_as_factors = FALSE,
                         log_limit = 8){
  x_nms <- names(X)
  if (unique){
    X <- lapply(X, function(x) collapse::funique(x, sort = sort))
  } else {
    X <- as.list(X)
  }
  expanded_n <- prod(collapse::vlengths(X, use.names = FALSE))
  expand_check(expanded_n, log_limit)
  if (strings_as_factors){
    is_chr <- vapply(X, FUN = is.character,
                     FUN.VALUE = logical(1),
                     USE.NAMES = FALSE)
    if (any(is_chr)){
      which_chr <- which(is_chr)
      X[which_chr] <- lapply(X[which_chr],
                             function(x) collapse::qF(x,
                                                      sort = FALSE,
                                                      ordered = FALSE,
                                                      na.exclude = FALSE))
    }
  }
  # out <- .Call(Ccj, X)
  # do.call(CJ, args = c(X, list(sorted = FALSE, unique = FALSE)))
  out <- CJ2(X)
  if (!is.null(x_nms)){
    out <- setnames(out, x_nms)
  }
  as_dt2 <- as_dt || (sort && !unique)
  if (as_dt2){
    data.table::setDT(out)
    data.table::setalloccol(out)
    if (sort){
      if (!unique){
        data.table::setorder(out, na.last = TRUE)
      }
      data.table::setattr(out, "sorted", names(out))
    }
  }
  if (!as_dt2){
    out <- as.list(out)
  }
  out
}
