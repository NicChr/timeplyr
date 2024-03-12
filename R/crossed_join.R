#' A `do.call()` and `data.table::CJ()` method
#'
#' @description This function operates like `do.call(CJ, ...)` and accepts
#' a list or data.frame as an argument. \cr
#' It has less overhead for small joins, especially when `unique = FALSE` and
#' `as_dt = FALSE`. \cr
#' `NA`s are by default sorted last.
#'
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
#' @details An important note is that currently `NA`s
#' are sorted last and therefore a key is not set.
#'
#' @returns
#' A data.table or list object.
#'
#' @examples
#' library(timeplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' crossed_join(list(1:3, -2:2))
#' crossed_join(iris)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
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
  expanded_n <- prod(cheapr::lengths_(X))
  expand_check(expanded_n, log_limit)
  if (strings_as_factors){
    which_chr <- which_(vapply(X, is.character, FALSE, USE.NAMES = FALSE))
    X[which_chr] <- lapply(X[which_chr],
                           function(x) cheapr::factor_(x, order = FALSE))
  }
  # out <- .Call(Ccj, X)
  # do.call(CJ, args = c(X, list(sorted = FALSE, unique = FALSE)))
  out <- CJ2(X)
  if (!is.null(x_nms)){
    names(out) <- x_nms
  }
  as_dt2 <- as_dt || (sort && !unique)
  if (as_dt2){
    out <- collapse::qDT(out)
    # data.table::setalloccol(out)
    if (sort){
      if (!unique){
        data.table::setorder(out, na.last = TRUE)
      }
    }
  }
  if (!as_dt && (sort && !unique)){
    out <- as.list(out)
  }
  out
}
