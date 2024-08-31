#' These functions have been superseded by fastplyr functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `r lifecycle::badge("superseded")`
#'
#' These functions can now be found in fastplyr. \cr
#' They are no longer recommended in this package and thus have been both
#' deprecated and superseded.
#'
#' Use `do.call(fastplyr::crossing, data)` to replicate `crossed_join(data)`.
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
#'
#'
#' @returns
#' A data.table or list object.
#' @export
crossed_join <- function(X, sort = FALSE, unique = TRUE,
                         as_dt = TRUE,
                         strings_as_factors = FALSE){
  x_nms <- names(X)
  if (unique){
    X <- lapply(X, function(x) collapse::funique(x, sort = sort))
  } else {
    X <- as.list(X)
  }
  expanded_n <- prod(cheapr::lengths_(X))
  if (strings_as_factors){
    which_chr <- which(vapply(X, is.character, FALSE, USE.NAMES = FALSE))
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
