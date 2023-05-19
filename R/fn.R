#' Alternative to `dplyr::n()`
#'
#' @details `fn()` and `gn()` efficiently calculate numbers of rows
#' per group. They utilise `collapse::GRPN()` but the behaviour
#' aligns a bit more closely with the `collapse` statistical functions. \cr
#' `fn()` always returns the unique number of rows by group. \cr
#' `gn()` is the same as `fn()` but the output is expanded to
#' match the size and order of your data.
#' @param ... Template with which to calculate group sizes.
#' This can be left unused as long as g is not NULL.
#' If a data frame or matrix is supplied,
#' the number of rows by group `g` is calculated.
#' If a vector is supplied, the group-wise lengths are calculated.
#' @param g Object to be used as groups, passed directly to `collapse::GRP()`.
#' @param use.g.names If `TRUE` group names are added to the result as names.
#' This only applies to `fn()`.
#' @return An integer vector of row sizes.
#' @examples
#' library(timeplyr)
#' library(ggplot2)
#' fn(mpg)
#' fn(mpg, g = mpg$manufacturer)
#' gn(mpg)
#' gn(mpg, g = mpg$manufacturer)
#'
#' library(data.table)
#' dt <- data.table(g = sample.int(10^5))
#' dt[, .N, by = g] # Usual DT method
#' dt[, list(g, N = fn(g = g))] # Using fn()
#' @rdname fn
#' @export
fn <- function(..., g = NULL, use.g.names = FALSE){
  n_dots <- dots_length(...)
  if (n_dots > 1){
    stop("Please supply <= 1 arguments to ...")
  }
  if (is.null(g)){
    if (n_dots == 0){
      stop("when g = NULL, ... must be supplied")
    }
    x <- list(...)[[1L]]
    nobs <- vec_length(x)
  } else {
    g <- GRP2(g, sort = TRUE, return.groups = use.g.names)
    nobs <- collapse::GRPN(g, expand = FALSE)
    if (use.g.names){
      names(nobs) <- collapse::GRPnames(g)
    }
    if (n_dots == 1){
      x <- list(...)[[1L]]
      N <- vec_length(x)
      if (N != length(g[["group.id"]])){
        stop("g must be the same size as the data")
      }
    }
  }
  nobs
}
#' @rdname fn
#' @export
gn <- function(..., g = NULL){
  n_dots <- dots_length(...)
  if (n_dots > 1){
    stop("Please supply <= 1 arguments to ...")
  }
  if (is.null(g)){
    if (n_dots == 0){
      stop("when g = NULL, ... must be supplied")
    }
    x <- list(...)[[1L]]
    N <- vec_length(x)
    nobs <- alloc(N, N)
  } else {
    g <- GRP2(g, sort = TRUE, return.groups = FALSE)
    nobs <- collapse::GRPN(g, expand = TRUE)
    if (n_dots == 1){
      x <- list(...)[[1L]]
      N <- vec_length(x)
      if (N != length(nobs)){
        stop("g must be the same size as the data")
      }
    }
  }
  nobs
}
