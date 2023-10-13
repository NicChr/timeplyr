#' Unweighted & weighted arithmetic, geometric and harmonic mean
#'
#' @description
#' Convenience functions for fast unweighted and weighted mean calculations.
#'
#' @param x [numeric] Vector.
#' @param weights [numeric] Vector of weights. \cr
#' Default is `NULL` which performs an unweighted mean.
#' @param na.rm [logical] Value (Default is `TRUE`).
#' @param ... Further arguments passed to `collapse::fmean`.
#'
#' @returns
#' `numeric(min(length(x), 1))`.
#'
#' @rdname weighted_mean
#' @export
arithmetic_mean <- function(x, weights = NULL, na.rm = TRUE, ...){
  collapse::fmean(x, w = weights, na.rm = na.rm, ...)
}
#' @rdname weighted_mean
#' @export
geometric_mean <- function(x, weights = NULL, na.rm = TRUE, ...){
  exp(arithmetic_mean(log(x), weights = weights, na.rm = na.rm, ...))
}
#' @rdname weighted_mean
#' @export
harmonic_mean <- function(x, weights = NULL, na.rm = TRUE, ...){
  1 / arithmetic_mean(1/x, weights = weights, na.rm = na.rm, ...)
}
