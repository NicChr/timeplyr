#' Fast grouped numbers of missing values
#'
#' @description A wrapper around `fsum(is.na(x))` to calculate
#' numbers of missing values by group.
#' @param x Vector or data frame.
#' @param g Object to be used for grouping,
#' passed directly to `collapse::GRP()`.
fnmiss <- function(x, g = NULL, ...){
  collapse::fsum(is.na(x), g = g, ...)
}
