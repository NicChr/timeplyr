arithmetic_mean <- function(x, weights = NULL, na.rm = TRUE, ...){
  collapse::fmean(x, w = weights, na.rm = na.rm, ...)
}
geometric_mean <- function(x, weights = NULL, na.rm = TRUE, ...){
  exp(arithmetic_mean(log(x), weights = weights, na.rm = na.rm, ...))
}
harmonic_mean <- function(x, weights = NULL, na.rm = TRUE, ...){
  1 / arithmetic_mean(1/x, weights = weights, na.rm = na.rm, ...)
}
