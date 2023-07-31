#' Rolling sum/mean - \bold{Deprecated}
#'
#' @description This function is deprecated, use `roll_mean` and `roll_sum` instead.
#'
#' @param x Numeric vector.
#' @param n Rolling window size, default is `length(x)`.
#' @param partial Should calculations be done using partial windows?
#' Default is \code{TRUE}.
#' @param weights Importance weights. Must be the same length as x.
#' Currently, no normalisation of weights occurs.
#' @param fill Numeric value to pad by for values in incomplete window.
#' @param algo Character, default "fast". When set to "exact", then slower algorithm is used.
#' See `?data.table::frollmean` details for more information.
#' @param na.rm Should missing values be removed when calculating window? Defaults to \code{FALSE}.
#' @param hasNA If it is known that x contains \code{NA} then setting to \code{TRUE} will speed up.
#' @rdname froll
#' @export
frollsum2 <- function(x, n = length(x), partial = TRUE,
                      weights = NULL,
                      fill = NA, algo = c("fast", "exact"),
                      na.rm = FALSE, hasNA = NA){
  .Deprecated("roll_sum")
  stopifnot(length(n) == 1L)
  if (!is.null(weights)){
    stopifnot(length(weights) == length(x))
    x <- x * weights
  }
  if (partial){
    n <- window_seq(k = n, n = length(x), partial = TRUE)
    adaptive <- TRUE
  } else {
    n <- n
    adaptive <- FALSE
  }
  data.table::frollsum(x = x, n = n, fill = fill,
                       algo = algo, align = "right",
                       na.rm = na.rm,
                       hasNA = hasNA, adaptive = adaptive)
}
#' @rdname froll
#' @export
frollmean2 <- function(x, n = length(x), partial = TRUE,
                       weights = NULL,
                       fill = NA, algo = c("fast", "exact"),
                       na.rm = FALSE, hasNA = NA){
  .Deprecated("roll_mean")
  stopifnot(length(n) == 1L)
  if (is.null(weights)){
    if (partial){
      n <- window_seq(k = n, n = length(x), partial = TRUE)
      adaptive <- TRUE
    } else {
      n <- n
      adaptive <- FALSE
    }
    out <- data.table::frollmean(x, n = n, adaptive = adaptive,
                                 fill = fill, algo = algo,
                                 na.rm = na.rm, hasNA = hasNA)
  } else {
    stopifnot(length(weights) == length(x))
    xna <- which(is.na(x))
    x <- x * weights
    weights[xna] <- NA_real_

    numerator <- frollsum2(x, n = n, partial = partial,
                     weights = NULL,
                     fill = fill, algo = algo,
                     na.rm = na.rm, hasNA = hasNA)
    denominator <- frollsum2(weights, n = n, partial = partial,
                             weights = NULL,
                             fill = fill, algo = algo,
                             na.rm = na.rm, hasNA = hasNA)
    out <- numerator/denominator
  }
  out
}
