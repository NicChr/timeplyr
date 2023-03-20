#' Rolling sum/mean
#'
#' @description Calculate rolling sums and means using data.table
#' for speed and efficiency. This is a wrapper around data.table's rolling functions
#' that by default calculates simple rolling averages/sums.
#' The adaptive argument has been replaced with the simpler
#' (and less functional) partial argument.
#'
#'
#' @param x Numeric vector.
#' @param n Rolling window size, default is `length(x)`.
#' @param partial Should calculations be done using partial windows?
#' Default is \code{TRUE}.
#' @param weights Importance weights. These can either be
#' length 1 or the same length as x.
#' Currently, no normalisation of weights occurs.
#' @param fill Numeric value to pad by for values in incomplete window.
#' @param algo Character, default "fast". When set to "exact", then slower algorithm is used.
#' See `?data.table::frollmean` details for more information.
#' @param na.rm Should missing values be removed when calculating window? Defaults to \code{FALSE}.
#' @param hasNA If it is known that x contains \code{NA} then setting to \code{TRUE} will speed up.
#' @examples
#' library(timeplyr)
#'
#' x <- 1:10
#' frollsum2(x) # Simple rolling total
#' frollmean2(x) # Simple moving average
#' frollsum2(x, n = 3)
#' frollmean2(x, n = 3)
#' frollsum2(x, n = 3, partial = FALSE)
#' frollmean2(x, n = 3, partial = FALSE)
#'
#' # Plot of expected value of 'coin toss' over many flips
#' set.seed(42)
#' x <- sample(c(1, 0), 10^3, replace = TRUE)
#' ev <- frollmean2(x)
#' plot(1:length(x), ev)
#' abline(h = 0.5, lty = 2)
#' @rdname froll
#' @export
frollsum2 <- function(x, n = length(x), partial = TRUE,
                      weights = NULL,
                      fill = NA, algo = c("fast", "exact"),
                      na.rm = FALSE, hasNA = NA){
  # if (!inherits(x, c("numeric", "integer", "logical", "NULL"))) stop("x must be of numeric, integer or logical class")
  if (!inherits(n, c("numeric", "integer"))) stop("n must be of numeric or integer class")
  if (!isTRUE(is_length_one(n) && n >= 1)) stop("n must be a numeric vector >= 1 of length 1")
  if (!isTRUE(is_length_one(partial) && is.logical(partial))) stop("partial must be a logical vector of length 1")
  if (!is.null(weights)){
    if (!length(weights) %in% c(1L, length(x))){
      stop("weights must be of length 1 or length(x)")
    }
    x <- x * weights
  }
  if (partial){
    partial_window <- window_seq(k = n, n = length(x), partial = TRUE)
    data.table::frollsum(x = x, n = partial_window, fill = fill,
                         algo = algo, align = "right", na.rm = na.rm,
                         hasNA = hasNA, adaptive = TRUE)
  } else {
    data.table::frollsum(x = x, n = n, fill = fill,
                         algo = algo, align = "right", na.rm = na.rm,
                         hasNA = hasNA, adaptive = FALSE)
  }
}
#' @rdname froll
#' @export
frollmean2 <- function(x, n = length(x), partial = TRUE,
                       weights = NULL,
                       fill = NA, algo = c("fast", "exact"),
                       na.rm = FALSE, hasNA = NA){
  if (!inherits(n, c("numeric", "integer"))) stop("n must be of numeric or integer class")
  if (!isTRUE(is_length_one(n) && n >= 1)) stop("n must be a numeric vector >= 1 of length 1")
  if (!isTRUE(is_length_one(partial) && is.logical(partial))) stop("partial must be a logical vector of length 1")

  if (is.null(weights)){
    if (partial){
      window_seq <- window_seq(k = n, n = length(x), partial = TRUE)
      out <- data.table::frollmean(x, n = window_seq, adaptive = TRUE,
                                   fill = fill, algo = algo,
                                   na.rm = na.rm, hasNA = hasNA)
    } else {
      out <- data.table::frollmean(x, n = n, adaptive = FALSE,
                       fill = fill, algo = algo,
                       na.rm = na.rm, hasNA = hasNA)
    }
  } else {
    xna <- collapse::whichNA(x, invert = FALSE)
    if (!length(weights) %in% c(1L, length(x))){
      stop("weights must be of length 1 or length(x)")
    }
    x <- x * weights
    if (length(weights) == 1L) weights <- rep_len(weights, length(x))
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
# No partial argument, just a weights extension to data.table::frollsum()
# Internal function
frollsum3 <- function(x, n, weights = NULL, ...){
  if (!is.null(weights)){
    x <- x * weights
  }
  data.table::frollsum(x, n = n, ...)
}
# Also internal
frollmean3 <- function(x, n, weights = NULL, ...){
  if (!is.null(weights)){
    x <- x * weights
    if (length(weights) == 1L) weights <- rep_len(weights, length(x))
    weights[is.na(x)] <- NA_real_
    out <- data.table::frollsum(x, n = n, ...)/data.table::frollsum(weights, n = n, ...)
  } else {
    out <- data.table::frollmean(x, n = n, ...)
  }
  out
}
