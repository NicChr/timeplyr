#' Fast by-group rolling sum/mean
#' @description An efficient method for rolling sum/mean for many groups.
#'
#' @param x Numeric vector.
#' @param window Rolling window size, default is `length(x)`.
#' @param partial Should calculations be done using partial windows?
#' Default is \code{TRUE}.
#' @param weights Importance weights. Must be the same length as x.
#' Currently, no normalisation of weights occurs.
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame.
#' @param na.rm Should missing values be removed for the calculation?
#' The default is `TRUE`.
#' @param ... Additional arguments passed to `data.table::frollmean` and
#' `data.table::frollsum`.
#' @examples
#' \dontrun{
#' library(timeplyr)
#' library(data.table)
#' library(bench)
#' df <- data.table(g = sample.int(10^5, 10^6, TRUE),
#'                  x = rnorm(10^6))
#' mark(e1 = df[, mean := frollmean(x, n = 7, align = "right", na.rm = FALSE), by = "g"]$mean,
#'      e2 = df[, mean := roll_mean(x, n = 7, g = get("g"), partial = FALSE, na.rm = FALSE)]$mean)
#' }
#' @rdname roll_sum
#' @export
roll_sum <- function(x, window = length(x),
                     g = NULL, partial = TRUE,
                     weights = NULL, na.rm = TRUE, ...){
  if (length(window) != 1L){
    stop("window must be of length 1")
  }
  has_groups <- !is.null(g)
  if (!has_groups){
    return(frollsum2(x, n = window,
                     partial = partial,
                     na.rm = na.rm, weights = weights, ...))
  }
  g <- GRP2(g)
  check_data_GRP_size(x, g)
  group_sizes <- GRP_group_sizes(g)
  n_groups <- GRP_n_groups(g)
  groups_are_sorted <- !has_groups || GRP_is_sorted(g)
  if (!groups_are_sorted){
    x <- x[GRP_order(g)]
  }
  roll_window <- window_sequence(group_sizes,
                                 k = rep.int(window, n_groups),
                                 partial = partial)

  out <- frollsum3(x, n = roll_window,
                   weights = weights,
                   adaptive = TRUE, align = "right",
                   na.rm = na.rm, ...)
  if (!groups_are_sorted){
    out <- collapse::greorder(out, g = g)
  }
  out
}
#' @rdname roll_sum
#' @export
roll_mean <- function(x, window = length(x), g = NULL, partial = TRUE,
                      weights = NULL, na.rm = TRUE, ...){
  if (length(window) != 1L){
    stop("window must be of length 1")
  }
  has_groups <- !is.null(g)
  if (!has_groups){
    return(frollmean2(x, n = window,
                      partial = partial,
                      na.rm = na.rm, weights = weights, ...))
  }
  g <- GRP2(g)
  check_data_GRP_size(x, g)
  group_sizes <- GRP_group_sizes(g)
  n_groups <- GRP_n_groups(g)
  groups_are_sorted <- !has_groups || GRP_is_sorted(g)
  if (!groups_are_sorted){
    x <- x[GRP_order(g)]
  }
  roll_window <- window_sequence(group_sizes,
                                 k = rep.int(window, n_groups),
                                 partial = partial)

  out <- frollmean3(x, n = roll_window,
                    weights = weights,
                    adaptive = TRUE, align = "right",
                    na.rm = na.rm, ...)
  if (!groups_are_sorted){
    out <- collapse::greorder(out, g = g)
  }
  out

}
