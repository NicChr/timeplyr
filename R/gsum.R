#' Grouped statistical functions.
#'
#' @description These functions are wrappers around the collapse equivalents
#' but always return a vector the same length as x. \cr
#' They all accept group IDs for grouped calculations.
#' @param x An atomic vector.
#' @param g An integer vector of group IDs.
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
#' @param ... Additional parameters passed on to the collapse package
#' equivalents, `fsum()`, `fmean()`, `fmin()`, and `fmax()`.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' iris %>%
#'   add_group_id(Species, .name = "g") %>%
#'   mutate(min = gmin(Sepal.Length, g = g),
#'          max = gmax(Sepal.Length, g = g),
#'          sum = gsum(Sepal.Length, g = g),
#'          mean = gmean(Sepal.Length, g = g)) %>%
#'   # The below is equivalent to above
#'   mutate(min2 = min(Sepal.Length),
#'          max2 = max(Sepal.Length),
#'          sum2 = sum(Sepal.Length),
#'          mean2 = mean(Sepal.Length),
#'          .by = Species) %>%
#'   distinct(Species,
#'            min, min2,
#'            max, max2,
#'            sum, sum2,
#'            mean, mean2)
#' @rdname gsum
#' @export
gsum <- function(x, g = NULL, na.rm = TRUE, ...){
  if (!is.null(g)) g <- as.integer(g)
  out <- collapse::fsum(x,
                        g = g,
                        use.g.names = FALSE,
                        na.rm = na.rm,
                        ...)
  if (length(g) == 0L){
    rep_len(out, length(x))
  } else {
    out[match(g, seq_len(length(out)))]
  }
}
#' @rdname gsum
#' @export
gmean <- function(x, g = NULL, na.rm = TRUE,
                  ...){
  if (!is.null(g)) g <- as.integer(g)
  out <- collapse::fmean(x,
                         g = g,
                         use.g.names = FALSE,
                         na.rm = na.rm)
  if (length(g) == 0L){
    rep_len(out, length(x))
  } else {
    out[match(g, seq_len(length(out)))]
  }
}
#' @rdname gsum
#' @export
gmin <- function(x, g = NULL, na.rm = TRUE,
                 ...){
  if (!is.null(g)) g <- as.integer(g)
  out <- collapse::fmin(x,
                        g = g,
                        use.g.names = FALSE,
                        na.rm = na.rm)
  if (length(g) == 0L){
    rep_len(out, length(x))
  } else {
    out[match(g, seq_len(length(out)))]
  }
}
#' @rdname gsum
#' @export
gmax <- function(x, g = NULL, na.rm = TRUE,
                 ...){
  if (!is.null(g)) g <- as.integer(g)
  out <- collapse::fmax(x,
                        g = g,
                        use.g.names = FALSE,
                        na.rm = na.rm)
  if (length(g) == 0L){
    rep_len(out, length(x))
  } else {
    out[match(g, seq_len(length(out)))]
  }
}
