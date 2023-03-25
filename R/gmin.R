#' Grouped minimum/maximum.
#'
#' @description This is like `min()` and `max()` but always returns
#' a vector the same length as x.
#' @param x An atomic vector.
#' @param g An integer vector of group IDs.
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' iris %>%
#'   add_group_id(Species, .name = "g") %>%
#'   mutate(min = gmin(Sepal.Length, g = g),
#'          max = gmax(Sepal.Length, g = g)) %>%
#'   # The below is equivalent to above
#'   mutate(min2 = min(Sepal.Length),
#'          max2 = max(Sepal.Length), .by = Species) %>%
#'   distinct(Species, min, max, min2, max2)
#' @rdname gmin
#' @export
gmin <- function(x, g = NULL, na.rm = TRUE){
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
#' @rdname gmin
#' @export
gmax <- function(x, g = NULL, na.rm = TRUE){
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
