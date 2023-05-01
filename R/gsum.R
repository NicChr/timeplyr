#' Grouped statistical functions.
#'
#' @description These functions are wrappers around the collapse equivalents
#' but always return a vector the same length and same order as x.\cr
#' They all accept group IDs for grouped calculations.
#' @param x An atomic vector.
#' @param g Group IDs passed directly to `collapse::GRP()`.
#' This can be a vector, list or data frame.
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
#' @param ... Additional parameters passed on to the collapse package
#' equivalents, `fsum()`, `fmean()`, `fmin()`, `fmax()`,
#' `fsd()`, `fvar()`, `fmode()`, `fmedian()`, `ffirst()`, `flast()` and
#' `fnobs()`
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(ggplot2)
#' # Dplyr
#' iris %>%
#'   mutate(mean = mean(Sepal.Length), .by = Species)
#' # Timeplyr
#' iris %>%
#'   mutate(mean = gmean(Sepal.Length, g = Species))
#'
#' # One can utilise pick() to specify multiple groups
#' mpg %>%
#'   mutate(mean = gmean(displ, g = pick(manufacturer, model)))
#'
#' # Alternatively you can create a unique ID for each group
#' mpg %>%
#'   add_group_id(manufacturer, model) %>%
#'   mutate(mean = gmean(displ, g = group_id))
#'
#' # Another example
#'
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
  collapse::fsum(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}
#' @rdname gsum
#' @export
gmean <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fmean(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}
#' @rdname gsum
#' @export
gmin <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fmin(x, g = g, use.g.names = FALSE,
                  na.rm = na.rm, TRA = "replace_fill", ...)
}
#' @rdname gsum
#' @export
gmax <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fmax(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}
#' @rdname gsum
#' @export
gsd <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fsd(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}
#' @rdname gsum
#' @export
gvar <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fvar(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}
#' @rdname gsum
#' @export
gmode <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fmode(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}
#' @rdname gsum
#' @export
gmedian <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fmedian(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}
#' @rdname gsum
#' @export
gfirst <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::ffirst(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}
#' @rdname gsum
#' @export
glast <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::flast(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}
#' @rdname gsum
#' @export
gnobs <- function(x, g = NULL, ...){
  collapse::fnobs(x, g = g, use.g.names = FALSE,
                  TRA = "replace_fill", ...)
}
# Version 3
# gsum <- function(x, g = NULL, ...){
#   if (!is.null(g)){
#     g <- GRP2(g, sort = TRUE, na.last = TRUE,
#               return.groups = FALSE)
#     gorder <- g[["order"]]
#     if (is.null(gorder)){
#       gorder <- radix_order(g[["group.id"]])
#     }
#   }
#   out <- collapse::fsum(x,
#                          g = g,
#                          use.g.names = FALSE,
#                          ...)
#   if (length(g) == 0L){
#     rep_len(out, length(x))
#   } else {
#     out <- rep(out, times = g[["group.sizes"]])
#     out[radix_order(gorder)]
#   }
# }
# version 2
# gsum <- function(x, g = NULL, na.rm = TRUE, ...){
#   if (!is.null(g)){
#     if (!is.numeric(g)){
#       g <- collapse::group(g)
#     }
#     g <- GRP2(as.integer(g), sort = TRUE,
#                        return.order = TRUE, na.last = TRUE)
#   }
#   out <- collapse::fsum(x,
#                         g = g,
#                         use.g.names = FALSE,
#                         na.rm = na.rm,
#                         ...)
#   if (length(g) == 0L){
#     rep_len(out, length(x))
#   } else {
#     out <- rep(out, g[["group.sizes"]])
#     out[radix_order(g[["order"]])]
#   }
# }
# Version 1
# gsum <- function(x, g = NULL, na.rm = TRUE, ...){
#   if (!is.null(g)){
#     g <- collapse::qG(as.integer(g), sort = FALSE)
#   }
#   out <- collapse::fsum(x,
#                         g = g,
#                         use.g.names = FALSE,
#                         na.rm = na.rm,
#                         ...)
#   if (length(g) == 0L){
#     rep_len(out, length(x))
#   } else {
#     out[match(g, seq_len(length(out)))]
#   }
# }
# if (!is.null(g)){
#   if (collapse::is_GRP(g)){
#     g <- GRP2(g[["group.id"]], sort = TRUE,
#                        return.order = TRUE, na.last = TRUE)
#   } else {
#     g <- GRP2(as.integer(g), sort = TRUE,
#                        return.order = TRUE, na.last = TRUE)
#   }
# }
