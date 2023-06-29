#' Grouped `unique()`, `sort()` and `duplicated()`
#'
#' @description These functions use `collapse` and are like the
#' `collapse` counterpart but differ in that they accept a group `g` argument
#' which allows for more flexible by-group sorting.
#'
#' @param x A vector or data frame.
#' @param g Object used for grouping, passed directly to `collapse::GRP()`.\cr
#' This can for example be a vector or data frame.
#' @param sort Should the result be sorted? \cr
#' This only applies to `gunique()`.
#' @param order Should the groups be treated as ordered groups?
#' Default is `TRUE`.
#' @param use.g.names Should group names be used? Default is `TRUE`.
#' @param all If `TRUE`, `gduplicated()` returns all duplicated values,
#' including the first occurrence.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(tibble)
#' set.seed(81234)
#' iris <- slice_sample(iris, n = nrow(iris)) %>%
#'   as_tibble()
#'
#' # gunique vs dplyr
#' # Unsorted unique values by group
#' gunique(iris$Sepal.Width, g = iris$Species, sort = FALSE)
#' iris %>%
#'   distinct(Species, Sepal.Width) %>%
#'   pull(Sepal.Width)
#' # Sorted unique values by ordered groups
#' gunique(iris$Sepal.Width, g = iris$Species, sort = TRUE)
#' iris %>%
#'   distinct(Species, Sepal.Width) %>%
#'   arrange(Species, Sepal.Width) %>%
#'   pull(Sepal.Width)
#' # Sorted unique values by unordered groups
#' gunique(iris$Sepal.Width, g = iris$Species,
#'         sort = TRUE, order = FALSE)
#' iris %>%
#'   reframe(Sepal.Width = sort(unique(Sepal.Width)), .by = Species) %>%
#'   pull(Sepal.Width)
#' @rdname gunique
#' @export
gunique <- function(x, g = NULL, sort = FALSE, order = TRUE,
                    use.g.names = TRUE){
  if (length(g) == 0){
    g <- GRP2(x,
              sort = sort,
              return.groups = TRUE, call = FALSE,
              return.order = TRUE)
  } else {
    g <- GRP2(g, return.order = FALSE,
              sort = order, return.groups = TRUE)
    if (use.g.names){
      if (is.list(x)){
        x[["name"]] <- GRP_names(g, expand = TRUE)
      } else {
        names(x) <- GRP_names(g, expand = TRUE)
      }
    }
    g <- GRP2(list(GRP_group_id(g),
                   group_id.default(x, order = sort)),
              sort = sort,
              return.groups = TRUE, call = FALSE,
              return.order = TRUE)
  }
  vec_slice2(x, GRP_starts(g))
}
#' @rdname gunique
#' @export
gduplicated <- function(x, g = NULL,
                        order = TRUE,
                        use.g.names = TRUE,
                        all = FALSE){
  out_nms <- names(x)
  if (length(g) == 0){
    g <- GRP2(x,
              sort = order,
              return.groups = FALSE, call = FALSE,
              return.order = FALSE)
  } else {
    g <- GRP2(g, return.order = FALSE,
              sort = order, return.groups = TRUE)
    if (use.g.names){
      out_nms <- GRP_names(g, expand = TRUE)
    }
    g <- GRP2(list(GRP_group_id(g),
                   group_id.default(x, order = order)),
              sort = order,
              return.groups = TRUE, call = FALSE,
              return.order = TRUE)
  }
  out <- collapse::fduplicated(GRP_group_id(g), all = all)
  names(out) <- out_nms
  out
}
#' @rdname gunique
#' @export
gsort <- function(x, g = NULL, order = TRUE, use.g.names = TRUE){
  if (length(g) == 0){
    order <- radixorderv2(x)
    sorted <- isTRUE(attr(order, "sorted"))
  } else {
    g <- GRP2(g, return.order = FALSE,
              sort = order, return.groups = TRUE)
    if (use.g.names){
      if (is.list(x)){
        x[["name"]] <- GRP_names(g, expand = TRUE)
      } else {
        names(x) <- GRP_names(g, expand = TRUE)
      }
    }
    g <- GRP2(list(GRP_group_id(g),
                   group_id.default(x, order = TRUE)),
              sort = TRUE,
              return.groups = TRUE, call = FALSE,
              return.order = TRUE)
    order <- GRP_order(g)
    sorted <- GRP_is_sorted(g)
  }
  if (sorted){
    x
  } else {
    vec_slice2(x, order)
  }
}
#' @rdname gunique
#' @export
gorder <- function(x, g = NULL, order = TRUE, use.g.names = TRUE){
  if (length(g) == 0){
    order <- radixorderv2(x)
  } else {
    g <- GRP2(g, return.order = FALSE,
              sort = order, return.groups = TRUE)
    g <- GRP2(list(GRP_group_id(g),
                   group_id.default(x, order = TRUE)),
              sort = TRUE,
              return.groups = TRUE, call = FALSE,
              return.order = TRUE)
    order <- GRP_order(g)
  }
  order
}
