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
#'
#' # Very fast way of finding duplicate rows
#' df <- data.frame(group = sample.int(10^6, 10^4, TRUE))
#'
#' which(gduplicated(df))
#' # More efficient
#' gwhich_duplicated(df)
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
    if (use.g.names){
      g <- GRP2(g, return.order = FALSE,
                sort = order, return.groups = TRUE)
      if (is.list(x)){
        x[["name"]] <- GRP_names(g, expand = TRUE)
      } else {
        names(x) <- GRP_names(g, expand = TRUE)
      }
      group_id <- GRP_group_id(g)
    } else {
      group_id <- group_id(g, .cols = names(g), order = order)
    }
    g <- GRP2(list(group_id,
                   group_id(x, order = sort, .cols = names(x))),
              sort = sort,
              return.groups = TRUE, call = FALSE,
              return.order = TRUE)
  }
  vec_slice2(x, GRP_starts(g))
}
#' @rdname gunique
#' @export
gduplicated <- function(x, g = NULL, order = TRUE, all = FALSE){
  # out_nms <- names(x)
  if (length(g) == 0){
    g <- GRP2(x, sort = order,
              return.groups = FALSE,
              return.order = TRUE)
  } else {
    g <- GRP2(list(group_id(g, order = order, .cols = names(g)),
                   group_id(x, order = order, .cols = names(x))),
              sort = order,
              return.groups = FALSE,
              return.order = TRUE)
  }
  GRP_duplicated(g, all = all)
}
#' @rdname gunique
#' @export
gwhich_duplicated <- function(x, g = NULL, order = TRUE, all = FALSE){
  if (length(g) == 0){
    g <- GRP2(x, sort = order,
              return.groups = FALSE)
  } else {
    g <- GRP2(list(group_id(g, order = order, .cols = names(g)),
                   group_id(x, order = order, .cols = names(x))),
              sort = order,
              return.groups = FALSE, call = FALSE,
              return.order = TRUE)
  }
  GRP_which_duplicated(g, all = all)
}
#' @rdname gunique
#' @export
gsort <- function(x, g = NULL, order = TRUE, use.g.names = TRUE){
  if (length(g) == 0){
    order <- radixorderv2(x)
    sorted <- isTRUE(attr(order, "sorted"))
  } else {
    if (use.g.names){
      g <- GRP2(g, return.order = FALSE,
                sort = order, return.groups = TRUE)
      if (is.list(x)){
        x[["name"]] <- GRP_names(g, expand = TRUE)
      } else {
        names(x) <- GRP_names(g, expand = TRUE)
      }
      group_id <- GRP_group_id(g)
    } else {
      group_id <- group_id(g, order = order, .cols = names(g))
    }
    g <- GRP2(list(group_id,
                   group_id(x, .cols = names(x), order = TRUE)),
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
gorder <- function(x, g = NULL, order = TRUE){
  if (length(g) == 0){
    order <- radixorderv2(x)
  } else {
    g <- GRP2(list(group_id(g, .cols = names(g), order = order),
                   group_id(x, order = TRUE, .cols = names(x))),
              sort = TRUE,
              return.groups = FALSE, call = FALSE,
              return.order = TRUE)
    order <- GRP_order(g)
  }
  order
}
