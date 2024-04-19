gunique <- function(x, g = NULL, sort = FALSE, order = TRUE,
                    use.g.names = TRUE){
  if (is.null(g)){
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
  cheapr::sset(x, GRP_starts(g))
}
gduplicated <- function(x, g = NULL, order = TRUE, all = FALSE){
  # out_nms <- names(x)
  if (is.null(g)){
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
gwhich_duplicated <- function(x, g = NULL, order = TRUE, all = FALSE){
  if (is.null(g)){
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
gsort <- function(x, g = NULL, order = TRUE, use.g.names = TRUE){
  if (is.null(g)){
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
    cheapr::sset(x, order)
  }
}
gorder <- function(x, g = NULL, order = TRUE){
  if (is.null(g)){
    order <- radixorderv2(x)
  } else {
    # g <- GRP2(list(group_id(g, .cols = names(g), order = order),
    #                group_id(x, order = TRUE, .cols = names(x))),
    #           sort = TRUE,
    #           return.groups = FALSE, call = FALSE,
    #           return.order = TRUE)
    # order <- GRP_order(g)
    order <- radixorderv2(
      list(group_id(g, order = order, .cols = names(g)),
           group_id(x, order = TRUE, .cols = names(x)))
    )
  }
  order
}
# Is data sorted within each group?
# Data need not be sorted over the entire data.
gis_sorted <- function(x, g = NULL, order = TRUE){
  isTRUE(attr(gorder(x, g = g, order = order), "sorted"))
}
