gduplicated <- function(x, g = NULL, order = TRUE, all = FALSE){
  # out_nms <- names(x)
  if (is.null(g)){
    g <- GRP2(x, sort = order,
              return.groups = FALSE,
              return.order = TRUE)
  } else {
    g <- GRP2(
      new_df(
        g1 = group_id(g, order = order, .cols = names(g)),
        g2 = group_id(x, order = order, .cols = names(x))
      ),
      sort = order,
      return.groups = FALSE,
      return.order = TRUE
    )
  }
  GRP_duplicated(g, all = all)
}
gorder <- function(x, g = NULL, order = TRUE){
  if (is.null(g)){
    order <- radixorderv2(x)
  } else {
    order <- radixorderv2(
      new_df(
        g1 = group_id(g, order = order, .cols = names(g)),
        g2 = group_id(x, order = TRUE, .cols = names(x))
      )
    )
  }
  order
}
# Is data sorted within each group?
# Data need not be sorted over the entire data.
gis_sorted <- function(x, g = NULL, order = TRUE){
  isTRUE(attr(gorder(x, g = g, order = order), "sorted"))
}
