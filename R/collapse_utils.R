
is_GRP <- get_from_package("is_GRP", "fastplyr")
GRP2 <- get_from_package("GRP2", "fastplyr")
GRP_n_groups <- get_from_package("GRP_n_groups", "fastplyr")
GRP_group_id <- get_from_package("GRP_group_id", "fastplyr")
GRP_data_size <- get_from_package("GRP_data_size", "fastplyr")
GRP_group_sizes <- get_from_package("GRP_group_sizes", "fastplyr")
GRP_starts <- get_from_package("GRP_starts", "fastplyr")
GRP_ends <- get_from_package("GRP_ends", "fastplyr")
GRP_order <- get_from_package("GRP_order", "fastplyr")
GRP_is_sorted <- get_from_package("GRP_is_sorted", "fastplyr")
GRP_names <- get_from_package("GRP_names", "fastplyr")
GRP_duplicated <- get_from_package("GRP_duplicated", "fastplyr")
GRP_group_data <- get_from_package("GRP_group_data", "fastplyr")

sorted_group_starts <- get_from_package("sorted_group_starts", "fastplyr")
sorted_group_ends <- get_from_package("sorted_group_ends", "fastplyr")
group2 <- get_from_package("group2", "fastplyr")
group_sizes <- get_from_package("group_sizes", "fastplyr")
gsplit2 <- get_from_package("gsplit2", "fastplyr")
radixorderv2 <- get_from_package("radixorderv2", "fastplyr")

df_to_GRP <- get_from_package("df_to_GRP", "fastplyr")

check_data_GRP_size <- function(x, GRP){
  if (!is.null(GRP)) {
    size <- NROW(x)
    GRP_size <- GRP_data_size(GRP)
    if (size != GRP_size) {
      stop("size of x must match size of groups")
    }
  }
}

# Use this to turn a sorted group ID into a GRP when you have basic group information
sorted_group_id_to_GRP <- function(x,
                                   n_groups,
                                   group_sizes,
                                   group.starts = TRUE,
                                   groups = NULL,
                                   group.vars = NULL){
  out <- structure(
    list(
      "N.groups" = n_groups,
      "group.id" = x,
      "group.sizes" = group_sizes,
      "groups" = groups,
      "group.vars" = group.vars,
      "ordered" = c("ordered" = TRUE, "sorted" = TRUE),
      "order" = seq_along(x),
      "group.starts" = NULL,
      "call" = NULL
    ),
    class = "GRP"
  )
  gstarts <- sorted_group_starts(group_sizes)
  if (group.starts){
    out[["group.starts"]] <- gstarts
  }
  attributes(out[["order"]]) <- list(starts = gstarts,
                                     maxgrpn = collapse::fmax(group_sizes),
                                     sorted = TRUE)
  # Alternative way of getting group starts is:
  # c(1L, which(x) != collapse::flag(x))
  out
}
gsplit2 <- function(x = NULL, g = NULL, use.g.names = FALSE, ...){
  if (is.null(g)){
    if (is.null(x)){
      list(integer())
    } else {
     list(x)
    }
  } else {
    collapse::gsplit(x, g = g, use.g.names = use.g.names, ...)
  }
}
# Sorts data by groups and returns key info
sort_data_by_GRP <- function(x, g, sorted_group_starts = TRUE){
  has_groups <- !is.null(g)
  if (!has_groups){
    return(list(
      x = x,
      n_groups = min(NROW(x), 1L),
      group_sizes = NROW(x),
      GRP = NULL,
      sorted_GRP = NULL,
      sorted = TRUE,
      group_order = NULL,
      has_groups = FALSE
    ))
  }
  g <- GRP2(g)
  check_data_GRP_size(x, g)
  group_id <- GRP_group_id(g)
  group_sizes <- GRP_group_sizes(g)
  n_groups <- GRP_n_groups(g)
  group_order <- GRP_order(g)
  groups_are_sorted <- isTRUE(attr(group_order, "sorted"))
  if (!groups_are_sorted){
    x <- sset(x, group_order)
    group_id <- sset(group_id, group_order)
  }
  if (sorted_group_starts){
    if (groups_are_sorted){
      sorted_group_starts <- GRP_starts(g)
    } else {
      sorted_group_starts <- sorted_group_starts(group_sizes)
    }
  } else {
    sorted_group_starts <- NULL
  }
  sorted_GRP <- sorted_group_id_to_GRP(
    group_id,
    n_groups = n_groups,
    group_sizes = group_sizes,
    group.starts = FALSE
  )
  sorted_GRP[["group.starts"]] <- sorted_group_starts
  list(x = x,
       n_groups = n_groups,
       group_sizes = group_sizes,
       GRP = g,
       sorted_GRP = sorted_GRP,
       sorted = groups_are_sorted,
       group_order = group_order,
       has_groups = has_groups)

}
# greorder but x can be a data frame or list
greorder2 <- function(x, g, ...){
  if (is.null(g)){
    return(x)
  }
  if (is.list(x)){
    sset(
      x,
      collapse::greorder(
        seq_len(NROW(x)), g = g, ...
      )
    )
  } else {
    collapse::greorder(x, g = g, ...)
  }
}

group_order_and_counts <- function(g = NULL){
  o <- radixorderv2(g, starts = FALSE, sort = FALSE, group.sizes = TRUE)
  if (is_GRP(g)) {
    sizes <- cheapr::val_rm(GRP_group_sizes(g), 0L)
  }
  else {
    sizes <- attr(o, "group.sizes")
  }
  list(order = o, sizes = sizes)
}
