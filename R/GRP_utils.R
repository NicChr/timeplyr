# data frame Wrapper around GRP to convert lubridate intervals to group IDs
GRP2 <- function(X, ...){
  if (is_GRP(X)) return(X)
  args <- as.list(match.call())[-1]
  is_list_with_intervals <- is.list(X) && has_interval(X, quiet = TRUE)
  if (is_list_with_intervals){
    x <- X
    which_int <- which(vapply(X, FUN = is_interval, FUN.VALUE = logical(1)))
    for (i in seq_along(which_int)){
      X[[.subset(which_int, i)]] <- group_id(.subset2(X, .subset(which_int, i)))
    }
    args[["X"]] <- X
  }
  out <- do.call(get("GRP", asNamespace("collapse")),
                 args, envir = parent.frame())
  if (is_list_with_intervals && !is.null(GRP_groups(out))){
    group_starts <- GRP_starts(out)
    for (i in seq_along(which_int)){
      out[["groups"]][[which_int[i]]] <-
        .subset2(x, .subset(which_int, i))[group_starts]
    }
  }
  out
}
# Not efficient at all but
# is an interesting alternative because
# as long as group_id and subset method
# are defined for any class, this should work
# GRP3 <- function(X, sort = TRUE, ...){
#   x <- X
#   args <- as.list(match.call())[-1]
#   if (is.list(X)){
#     for (i in seq_along(X)){
#       X[[i]] <- group_id(x[[i]], order = sort, as_qg = TRUE)
#     }
#   }
#   args[["X"]] <- X
#   out <- do.call(get("GRP", asNamespace("collapse")),
#                  c(args, call = FALSE), envir = parent.frame())
#   if (!is.null(GRP_groups(out))){
#     group_starts <- GRP_starts(out)
#     for (i in seq_along(GRP_groups(out))){
#       out[["groups"]][[i]] <- x[[i]][group_starts]
#     }
#   }
#   out
# }
# Is object a collapse GRP?
is_GRP <- function(GRP){
  inherits(GRP, "GRP")
}
# Number of groups
GRP_n_groups <- function(GRP){
  .subset2(GRP, "N.groups")
  # GRP[["N.groups"]]
}
# Group IDs (integer)
GRP_group_id <- function(GRP){
  .subset2(GRP, "group.id")
  # GRP[["group.id"]]
}
GRP_data_size <- function(GRP){
  length(GRP_group_id(GRP))
}
# Group sizes
GRP_group_sizes <- function(GRP){
  # GRP[["group.sizes"]]
  .subset2(GRP, "group.sizes")
}
GRP_expanded_group_sizes <- function(GRP){
  GRP_group_sizes(GRP)[GRP_group_id(GRP)]
}
# Groups
GRP_groups <- function(GRP){
  GRP[["groups"]]
}
check_GRP_has_groups <- function(GRP){
  if (is_GRP(GRP) && is.null(GRP_groups(GRP))){
    stop("GRP has no group data. Please supply a GRP object with group data")
  }
}
check_data_GRP_size <- function(x, GRP){
  if (!is.null(GRP)){
    size <- vec_length(x)
    GRP_size <- GRP_data_size(GRP)
    if (size != GRP_size){
      stop("size of x must match size of groups")
    }
  }
}

GRP_duplicated <- function(GRP, all = FALSE){
  sizes <- GRP_group_sizes(GRP)
  group_id <- GRP_group_id(GRP)
  if (all){
    out <- (sizes > 1L)[group_id]
  } else {
    out <- frowid(group_id, g = GRP) > 1L
  }
  out
}
# Alternative that just returns dup indices
GRP_which_duplicated <- function(GRP, all = FALSE){
  sizes <- GRP_group_sizes(GRP)
  group_id <- GRP_group_id(GRP)
  if (all){
    out <- collapse::whichv(sizes[group_id], 1L, invert = TRUE)
  } else {
    out <- collapse::whichv(frowid(group_id, g = GRP), 1L, invert = TRUE)
  }
  out
}
# Extract group starts from GRP object safely and efficiently
GRP_starts <- function(GRP, use.g.names = FALSE,
                       loc = NULL){
  out <- GRP[["group.starts"]]
  if (is.null(out)){
    GRP_sizes <- GRP_group_sizes(GRP)
    if (GRP_is_sorted(GRP)){
      out <- collapse::fcumsum(c(rep_len(1L, min(length(GRP_sizes), 1L)),
                                 GRP_sizes[-length(GRP_sizes)]))
      # For factors with 0 size, replace 0 with NA
      setv(out, collapse::whichv(GRP_sizes, 0L),
           0L, vind1 = TRUE)
    } else {
      if (is.null(loc)){
        loc <- GRP_loc(GRP, use.g.names = FALSE)
      }
      gstarts <- GRP_loc_starts(loc)
      # Accounting for factors with no data
      if (collapse::anyv(GRP_sizes, 0L)){
        out <- integer(length(loc))
        setv(out, which(GRP_sizes != 0L),
             gstarts, vind1 = TRUE)
      } else {
        out <- gstarts
      }
    }
  }
  if (is.null(out)){
    out <- integer(0)
  }
  if (use.g.names){
    names(out) <- GRP_names(GRP)
  }
  out
}
# Extract group ends from GRP object safely and efficiently
GRP_ends <- function(GRP, use.g.names = FALSE,
                     loc = NULL){
  GRP_sizes <- GRP_group_sizes(GRP)
  if (GRP_is_sorted(GRP)){
    out <- collapse::fcumsum(GRP_sizes)
    # For factors with 0 size, replace 0 with NA
    setv(out, collapse::whichv(GRP_sizes, 0L),
         0L, vind1 = TRUE)
  } else {
    if (is.null(loc)){
      loc <- GRP_loc(GRP, use.g.names = FALSE)
    }
    gends <- GRP_loc_ends(loc)
    # Accounting for factors with no data
    if (collapse::anyv(GRP_sizes, 0L)){
      out <- integer(length(loc))
      setv(out, which(GRP_sizes != 0L),
           gends, vind1 = TRUE)
    } else {
      out <- gends
    }
  }
  if (is.null(out)){
    out <- integer(0)
  }
  if (use.g.names){
    names(out) <- GRP_names(GRP)
  }
  out
}
# Extract group order from GRP object safely
GRP_order <- function(GRP){
  out <- GRP[["order"]]
  if (is.null(out)){
    group_id <- GRP_group_id(GRP)
    if (GRP_is_sorted(GRP) || is_sorted(group_id)){
      out <- seq_along(group_id)
      attr(out, "sorted") <- TRUE
    } else {
      out <- collapse::radixorderv(group_id)
    }
  }
  out
}
# Making this because of a bug when gsplit(NULL, GRP(x, sort = FALSE))
GRP_loc <- function(GRP, use.g.names = FALSE){
  if (length(GRP_group_id(GRP)) == 0L){
    if (use.g.names){
      setnames(list(), character(0))
    } else {
      list()
    }
  } else {
    collapse::gsplit(NULL, g = GRP, use.g.names = use.g.names)
  }
}
# GRP starts & ends from list of group locations
# Groups are assumed to be sorted and
# index locations are also assumed to be sorted
GRP_loc_starts <- function(loc){
  unlist(
    collapse::ffirst(
      loc,
      use.g.names = FALSE,
      na.rm = FALSE
    )
    , use.names = FALSE, recursive = FALSE
  )

}
GRP_loc_ends <- function(loc){
  unlist(
    collapse::flast(
      loc,
      use.g.names = FALSE,
      na.rm = FALSE
    )
    , use.names = FALSE, recursive = FALSE
  )
}
GRP_ordered <- function(GRP){
  GRP[["ordered"]]
}
GRP_is_ordered <- function(GRP){
  ordered <- GRP_ordered(GRP)
  sorted <- ordered[names(ordered) == "sorted"]
  ordered <- ordered[names(ordered) == "ordered"]
  isTRUE(ordered || (is.na(ordered) && !is.na(sorted)))
}
# Logical is GRP sorted
GRP_is_sorted <- function(GRP){
  ordered <- GRP_ordered(GRP)
  isTRUE(ordered[names(ordered) == "sorted"])
}
GRP_group_data <- function(GRP, expand = FALSE){
  out <- list_to_tibble(as.list(GRP_groups(GRP)))
  if (expand){
    out <- df_row_slice(out, GRP_group_id(GRP))
  }
  out
}

GRP_names <- function(GRP, sep = "_", expand = FALSE){
  g_names <- collapse::GRPnames(GRP, force.char = FALSE, sep = sep)
  if (expand && !is.null(g_names)){
    g_names[GRP_group_id(GRP)]
  } else {
    g_names
  }
}
# Convert data frame to GRP safely
# Either treats data as 1 big group or
# Uses dplyr group vars
df_as_GRP <- function(data, return.groups = TRUE, return.order = TRUE){
  out <- vector("list", 9)
  names(out) <- c("N.groups", "group.id",
                  "group.sizes", "groups",
                  "group.vars",
                  "ordered", "order",
                  "group.starts", "call")
  gdata <- group_data(data)
  gvars <- group_vars(data)
  n_groups <- nrow2(gdata)
  group_id <- dplyr::group_indices(data)
  gsizes <- collapse::vlengths(gdata[[".rows"]], use.names = FALSE)
  if (return.order){
    gorder <- collapse::radixorderv(group_id,
                                    starts = TRUE,
                                    sort = TRUE,
                                    na.last = TRUE)
    sorted <- attr(gorder, "sorted")
  } else {
    gorder <- NULL
    sorted <- NA
  }
  gordered <- c("ordered" = TRUE,
                "sorted" = sorted)
  has_factor <- any(vapply(gdata, is.factor, logical(1)))
  if (return.groups){
    if (has_factor){
      gstarts <- integer(n_groups)
      setv(gstarts,
           collapse::whichv(gsizes, 0L, invert = TRUE),
           GRP_loc_starts(gdata[[".rows"]]),
           vind1 = TRUE)
    } else {
      gstarts <- GRP_loc_starts(gdata[[".rows"]])
    }
  }
  out[["N.groups"]] <- n_groups
  out[["group.id"]] <- group_id
  out[["group.sizes"]] <- gsizes
  if (length(gvars) > 0L && return.groups){
    out[["groups"]] <- fselect(gdata, .cols = gvars)
    out[["group.vars"]] <- gvars
    out[["group.starts"]] <- gstarts
  }
  if (!is.null(gorder)){
    out[["order"]] <- gorder
  }
  out[["ordered"]] <- gordered
  structure(out, class = "GRP")
}
df_as_one_GRP <- function(data, order = TRUE,
                          return.order = TRUE){
  out <- vector("list", 9)
  names(out) <- c("N.groups", "group.id",
                  "group.sizes", "groups",
                  "group.vars",
                  "ordered", "order",
                  "group.starts", "call")
  gsizes <- nrow2(data)
  n_groups <- min(gsizes, 1L)
  group_id <- collapse::alloc(1L, gsizes)
  if (order && return.order){
    gorder <- seq_len(gsizes)
    sorted <- TRUE
  } else {
    gorder <- NULL
    sorted <- NA
  }
  gordered <- c("ordered" = order,
                "sorted" = sorted)
  gstarts <- n_groups
  out[["N.groups"]] <- n_groups
  out[["group.id"]] <- group_id
  out[["group.sizes"]] <- gsizes
  if (!is.null(gorder)){
    out[["order"]] <- gorder
  }
  out[["group.starts"]] <- gstarts
  out[["ordered"]] <- gordered
  structure(out, class = "GRP")
}
# Custom GRP method for data frames
# Group starts is always returned
df_to_GRP <- function(data, .cols = character(0),
                      order = TRUE,
                      return.order = TRUE,
                      return.groups = TRUE){
  dplyr_groups <- group_vars(data)
  cols <- col_select_names(data, .cols = .cols)
  extra_groups <- setdiff(cols, dplyr_groups)
  group_vars <- c(dplyr_groups, extra_groups)
  data <- fselect(data, .cols = group_vars)
  if (length(names(data)) == 0L){
    out <- df_as_one_GRP(data, order = order, return.order = return.order)
  } else if (length(extra_groups) == 0L && order){
    out <- df_as_GRP(data, return.order = return.order, return.groups = return.groups)
  } else {
    out <- GRP2(safe_ungroup(data), sort = order,
                return.order = return.order,
                return.groups = return.groups,
                call = FALSE)
  }
  # if (is.null(fpluck(out, "group.starts"))){
  #   out[["group.starts"]] <- GRP_starts(out)
  # }
  out
}
GRP.Interval <- function(X, ...){
  x <- X
  X <- interval_separate(x)
  out <- collapse::GRP(X, ...)
  if (!is.null(GRP_groups(out))){
    out[["groups"]] <- list(x = x[GRP_starts(out)])
  }
  out
}
GRP.NULL <- function(X, ...){
  NULL
}
GRP_row_id <- function(GRP, ascending = TRUE){
  size <- GRP_data_size(GRP)
  has_order <- !is.null(GRP[["order"]])
  is_sorted <- GRP_is_sorted(GRP)
  if (has_order || is_sorted){
    seq_sizes <- GRP_group_sizes(GRP)
    if (ascending){
      start <- 1L
      every <- 1L
    } else {
      start <- seq_sizes
      every <- -1L
    }
    out <- sequence2(seq_sizes, from = start, by = every)
    if (!is_sorted){
      out <- collapse::greorder(out, g = GRP)
    }
  } else {
    if (!ascending){
      o <- seq.int(from = size, to = min(1L, size), by = -1L)
    } else {
     o <- NULL
    }
    out <- fcumsum(seq_ones(size),
                   na.rm = FALSE,
                   check.o = FALSE,
                   o = o,
                   g = GRP)
  }
  out
}
# Use this to turn a sorted group ID into a GRP when you have basic group information
sorted_group_id_to_GRP <- function(x, n_groups, group_sizes,
                                   group.starts = TRUE){
  out <- structure(
    list(
      "N.groups" = n_groups,
      "group.id" = x,
      "group.sizes" = group_sizes,
      "groups" = NULL,
      "group.vars" = NULL,
      "ordered" = c("ordered" = TRUE, "sorted" = TRUE),
      "order" = structure(seq_along(x), sorted = TRUE),
      "group.starts" = NULL,
      "call" = NULL
    ),
    class = "GRP"
  )
  if (group.starts){
    out[["group.starts"]] <- cumsum(c(rep_len(1L, min(length(group_sizes), 1L)),
                                      group_sizes[-length(group_sizes)]))
  }
  out
}
gsplit2 <- function(x = NULL, g = NULL, use.g.names = FALSE, ...){
  if (is.null(g)){
    if (is.null(x)){
      list(seq_along(x))
    } else {
     list(x)
    }
  } else {
    collapse::gsplit(x, g = g, use.g.names = use.g.names, ...)
  }
}
sort_data_by_GRP <- function(x, g, sorted_group_starts = TRUE){
  has_groups <- !is.null(g)
  if (!has_groups){
    return(list(
      x = x,
      n_groups = min(vec_length(x), 1L),
      group_sizes = vec_length(x),
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
    x <- vec_slice3(x, group_order)
    group_id <- vec_slice3(group_id, group_order)
  }
  if (sorted_group_starts){
    if (groups_are_sorted){
      sorted_group_starts <- GRP_starts(g)
    } else {
      sorted_group_starts <- cumsum(c(rep_len(1L, min(length(group_sizes), 1L)),
                                      group_sizes[-length(group_sizes)]))
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
# Basic GRP info for when
# you need to sort data by groups
# and pass that to a grouped function
# sort_data_by_GRP <- function(x, g, sorted_group_starts = TRUE){
#   has_groups <- !is.null(g)
#   g <- GRP2(g)
#   check_data_GRP_size(x, g)
#   group_id <- group_id(g)
#   if (has_groups){
#     group_sizes <- GRP_group_sizes(g)
#     n_groups <- GRP_n_groups(g)
#     group_order <- GRP_order(g)
#     groups_are_sorted <- isTRUE(attr(group_order, "sorted"))
#     if (groups_are_sorted){
#       if (sorted_group_starts){
#         sorted_group_starts <- GRP_starts(g)
#       }
#     } else {
#       x <- x[group_order]
#       group_id <- group_id[group_order]
#       if (sorted_group_starts){
#         sorted_group_starts <- cumsum(c(rep_len(1L, min(length(group_sizes), 1L)),
#                                         group_sizes[-length(group_sizes)]))
#       }
#     }
#     sorted_GRP[["group.starts"]] <- sorted_group_starts
#   } else {
#     group_sizes <- length(x)
#     n_groups <- min(length(x), 1L)
#     groups_are_sorted <- TRUE
#     group_order <- NULL
#     sorted_GRP <- NULL
#   }
#   list(x = x,
#        n_groups = n_groups,
#        group_sizes = group_sizes,
#        GRP = g,
#        sorted_GRP = sorted_GRP,
#        sorted = groups_are_sorted,
#        group_order = group_order,
#        has_groups = has_groups)
#
# }
# sort_data_by_GRP <- function(x, g, sorted_group_starts = TRUE){
#   has_groups <- !is.null(g)
#   g <- GRP2(g)
#   check_data_GRP_size(x, g)
#   group_id <- group_id(g)
#   if (has_groups){
#     group_sizes <- GRP_group_sizes(g)
#     n_groups <- GRP_n_groups(g)
#     group_order <- GRP_order(g)
#     groups_are_sorted <- isTRUE(attr(group_order, "sorted"))
#     if (!groups_are_sorted){
#       x <- x[group_order]
#       group_id <- group_id[group_order]
#     }
#     if (sorted_group_starts){
#       if (groups_are_sorted){
#         sorted_group_starts <- GRP_starts(g)
#       } else {
#         sorted_group_starts <- cumsum(c(rep_len(1L, min(length(group_sizes), 1L)),
#                                         group_sizes[-length(group_sizes)]))
#       }
#     } else {
#       sorted_group_starts <- NULL
#     }
#     sorted_GRP <- sorted_group_id_to_GRP(
#       group_id,
#       n_groups = n_groups,
#       group_sizes = group_sizes,
#       group.starts = FALSE
#     )
#     sorted_GRP[["group.starts"]] <- sorted_group_starts
#   } else {
#     group_sizes <- length(x)
#     n_groups <- min(length(x), 1L)
#     groups_are_sorted <- TRUE
#     group_order <- NULL
#     sorted_GRP <- NULL
#   }
#   # if (!groups_are_sorted){
#   #   group_order <- GRP_order(g)
#   #   x <- x[group_order]
#   #   group_id <- group_id[group_order]
#   # } else {
#   #   group_order <- seq_along(group_id)
#   # }
#   # if (has_groups && groups_are_sorted && sorted_group_starts){
#   #   sorted_group_starts <- GRP_starts(g)
#   # } else if (sorted_group_starts){
#   #   sorted_group_starts <- cumsum(c(rep_len(1L, min(length(group_sizes), 1L)),
#   #                                   group_sizes[-length(group_sizes)]))
#   # }
#   # sorted_GRP[["group.starts"]] <- sorted_group_starts
#   list(x = x,
#        n_groups = n_groups,
#        group_sizes = group_sizes,
#        GRP = g,
#        sorted_GRP = sorted_GRP,
#        sorted = groups_are_sorted,
#        group_order = group_order,
#        has_groups = has_groups)
#
# }
# greorder but x can be a data frame or list
greorder2 <- function(x, g, ...){
  if (is.list(x)){
    vec_slice3(
      x,
      collapse::greorder(
        seq_len(vec_length(x)), g = g, ...
      )
    )
  } else {
    collapse::greorder(x, g = g, ...)
  }
}
