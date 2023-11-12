# data frame Wrapper around GRP to convert lubridate intervals to group IDs
GRP2 <- function(X, ...){
  if (is_GRP(X)) return(X)
  args <- as.list(match.call())[-1]
  is_list_with_intervals <- is.list(X) && list_has_interval(X)
  if (is_list_with_intervals){
    x <- X
    which_int <- which(list_item_is_interval(X))
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
  # GRP[["groups"]]
  .subset2(GRP, "groups")
}
# Group variable names
GRP_group_vars <- function(GRP){
  .subset2(GRP, "group.vars")
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
    out <- frowid(GRP) > 1L
  }
  out
}
# Alternative that just returns dup indices
GRP_which_duplicated <- function(GRP, all = FALSE){
  cpp_which(GRP_duplicated(GRP, all))
}
calc_sorted_group_starts <- function(group_sizes){
  cpp_sorted_group_starts(as.integer(group_sizes))
}
calc_sorted_group_ends <- function(group_sizes){
  collapse::fcumsum(group_sizes)
}
# Extract group starts from GRP object safely and efficiently
GRP_starts <- function(GRP, use.g.names = FALSE,
                       loc = NULL){
  out <- GRP[["group.starts"]]
  if (is.null(out)){
    GRP_sizes <- GRP_group_sizes(GRP)
    if (GRP_is_sorted(GRP)){
      out <- calc_sorted_group_starts(GRP_sizes)
      # For factors with 0 size, replace calculated group starts with 0
      out[cpp_which(GRP_sizes == 0L)] <- 0L
    } else {
      if (is.null(loc)){
        loc <- GRP_loc(GRP, use.g.names = FALSE)
      }
      gstarts <- GRP_loc_starts(loc)
      # Accounting for factors with no data
      if (collapse::anyv(GRP_sizes, 0L)){
        out <- integer(length(loc))
        out[cpp_which(GRP_sizes != 0L)] <- gstarts
      } else {
        out <- gstarts
      }
    }
  }
  if (is.null(out)){
    out <- integer()
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
    out <- calc_sorted_group_ends(GRP_sizes)
    # For factors with 0 size, replace 0 with NA
    out[cpp_which(GRP_sizes == 0L)] <- 0L
  } else {
    if (is.null(loc)){
      loc <- GRP_loc(GRP, use.g.names = FALSE)
    }
    gends <- GRP_loc_ends(loc)
    # Accounting for factors with no data
    if (collapse::anyv(GRP_sizes, 0L)){
      out <- integer(length(loc))
      out[cpp_which(GRP_sizes != 0L)] <- gends
    } else {
      out <- gends
    }
  }
  if (is.null(out)){
    out <- integer()
  }
  if (use.g.names){
    names(out) <- GRP_names(GRP)
  }
  out
}
# Extract group order from GRP object safely
GRP_order <- function(GRP){
                      ### Only use the below arguments
                      ### If GRP_order is called from radixorderv2
                      ### Otherwise leave as is
                      # starts = TRUE, group.sizes = FALSE, sort = TRUE){
  out <- GRP[["order"]]
  if (is.null(out)){
    group_id <- GRP_group_id(GRP)
    if (GRP_is_sorted(GRP) || is_sorted(group_id)){
      out <- seq_along(group_id)
      # This should not be used unless through radixorderv
      # if (group.sizes){
      #   attributes(out) <- list("starts" = GRP_starts(GRP),
      #                           "group.sizes" = GRP_group_sizes(GRP),
      #                           "maxgrpn" = collapse::fmax(GRP_group_sizes(GRP)),
      #                           "sorted" = TRUE)
      # } else {
        attributes(out) <- list(starts = GRP_starts(GRP),
                                maxgrpn = collapse::fmax(GRP_group_sizes(GRP)),
                                sorted = TRUE)
      # }
    } else {
      out <- collapse::radixorderv(group_id,
                                   starts = TRUE,
                                   group.sizes = FALSE,
                                   sort = TRUE)
    }
  }
  # else if (group.sizes){
  #   ### Again, if we need group.sizes from radixorderv2
  #   ### We simply add group.sizes and reorder the attributes correctly
  #   starts <- attr(out, "starts")
  #   maxgrpn <- attr(out, "maxgrpn")
  #   sorted <- attr(out, "sorted")
  #   out <- strip_attrs(out)
  #   attributes(out) <- list("starts" = starts,
  #                           "group.sizes" = GRP_group_sizes(GRP),
  #                           "maxgrpn" = maxgrpn,
  #                           "sorted" = sorted)
  # }
  out
}
# Making this because of a bug when gsplit(NULL, GRP(x, sort = FALSE))
GRP_loc <- function(GRP, use.g.names = FALSE){
  if (length(GRP_group_id(GRP)) == 0L){
    if (use.g.names){
      add_names(list(), character(0))
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

GRP_names <- function(GRP, sep = "_", expand = FALSE, force.char = FALSE){
  g_names <- collapse::GRPnames(GRP, force.char = force.char, sep = sep)
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
  out <- vector("list", 9L)
  names(out) <- c("N.groups", "group.id",
                  "group.sizes", "groups",
                  "group.vars",
                  "ordered", "order",
                  "group.starts", "call")
  gdata <- group_data(data)
  gvars <- group_vars(data)
  n_groups <- df_nrow(gdata)
  group_id <- df_group_id(data)
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
  has_factor <- any(vapply(gdata, is.factor, FALSE, USE.NAMES = FALSE))
  if (return.groups){
    if (has_factor){
      gstarts <- integer(n_groups)
      gstarts[cpp_which(gsizes == 0L, invert = TRUE)] <-
        GRP_loc_starts(gdata[[".rows"]])
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
  class(out) <- "GRP"
  out
}
df_as_one_GRP <- function(data, order = TRUE,
                          return.order = TRUE){
  out <- vector("list", 9L)
  names(out) <- c("N.groups", "group.id",
                  "group.sizes", "groups",
                  "group.vars",
                  "ordered", "order",
                  "group.starts", "call")
  gsizes <- df_nrow(data)
  n_groups <- min(gsizes, 1L)
  gstarts <- if (n_groups == 0L) NULL else n_groups
  group_id <- collapse::alloc(1L, gsizes)
  if (order && return.order){
    gorder <- seq_len(gsizes)
    sorted <- TRUE
    attributes(gorder) <- list(starts = gstarts,
                               maxgrpn = gsizes,
                               sorted = TRUE)
  } else {
    gorder <- NULL
    sorted <- NA
  }
  gordered <- c("ordered" = order,
                "sorted" = sorted)
  out[["N.groups"]] <- n_groups
  out[["group.id"]] <- group_id
  out[["group.sizes"]] <- gsizes
  if (!is.null(gorder)){
    out[["order"]] <- gorder
  }
  if (!is.null(gstarts)){
    out[["group.starts"]] <- gstarts
  }
  out[["ordered"]] <- gordered
  class(out) <- "GRP"
  out
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
  gstarts <- calc_sorted_group_starts(group_sizes)
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
      sorted_group_starts <- calc_sorted_group_starts(group_sizes)
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

grouped_seq_len <- function(length, g = NULL, ...){
  if (is_integerable(length)){
    ones <- collapse::alloc(1L, length)
  } else {
    ones <- collapse::alloc(1, length)
  }
  collapse::fcumsum(ones, g = g, na.rm = FALSE, ...)
}

# Fast grouped row IDs when you already have  GRP object
GRP_row_id <- function(g, ascending = TRUE){
  g <- GRP2(g)
  size <- GRP_data_size(g)
  # If groups are sorted we can use sequence()
  if (GRP_is_sorted(g)){
    group_sizes <- GRP_group_sizes(g)
    if (ascending){
      start <- 1L
      every <- 1L
    } else {
      start <- group_sizes
      every <- -1L
    }
    out <- sequence2(group_sizes, from = start, by = every)
  } else {
    if (!ascending){
      o <- seq.int(length.out = size, from = size, by = -1L)
      out <- collapse::fcumsum(seq_ones(size), g = g, na.rm = FALSE,
                               o = o, check.o = FALSE)
    } else {
      out <- collapse::fcumsum(seq_ones(size), g = g, na.rm = FALSE)
    }
  }
  out
}

radixorderv2 <- function(x, starts = FALSE, sort = TRUE, group.sizes = FALSE,
                         ...){
  if (is.null(x)){
    return(NULL)
  }
  if (is_GRP(x)){
    return(GRP_order(x))
    # return(GRP_order(x, group.sizes = group.sizes))
  }
  if (is_interval(x)){
    x <- interval_separate(x)
  }
  if (is.list(x) && list_has_interval(x)){
    x <- mutate_intervals_to_ids(x)
  }
  collapse::radixorderv(x, starts = starts, sort = sort, group.sizes = group.sizes,
                        ...)
}
grouped_row_id <- function(x, ascending = TRUE){
  o <- radixorderv2(x, starts = FALSE, sort = FALSE, group.sizes = TRUE)
  if (is.null(o)){
    return(seq_len(vec_length(x)))
  }
  # Basically the order item of a GRP object
  # Doesn't naturally come with group sizes
  if (is_GRP(x)){
    group_sizes <- GRP_group_sizes(x)
  } else {
    group_sizes <- attr(o, "group.sizes")
  }
  starts <- attr(o, "starts")
  is_sorted <- isTRUE(attr(o, "sorted"))
  if (is_sorted){
    if (ascending){
      start <- 1L
      every <- 1L
    } else {
      start <- group_sizes
      every <- -1L
    }
    out <- sequence2(group_sizes, from = start, by = every)
  } else {
    out <- cpp_row_id(o, group_sizes, ascending)
  }
  out
}
