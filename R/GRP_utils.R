# data frame Wrapper around GRP to convert lubridate intervals to group IDs
GRP2 <- function(X, ...){
  args <- as.list(match.call())[-1]
  is_list_with_intervals <- is.list(X) && has_interval(X, quiet = TRUE)
  if (is_list_with_intervals){
    x <- X
    which_int <- which(vapply(X, FUN = is_interval, FUN.VALUE = logical(1)))
    for (i in seq_along(which_int)){
      X[[which_int[i]]] <- group_id(X[[which_int[i]]])
    }
    args[["X"]] <- X
  }
  out <- do.call(get("GRP", asNamespace("collapse")),
                 args, envir = parent.frame())
  if (is_list_with_intervals && !is.null(GRP_groups(out))){
    group_starts <- GRP_starts(out)
    for (i in seq_along(which_int)){
      out[["groups"]][[which_int[i]]] <-
        x[[which_int[i]]][group_starts]
    }
  }
  out
}
# Not efficient at all but
# is an interesting alternative because
# as long a group_id and subset method
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
  GRP[["N.groups"]]
}
# Group IDs (integer)
GRP_group_id <- function(GRP){
  GRP[["group.id"]]
}
GRP_data_size <- function(GRP){
  length(GRP_group_id(GRP))
}
# Group sizes
GRP_group_sizes <- function(GRP){
  GRP[["group.sizes"]]
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
# # Very efficient as well..
# GRP_duplicated2 <- function(GRP, all = FALSE){
#   sizes <- GRP_group_sizes(GRP)
#   group_id <- GRP_group_id(GRP)
#   if (all){
#     out <- (sizes > 1L)[group_id]
#   } else {
#     if (is.null(GRP[["group.starts"]])){
#       out <- frowid(group_id, g = GRP) > 1L
#     } else {
#      group_starts <- GRP_starts(GRP)[sizes > 0L]
#      out <- rep_len(TRUE, length(group_id))
#      setv(out, group_starts, FALSE, vind1 = TRUE)
#     }
#   }
#   out
# }
# # Another alternative
# GRP_duplicated2 <- function(GRP, all = FALSE){
#   sizes <- GRP_group_sizes(GRP)
#   group_id <- GRP_group_id(GRP)
#   dup_groups <- (sizes > 1L)[group_id]
#   if (!all){
#     group_starts <- GRP_starts(GRP)[sizes > 0L]
#     setv(dup_groups, group_starts, FALSE, vind1 = TRUE)
#   }
#   dup_groups
# }
# # Alternative but slightly less efficient
# GRP_duplicated3 <- function(GRP, all = FALSE){
#   sizes <- GRP_group_sizes(GRP)
#   group_id <- GRP_group_id(GRP)
#   if (all){
#     (sizes > 1L)[group_id]
#   } else {
#     frowid(group_id, g = GRP) != 1L
#   }
# }
# # Another alternative
# GRP_duplicated4 <- function(GRP, all = FALSE){
#   sizes <- GRP_group_sizes(GRP)
#   group_id <- GRP_group_id(GRP)
#   if (all){
#     (sizes > 1L)[group_id]
#   } else {
#     if (GRP_is_sorted(GRP)){
#       frowid(group_id, g = GRP) != 1L
#     } else {
#       dup_groups <- (sizes > 1L)[group_id]
#       group_starts <- GRP_starts(GRP)[sizes > 0L]
#       setv(dup_groups, group_starts, FALSE, vind1 = TRUE)
#     }
#   }
# }
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
    if (GRP_is_sorted(GRP)){
      out <- seq_along(GRP_group_id(GRP))
    } else {
      out <- collapse::radixorderv(GRP_group_id(GRP))
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
  #   as.integer(
  #     unlist(
  #       collapse::fmin(
  #         loc,
  #         use.g.names = FALSE,
  #         na.rm = FALSE
  #       )
  #       , use.names = FALSE, recursive = FALSE
  #     )
  #   )

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
  #   as.integer(
  #     unlist(
  #       collapse::fmax(
  #         loc,
  #         use.g.names = FALSE,
  #         na.rm = FALSE
  #       )
  #       , use.names = FALSE, recursive = FALSE
  #     )
  #   )
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
# Alternate version of GRP_group_data that uses template data
# GRP_group_data2 <- function(GRP, template){
#   if (is_GRP(template)){
#     template <- GRP_group_data(template)
#   }
#   if (is.null(GRP)){
#     size <- vec_length(template)
#     if (size == 0){
#      return(fselect(dplyr::tibble(x = integer(0)), .cols = character(0)))
#     } else {
#       return(fselect(dplyr::tibble(x = 1L), .cols = character(0)))
#     }
#   }
#   groups <- GRP_groups(GRP)
#   if (is.null(groups)){
#     groups <- vec_slice2(template, GRP_starts(GRP))
#     if (is.atomic(groups)){
#      groups <- list(x = groups)
#     }
#   }
#   out <- list_to_tibble(as.list(groups))
#   if (is.atomic(template) && collapse::fncol(out) == 1L){
#     out <- frename(out, .cols = c("x" = 1L))
#   }
#   out
# }
GRP_names <- function(GRP, sep = "_", expand = FALSE){
  g_names <- collapse::GRPnames(GRP, force.char = FALSE, sep = sep)
  if (expand && !is.null(g_names)){
    g_names[GRP_group_id(GRP)]
  } else {
    g_names
  }
  # if (expand && !is.null(g_names)){
  #   GRP_loc <- GRP_loc(GRP)
  #   out <- integer(GRP_data_size(GRP))
  #   for (i in seq_along(GRP_loc)){
  #     out[GRP_loc[[i]]] <- g_names[i]
  #     # The below has memory issues
  #     # setv(out, GRP_loc[[i]], g_names[i], vind1 = TRUE)
  #   }
  # } else {
  #  out <- g_names
  # }
  # out
}
# df_to_GRP <- function(data, .cols = character(0), ...){
#   if (
#   if (collapse::fncol(data) == 0L){
#     NULL
#   } else {
#     GRP2(data, ...)
#   }
# }
# Can GRP be used for group_by()?
# GRP_is_dplyr_group_able <- function(GRP){
#   is_df(GRP_groups(GRP))
# }
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
  group_id <- alloc(1L, gsizes)
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
# Alternative to df_to_GRP that works nicely
# but is not as efficient due to
# double calculation of group ID and order
# df_to_GRP2 <- function(data, .cols = character(0),
#                       order = TRUE,
#                       return.order = TRUE){
#   out <- vector("list", 9)
#   names(out) <- c("N.groups", "group.id",
#                   "group.sizes", "groups",
#                   "group.vars",
#                   "ordered", "order",
#                   "group.starts", "call")
#   group_data <- group_collapse(data, .cols = .cols,
#                                order = order,
#                                id = FALSE,
#                                start = TRUE,
#                                end = FALSE,
#                                loc = FALSE,
#                                size = TRUE,
#                                sort = order)
#   gvars <- setdiff(names(group_data), c(".group", ".loc", ".start",
#                                         ".end", ".size"))
#   n_groups <- nrow2(group_data)
#   group_id <- group_id(data, .cols = .cols, order = order)
#   gsizes <- group_data[[".size"]]
#   if (order && return.order){
#     gorder <- collapse::radixorderv(group_id,
#                                     starts = TRUE,
#                                     sort = TRUE,
#                                     na.last = TRUE)
#     sorted <- attr(gorder, "sorted")
#   } else {
#     gorder <- NULL
#     sorted <- NA
#   }
#   gordered <- c("ordered" = order,
#                 "sorted" = sorted)
#   gstarts <- group_data[[".start"]]
#   out[["N.groups"]] <- n_groups
#   out[["group.id"]] <- group_id
#   out[["group.sizes"]] <- gsizes
#   if (length(gvars) > 0L){
#     out[["groups"]] <- fselect(group_data, .cols = gvars)
#     out[["group.vars"]] <- gvars
#   }
#   if (!is.null(gorder)){
#     out[["order"]] <- gorder
#   }
#   out[["group.starts"]] <- gstarts
#   out[["ordered"]] <- gordered
#   structure(out, class = "GRP")
# }
# dplyr grouped_df to GRP
# df_as_GRP <- function(data, return.order = TRUE){
#   out <- vector("list", 9)
#   names(out) <- c("N.groups", "group.id",
#                   "group.sizes", "groups",
#                   "group.vars",
#                   "ordered", "order",
#                   "group.starts", "call")
#   gdata <- group_data(data)
#   gvars <- group_vars(data)
#   n_groups <- nrow2(gdata)
#   group_id <- dplyr::group_indices(data)
#   gsizes <- collapse::vlengths(gdata[[".rows"]], use.names = FALSE)
#   if (return.order){
#     gorder <- collapse::radixorderv(group_id,
#                                     starts = TRUE,
#                                     sort = TRUE,
#                                     na.last = TRUE)
#     sorted <- attr(gorder, "sorted")
#   } else {
#     gorder <- NULL
#     sorted <- NA
#   }
#   gordered <- c("ordered" = TRUE,
#                 "sorted" = sorted)
#   # gstarts <- GRP_loc_starts(gdata[[".rows"]])
#   has_factor <- any(vapply(gdata, is.factor, logical(1)))
#   if (has_factor){
#     gstarts <- integer(n_groups)
#     setv(gstarts,
#          collapse::whichv(gsizes, 0L, invert = TRUE),
#          GRP_loc_starts(gdata[[".rows"]]),
#          vind1 = TRUE)
#   } else {
#     gstarts <- GRP_loc_starts(gdata[[".rows"]])
#   }
#   out[["N.groups"]] <- n_groups
#   out[["group.id"]] <- group_id
#   out[["group.sizes"]] <- gsizes
#   if (length(gvars) > 0L){
#     out[["groups"]] <- fselect(gdata, .cols = gvars)
#     out[["group.vars"]] <- gvars
#   }
#   out[["order"]] <- gorder
#   out[["ordered"]] <- gordered
#   out[["group.starts"]] <- gstarts
#   structure(out, class = "GRP")
# }
