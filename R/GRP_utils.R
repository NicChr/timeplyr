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
# Groups
GRP_groups <- function(GRP){
  GRP[["groups"]]
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
  # fun <- rlang::arg_match0(fun, c("first", "min"))
  # if (fun == "first"){
  #   unlist(
  #     collapse::ffirst(
  #       loc,
  #       use.g.names = FALSE,
  #       na.rm = FALSE
  #     )
  #     , use.names = FALSE, recursive = FALSE
  #   )
  # } else {
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
  # }

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
  # fun <- rlang::arg_match0(fun, c("last", "max"))
  # if (fun == "last"){
  #   unlist(
  #     collapse::flast(
  #       loc,
  #       use.g.names = FALSE,
  #       na.rm = FALSE
  #     )
  #     , use.names = FALSE, recursive = FALSE
  #   )
  # } else {
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
  # }
}
GRP_ordered <- function(GRP){
  GRP[["ordered"]]
}
GRP_is_ordered <- function(GRP){
  ordered <- GRP_ordered(GRP)
  isTRUE(ordered[names(ordered) == "ordered"])
}
# Logical is GRP sorted
GRP_is_sorted <- function(GRP){
  ordered <- GRP_ordered(GRP)
  isTRUE(ordered[names(ordered) == "sorted"])
}
GRP_group_data <- function(GRP){
  list_to_tibble(as.list(GRP_groups(GRP)))
}
GRP_names <- function(GRP, sep = "_"){
  collapse::GRPnames(GRP, sep = sep)
}
df_to_GRP <- function(data, .cols = names(data), ...){
  data <- fselect(data, .cols = .cols)
  if (collapse::fncol(data) == 0L){
    NULL
  } else {
    GRP2(data, ...)
  }
}
# Can GRP be used for group_by()?
GRP_is_dplyr_group_able <- function(GRP){
  is_df(GRP_groups(GRP))
}
# dplyr grouped_df to GRP
grouped_df_to_GRP <- function(data, return.order = TRUE){
  if (inherits(data, "grouped_df")){
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
    # gstarts <- GRP_loc_starts(gdata[[".rows"]])
    has_factor <- any(vapply(gdata, is.factor, logical(1)))
    if (has_factor){
      gstarts <- integer(n_groups)
      setv(gstarts,
           collapse::whichv(gsizes, 0L, invert = TRUE),
           GRP_loc_starts(gdata[[".rows"]]),
           vind1 = TRUE)
    } else {
      gstarts <- GRP_loc_starts(gdata[[".rows"]])
    }
    out[["N.groups"]] <- n_groups
    out[["group.id"]] <- group_id
    out[["group.sizes"]] <- gsizes
    out[["groups"]] <- fselect(gdata, .cols = gvars)
    out[["group.vars"]] <- gvars
    out[["order"]] <- gorder
    out[["ordered"]] <- gordered
    out[["group.starts"]] <- gstarts
    structure(out, class = "GRP")
  } else {
    NULL
  }
}
