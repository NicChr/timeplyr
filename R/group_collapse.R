#' Key group information
#'
#' @description
#' `group_collapse()` is similar to `dplyr::group_data()` but differs in 3 key regards:
#'
#' * The output tries to convey as much information about the groups as possible.
#' By default, like `dplyr`, the groups are ordered, but unlike `dplyr` they are not
#' sorted, which conveys information on order-of-first-appearance in the data.
#' In addition to group locations, group sizes and start indices are returned.
#'
#' * There is more flexibility in specifying how the groups are ordered and/or sorted.
#'
#' * `collapse` is used to obtain the grouping structure, which is very fast.
#'
#' `tidyselect` is used to specify the groups.
#'
#' @param data A data frame or vector.
#' @param ... Additional groups using `tidyselect` notation.
#' @param order Should the groups be ordered?
#' \bold{THE PHYSICAL ORDER OF THE DATA IS NOT CHANGED.} \cr
#' When order is `TRUE` (the default) the group IDs will be ordered but not sorted.
#' If `FALSE` the order of the group IDs will be based on first appearance.
#' @param sort Should the data frame be sorted by the groups?
#' @param ascending Should groups be ordered in ascending order?
#' Default is `TRUE` and only applies when `order = TRUE`.
#' @param .by Alternative way of supplying groups using `tidyselect` notation.
#' This is kept to be consistent with other functions.
#' @param size Should group sizes be added? Default is `TRUE`.
#' @param loc Should group locations be added? Default is `TRUE`.
#' @param start Should group start locations be added? Default is `TRUE`.
#' @param end Should group end locations be added? Default is `TRUE`.
#' @return
#' A `tibble` of unique groups and an integer ID uniquely identifying each group.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#'
#' iris <- dplyr::as_tibble(iris)
#' group_collapse(iris) # No groups
#' group_collapse(iris, Species) # Species groups
#'
#' iris %>%
#'   group_by(Species) %>%
#'   group_collapse() # Same thing
#'
#' # Group entire data frame
#' group_collapse(iris, everything())
#' @rdname group_collapse
#' @export
group_collapse <- function(data, ..., order = TRUE, sort = FALSE,
                           ascending = TRUE,
                           .by = NULL,
                           size = TRUE, loc = TRUE,
                           # loc_order = TRUE,
                           start = TRUE, end = TRUE){
  UseMethod("group_collapse")
}
#' @export
group_collapse.default <- function(data, ..., order = TRUE, sort = FALSE,
                                   ascending = TRUE,
                                   .by = NULL,
                                   size = TRUE, loc = TRUE,
                                   # loc_order = TRUE,
                                   start = TRUE, end = TRUE){
  g <- GRP2(safe_ungroup(data),
            sort = order,
            decreasing = !ascending,
            na.last = TRUE,
            return.groups = TRUE,
            return.order = order || loc,
            method = "auto",
            call = FALSE)
  out <- dplyr::as_tibble(as.list(g[["groups"]]))
  out[[".group"]] <- seq_len(nrow2(out))
  if (loc || start || end){
    # out[[".loc"]] <- vctrs::as_list_of(
    #   vctrs::vec_locate_sorted_groups(g[["group.id"]],
    #                                   direction = "asc", na_value = "largest")[["loc"]],
    #   .ptype = integer(0)
    # )
    out[[".loc"]] <- collapse::gsplit(NULL, g = g)
    attr(out[[".loc"]], "ptype") <- integer(0)
    attr(out[[".loc"]], "class") <- c("vctrs_list_of", "vctrs_vctr", "list")
  }
  # if (loc_order){
  #   gorder <- g[["order"]]
  #   if (is.null(gorder)){
  #     gorder <- radix_order(g[["group.id"]])
  #   }
  #   for (a in names(attributes(gorder))){
  #     attr(gorder, a) <- NULL
  #   }
  #   out[[".order"]] <- vctrs::as_list_of(
  #     collapse::gsplit(x = gorder, g = g),
  #     .ptype = integer(0)
  #   )
  # }
  if (start){
    out[[".start"]] <- g[["group.starts"]]
    if (is.null(out[[".start"]])){
      out[[".start"]] <- as.integer(collapse::fmin(out[[".loc"]],
                                                   use.g.names = FALSE,
                                                   na.rm = FALSE))
    }
  }
  if (end){
    out[[".end"]] <- as.integer(collapse::fmax(out[[".loc"]],
                                               use.g.names = FALSE,
                                               na.rm = FALSE))
  }
  if (!loc){
    out[[".loc"]] <- NULL
  }
  if (size){
    out[[".size"]] <- g[["group.sizes"]]
  }
  if (!sort && order){
    out <- df_row_slice(out, data.table::frank(collapse::funique(g[["group.id"]], sort = FALSE),
                                               ties.method = "first"),
                        reconstruct = FALSE)
  }
  out
}
#' @export
group_collapse.data.frame <- function(data, ..., order = TRUE, sort = FALSE,
                                      ascending = TRUE,
                                      .by = NULL,
                                      size = TRUE, loc = TRUE,
                                      # loc_order = TRUE,
                                      start = TRUE, end = TRUE){
  vars <- get_group_info(data, !!!enquos(...),
                         type = "select", .by = {{ .by }})[["all_groups"]]
  if (length(vars) == 0L){
    rowids <- seq_len(nrow2(data))
    n <- length(rowids)
    ss <- min(nrow2(data), 1L)
    rowids <- list(rowids)[ss]
    out <- dplyr::tibble(!!".group" := integer(ss) + 1L)
    if (loc){
      out[[".loc"]] <- vctrs::as_list_of(rowids, .ptype = integer(0))
    }
    # if (loc_order){
    #   out[[".order"]] <- vctrs::as_list_of(rowids, .ptype = integer(0))
    # }
    if (start){
      out[[".start"]] <- integer(ss) + 1L
    }
    if (end){
      out[[".end"]] <- integer(ss) + n
    }
    if (size){
      out[[".size"]] <- n[ss]
    }
  } else {
    out <- group_collapse.default(collapse::fselect(data, vars),
                                  order = order, sort = sort,
                                  size = size, loc = loc,
                                  ascending = ascending,
                                  # loc_order = loc_order,
                                  start = start, end = end)
  }
  # out <- df_reconstruct(out, data)
  attr(out, "row.names") <- seq_len(nrow2(out))
  out
}
#' @export
group_collapse.grouped_df <- function(data, ..., order = TRUE, sort = FALSE,
                                      ascending = TRUE,
                                      .by = NULL,
                                      size = TRUE, loc = TRUE,
                                      # loc_order = TRUE,
                                      start = TRUE, end = TRUE){
  # Special conditions where if met,
  # we can use dplyr grouping structure
  if (dots_length(...) == 0 &&
      rlang::quo_is_null(enquo(.by)) &&
      sort &&
      order &&
      ascending){
    out <- dplyr::group_data(data)
    out_nms <- names(out)
    names(out)[out_nms == ".rows"] <- ".loc"
    out[[".group"]] <- seq_len(nrow2(out))
    ncol <- ncol(out)
    out <- out[, c(seq_len(ncol - 2L), ncol, ncol - 1L), drop = FALSE]
    if (start){
      out[[".start"]] <- collapse::fmin(out[[".loc"]],
                                        use.g.names = FALSE, na.rm = FALSE)
    }
    if (end){
      out[[".end"]] <- collapse::fmax(out[[".loc"]],
                                        use.g.names = FALSE, na.rm = FALSE)
    }
    if (size){
      out[[".size"]] <- collapse::vlengths(out[[".loc"]], use.names = FALSE)
    }
    if (!loc){
      out[[".loc"]] <- NULL
    }
    # if (loc_order){
    #   g <- dplyr::group_indices(data)
    #   out[[".order"]] <- vctrs::as_list_of(collapse::gsplit(radix_order(g),
    #                                                         g = g, use.g.names = FALSE),
    #                                        .ptype = integer(0))
    # }
  } else {
    vars <- get_group_info(data, !!!enquos(...),
                           type = "select", .by = {{ .by }})[["all_groups"]]
    out <- group_collapse(safe_ungroup(data), all_of(vars),
                          order = order, sort = sort,
                          size = size, loc = loc,
                          ascending = ascending,
                          # loc_order = loc_order,
                          start = start, end = end)
    attr(out, ".drop") <- dplyr::group_by_drop_default(data)
  }
  out
}
