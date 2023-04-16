#' Fast group IDs
#'
#' @description
#' *  `group_id()` returns an integer vector of group IDs the same size as the data.
#' *  `add_group_id()` adds an integer column of group IDs.
#' *  `row_id()` returns an integer vector of row IDs.
#' *  `add_row_id()` adds an integer column of row IDs.
#'
#' @param data A data frame or vector.
#' @param ... Additional groups using tidy select notation.
#' @param order Should the groups be ordered?
#' \bold{THE PHYSICAL ORDER OF THE DATA IS NOT CHANGED.} \cr
#' When order is `TRUE` (the default) the group IDs will be ordered but not sorted.
#' If `FALSE` the order of the group IDs will be based on first appearance.
#' @param .by Alternative way of supplying groups using tidy
#' select notation. This is kept to be consistent with other functions.
#' @param .name Name of the added group ID column which should be a
#' character vector of length 1.
#' If `NULL` then a column named "group_id" will be added,
#' and if one already exists, a unique name will be used.
#' @param as_qg Should the group IDs be returned as a
#' collapse "qG" class? The default (`FALSE`) always returns
#' an integer vector.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(ggplot2)
#' group_id(iris) # No groups
#' group_id(iris, Species) # Species groups
#' iris %>%
#'   group_by(Species) %>%
#'   group_id() # Same thing
#' group_id(iris, where(is.numeric)) # Groups across numeric values
#'
#' iris %>%
#'   add_group_id(Species) %>%
#'   distinct(Species, group_id)
#'
#' mm_mpg <- mpg %>%
#'   select(manufacturer, model) %>%
#'   arrange(desc(pick(everything())))
#'
#' # Sorted/non-sorted groups
#' mm_mpg %>%
#'   add_group_id(everything(),
#'                .name = "sorted_id", order = TRUE) %>%
#'   add_group_id(manufacturer, model,
#'                .name = "not_sorted_id", order = FALSE) %>%
#'   distinct()
#' @rdname group_id
#' @export
group_id <- function(data, ...,
                     order = TRUE,
                     .by = NULL,
                     as_qg = FALSE){
  UseMethod("group_id")
}
#' @export
group_id.default <- function(data, ..., order = TRUE, as_qg = FALSE){
  if (order){
    out <- GRP2(safe_ungroup(data),
                sort = TRUE,
                decreasing = FALSE,
                na.last = TRUE,
                return.groups = FALSE,
                return.order = FALSE,
                method = "auto",
                call = FALSE)[["group.id"]]
  } else {
    out <- group2(data)
  }
  if (as_qg && order){
    out <- collapse::qG(out, sort = TRUE, ordered = FALSE, na.exclude = FALSE)
  }
  if (!as_qg && !order){
    out <- as.integer(out)
  }
  out
}
#' @export
group_id.Interval <- function(data, ..., order = TRUE, as_qg = FALSE){
  data <- GRP.Interval(data, sort = order,
                       call = FALSE, return.groups = FALSE)[["group.id"]]
  group_id.default(data, ..., order = order, as_qg = as_qg)
}
#' @export
group_id.data.frame <- function(data, ...,
                                order = TRUE,
                                .by = NULL,
                                as_qg = FALSE){
  N <- nrow2(data)
  group_vars <- group_vars(data)
  by_vars <- tidy_select_names(data, {{ .by }})
  dot_vars <- tidy_select_names(data, !!!enquos(...))
  if (length(by_vars) > 0L){
    if (length(group_vars) > 0L){
      stop(".by cannot be used on a grouped_df")
    }
  }
  select_info <- tidy_select_info(data, all_of(c(group_vars, by_vars)),
                                  !!!enquos(...))
  pos <- select_info[["pos"]]
  in_nms <- select_info[["in_nms"]]
  out_nms <- select_info[["out_nms"]]
  data <- collapse::fselect(data, pos)
  names(data) <- out_nms
  # Group var might have been renamed, so use select info
  group_vars2 <- out_nms[match(c(group_vars, by_vars), in_nms)]
  # by and group vars cannot both be supplied (unless .overwrite = TRUE)
  needs_regrouping <- !isTRUE(order &&
                                length(group_vars) > 0L &&
                                length(by_vars) == 0L &&
                                length(dot_vars) == 0L)
    # Usual Method for when data does not contain interval
    if (length(group_vars2) == 0L &&
        length(dot_vars) == 0L){
      out <- rep_len(1L, N)
      # Method for grouped_df
    } else if (!needs_regrouping){
      out <- dplyr::group_indices(data)
    } else {
      out <- GRP2(safe_ungroup(data),
                  sort = order,
                  decreasing = FALSE,
                  na.last = TRUE,
                  return.groups = FALSE,
                  return.order = FALSE,
                  method = "auto",
                  call = FALSE)[["group.id"]]
    }
  if (as_qg){
    out <- collapse::qG(out, sort = order, ordered = FALSE, na.exclude = FALSE)
  } else {
    out <- as.integer(out)
  }
  out
}
#' @rdname group_id
#' @export
add_group_id <- function(data, ...,
                         order = TRUE,
                         .by = NULL,
                         .name = NULL,
                         as_qg = FALSE){
  if (is.null(.name)) .name <- new_var_nm(names(data), "group_id")
  data[[.name]] <- group_id.data.frame(data, !!!enquos(...),
                                       order = order, .by = {{ .by }},
                                       as_qg = as_qg)
  data
}
#' @rdname group_id
#' @export
row_id <- function(data, ..., .by = NULL){
  UseMethod("row_id")
}
#' @export
row_id.default <- function(data, ..., .by = NULL){
  g <- GRP2(safe_ungroup(data),
              sort = TRUE,
              decreasing = FALSE,
              na.last = TRUE,
              return.groups = FALSE,
              return.order = FALSE,
              method = "auto",
              call = FALSE)
  growid(data, g = g)
}
#' @export
row_id.data.frame <- function(data, ..., .by = NULL){
  vars <- get_group_info(data, !!!enquos(...),
                         type = "select", .by = {{ .by }})[["all_groups"]]
  if (length(vars) == 0L){
    g <- NULL
  } else {
    g <- GRP2(collapse::fselect(data, vars),
              sort = TRUE,
              return.groups = FALSE, return.order = FALSE,
              call = FALSE)
  }
  growid(data, g = g)
}
#' @rdname group_id
#' @export
add_row_id <- function(data, ..., .by = NULL, .name = NULL){
  if (is.null(.name)) .name <- new_var_nm(names(data), "row_id")
  data[[.name]] <- row_id.data.frame(data, !!!enquos(...), .by = {{ .by }})
  data
}
GRP.Interval <- function(X, ...){
  X <- dplyr::tibble(!!"start" := lubridate::int_start(X),
                     !!"data" := lubridate::int_length(X))
  collapse::GRP(X, ...)
}
group2 <- function(X, ...){
  if (is_interval(X)){
    X <- dplyr::tibble(!!"start" := lubridate::int_start(X),
                       !!"data" := lubridate::int_length(X))
  }
  if (is_df(X) && has_interval(X, quiet = TRUE)){
    which_int <- which(vapply(X, FUN = is_interval, FUN.VALUE = logical(1)))
    for (i in seq_along(which_int)){
      X[[which_int[[i]]]] <- group_id.Interval(X[[which_int[[i]]]],
                                               order = FALSE, as_qg = FALSE)
    }
  }
  collapse::group(X, ...)
}
# data frame Wrapper around GRP to convert lubridate intervals to group IDs
GRP2 <- function(X, ...){
  if (is_df(X) && has_interval(X, quiet = TRUE)){
    which_int <- which(vapply(X, FUN = is_interval, FUN.VALUE = logical(1)))
    for (i in seq_along(which_int)){
      X[[which_int[[i]]]] <- group_id.Interval(X[[which_int[[i]]]],
                                               order = TRUE, as_qg = FALSE)
    }
    collapse::GRP(X, ...)
  } else {
    do.call(get("GRP", asNamespace("collapse")),
            as.list(match.call())[-1L],
            envir = parent.frame())
  }
  # collapse::GRP(X, ...)
}
# group_loc <- function(data, ..., order = TRUE, .by = NULL,
#                       sort = FALSE){
#   .Deprecated("group_collapse")
#   UseMethod("group_loc")
# }
# group_loc.default <- function(data, ..., order = TRUE, .by = NULL,
#                               sort = FALSE){
#   g <- GRP2(safe_ungroup(data),
#               sort = order,
#               decreasing = FALSE,
#               na.last = TRUE,
#               return.groups = TRUE,
#               return.order = FALSE,
#               method = "auto",
#               call = FALSE)
#   out <- dplyr::as_tibble(as.list(g[["groups"]]))
#   grp_nm <- new_var_nm(names(out), "group_id")
#   out[[grp_nm]] <- seq_len(nrow2(out))
#   rowids <- growid(data, g = NULL)
#   out[[".rows"]] <- vctrs::as_list_of(
#     collapse::gsplit(x = rowids, g = g),
#     .ptype = integer(0)
#   )
#   if (!sort && order){
#     out <- out[data.table::frankv(collapse::funique(g[["group.id"]]),
#                                   ties.method = "average"), , drop = FALSE]
#   }
#   out
# }
# group_loc.data.frame <- function(data, ..., order = TRUE, .by = NULL,
#                                  sort = FALSE){
#   vars <- get_group_info(data, !!!enquos(...),
#                          type = "select", .by = {{ .by }})[["all_groups"]]
#   if (length(vars) == 0L){
#     rowids <- seq_len(nrow2(data))
#     n <- min(nrow2(data), 1L)
#     rowids <- list(rowids)[n]
#     out <- data.frame(group_id = integer(n) + 1L,
#                       .rows = vctrs::as_list_of(rowids, .ptype = integer(0)))
#   } else {
#     out <- group_loc.default(collapse::fselect(safe_ungroup(data), vars),
#                              order = order, sort = sort)
#   }
#   out <- df_reconstruct(out, safe_ungroup(data))
#   attr(out, "row.names") <- seq_len(nrow2(out))
#   out
# }
