#' Fast group and row IDs
#'
#' @description
#' *  `group_id()` returns an integer vector of group IDs the same size as the data.
#' *  `add_group_id()` adds an integer column of group IDs.
#' *  `row_id()` returns an integer vector of row IDs.
#' *  `add_row_id()` adds an integer column of row IDs.
#' *  `group_order()` returns the order of the groups.
#' *  `add_group_order()` adds an integer column of the order of the groups.
#'
#' It's important to note that when using `data.frames`, these functions by default assume
#' no groups.
#' This means that when no groups are supplied:
#' * `group_id(iris)` returns a vector of ones
#' * `row_id(iris)` returns the plain row id numbers
#' * `group_order(iris) == row_id(iris)`.
#'
#' One can specify groups in the second argument like so:
#' * `group_id(iris, Species)`
#' * `row_id(iris, dplyr::all_of("Species"))`
#' * `group_order(iris, contains("width"))`
#'
#' @param data A data frame or vector.
#' @param ... Additional groups using tidy select notation.
#' @param order Should the groups be ordered?
#' \bold{THE PHYSICAL ORDER OF THE DATA IS NOT CHANGED.} \cr
#' When order is `TRUE` (the default) the group IDs will be ordered but not sorted.
#' If `FALSE` the order of the group IDs will be based on first appearance.
#' @param ascending Should the group order be ascending or descending?
#' The default is `TRUE`. \cr
#' For `row_id()` this determines if the row IDs are increasing or decreasing. \cr
#' \bold{NOTE} - When `order = FALSE`, the `ascending` argument is
#' ignored. This is something that will be fixed in a later version.
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
                     ascending = TRUE,
                     .by = NULL,
                     as_qg = FALSE){
  UseMethod("group_id")
}
#' @export
group_id.default <- function(data, ..., order = TRUE,
                             ascending = TRUE, as_qg = FALSE){
  if (order){
    out <- GRP2(safe_ungroup(data),
                sort = TRUE,
                decreasing = !ascending,
                na.last = TRUE,
                return.groups = FALSE,
                return.order = FALSE,
                method = "auto",
                call = FALSE)[["group.id"]]
  } else {
    out <- group2(data)
  }
  if (as_qg && order){
    out <- qG2(out, sort = TRUE, ordered = order, na.exclude = FALSE)
  }
  if (!as_qg && !order){
    out <- as.integer(out)
  }
  out
}
#' @export
group_id.Interval <- function(data, ..., order = TRUE, ascending = TRUE, as_qg = FALSE){
  out <- GRP.Interval(data, sort = order, decreasing = !ascending,
                      call = FALSE, return.groups = FALSE)[["group.id"]]
  if (as_qg){
    out <- qG2(out, sort = order, ordered = order, na.exclude = FALSE)
  }
  out
  # group_id.default(out, ..., order = order, ascending = ascending, as_qg = as_qg)
}
#' @export
group_id.data.frame <- function(data, ...,
                                order = TRUE,
                                ascending = TRUE,
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
                                ascending &&
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
                  decreasing = !ascending,
                  na.last = TRUE,
                  return.groups = FALSE,
                  return.order = FALSE,
                  method = "auto",
                  call = FALSE)[["group.id"]]
    }
  if (as_qg){
    out <- qG2(out, sort = order, ordered = order, na.exclude = FALSE)
  } else {
    out <- as.integer(out)
  }
  out
}
#' @rdname group_id
#' @export
add_group_id <- function(data, ...,
                         order = TRUE,
                         ascending = TRUE,
                         .by = NULL,
                         .name = NULL,
                         as_qg = FALSE){
  if (is.null(.name)) .name <- new_var_nm(names(data), "group_id")
  data[[.name]] <- group_id.data.frame(data, !!!enquos(...),
                                       order = order,
                                       ascending = ascending,
                                       .by = {{ .by }},
                                       as_qg = as_qg)
  data
}
#' @rdname group_id
#' @export
row_id <- function(data, ..., ascending = TRUE, .by = NULL){
  UseMethod("row_id")
}
#' @export
row_id.default <- function(data, ..., ascending = TRUE, .by = NULL){
  g <- GRP2(safe_ungroup(data),
              sort = TRUE,
              decreasing = FALSE,
              na.last = TRUE,
              return.groups = FALSE,
              return.order = FALSE,
              method = "auto",
              call = FALSE)
  growid(data, g = g, ascending = ascending)
}
#' @export
row_id.data.frame <- function(data, ..., ascending = TRUE, .by = NULL){
  vars <- get_group_info(data, !!!enquos(...),
                         type = "select", .by = {{ .by }})[["all_groups"]]
  if (length(vars) == 0L){
    g <- NULL
  } else {
    g <- GRP2(collapse::fselect(data, vars),
              sort = TRUE,
              decreasing = FALSE,
              return.groups = FALSE, return.order = FALSE,
              call = FALSE)
  }
  growid(data, g = g, ascending = ascending)
}
#' @rdname group_id
#' @export
add_row_id <- function(data, ..., ascending = TRUE, .by = NULL, .name = NULL){
  if (is.null(.name)) .name <- new_var_nm(names(data), "row_id")
  data[[.name]] <- row_id.data.frame(data, !!!enquos(...), ascending = ascending, .by = {{ .by }})
  data
}
#' @rdname group_id
#' @export
group_order <- function(data, ..., ascending = TRUE, .by = NULL){
  UseMethod("group_order")
}
#' @export
group_order.default <- function(data, ..., ascending = TRUE, .by = NULL){
  if (is_interval(data)){
    data <- GRP.Interval(data, sort = TRUE, call = FALSE,
                         return.groups = FALSE)[["group.id"]]
  }
  as.integer(collapse::radixorderv(data, decreasing = !ascending,
                                   na.last = TRUE, starts = FALSE,
                                   group.sizes = FALSE, sort = TRUE))
  # Alternate method
  # g <- GRP2(safe_ungroup(data),
  #           sort = TRUE,
  #           decreasing = !ascending,
  #           na.last = TRUE,
  #           return.groups = FALSE,
  #           return.order = TRUE,
  #           method = "auto",
  #           call = FALSE)
  # out <- g[["order"]]
  # if (is.null(out)){
  #   out <- radix_order(g[["group.id"]])
  # }
  # as.integer(out)
}
#' @export
group_order.data.frame <- function(data, ..., ascending = TRUE, .by = NULL){
  vars <- get_group_info(data, !!!enquos(...),
                         type = "select", .by = {{ .by }})[["all_groups"]]
  if (length(vars) == 0L){
    g <- NULL
    out <- seq_len(nrow2(data))
  } else {
    if (has_interval(data, quiet = TRUE)){
      which_int <- which(vapply(data, FUN = is_interval, FUN.VALUE = logical(1)))
      for (i in seq_along(which_int)){
        data[[which_int[[i]]]] <- group_id.Interval(data[[which_int[[i]]]],
                                                 order = TRUE, as_qg = FALSE)
      }
    }
    out <- group_order.default(collapse::fselect(data, vars),
                               ascending = ascending)
    # Alternate method
    # g <- GRP2(collapse::fselect(data, vars),
    #           sort = TRUE,
    #           decreasing = !ascending,
    #           return.groups = FALSE, return.order = TRUE,
    #           call = FALSE)
    # out <- g[["order"]]
    # if (is.null(out)){
    #   out <- radix_order(g[["group.id"]])
    # }
  }
  as.integer(out)
}
#' @rdname group_id
#' @export
add_group_order <- function(data, ..., ascending = TRUE, .by = NULL, .name = NULL){
  if (is.null(.name)) .name <- new_var_nm(names(data), "group_order")
  data[[.name]] <- group_order.data.frame(data, !!!enquos(...), .by = {{ .by }},
                                          ascending = ascending)
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
qG2 <- function(x, sort = TRUE, na.exclude = FALSE, ...){
  if (is_interval(x)){
    i_na <- collapse::whichNA(x)
    x <- GRP.Interval(x, sort = sort, call = FALSE, return.groups = FALSE)[["group.id"]]
    if (na.exclude){
      setv(x, i_na, NA_integer_, vind1 = TRUE)
    }
  }
  collapse::qG(x, sort = sort, na.exclude = na.exclude, ...)
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
