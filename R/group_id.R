#' Fast group IDs
#'
#' @description
#' These are tidy-based functions for calculating group IDs, row IDs and
#' group orders. \cr
#'
#' *  `group_id()` returns an integer vector of group IDs the same size as the data.
#' *  `row_id()` returns an integer vector of row IDs.
#' *  `group_order()` returns the order of the groups.
#'
#' The `add_` variants add a column of group IDs/row IDs/group orders.
#'
#' @param data A data frame or vector.
#' @param ... Additional groups using tidy `data-masking` rules. \cr
#' To specify groups using `tidyselect`, simply use the `.by` argument.
#' @param order Should the groups be ordered?
#' \bold{THE PHYSICAL ORDER OF THE DATA IS NOT CHANGED.} \cr
#' When order is `TRUE` (the default) the group IDs will be ordered but not sorted.
#' If `FALSE` the order of the group IDs will be based on first appearance.
#' @param ascending Should the group order be ascending or descending?
#' The default is `TRUE`. \cr
#' For `row_id()` this determines if the row IDs are increasing or decreasing. \cr
#' \bold{NOTE} - When `order = FALSE`, the `ascending` argument is
#' ignored. This is something that will be fixed in a later version.
#' @param .by Alternative way of supplying groups using `tidyselect` notation.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .name Name of the added group ID column which should be a
#' character vector of length 1.
#' If `NULL` then a column named "group_id" will be added,
#' and if one already exists, a unique name will be used.
#' @param as_qg Should the group IDs be returned as a
#' collapse "qG" class? The default (`FALSE`) always returns
#' an integer vector.
#' @details
#' It's important to note for `data.frames`, these functions by default assume
#' no groups unless you supply them.
#'
#' This means that when no groups are supplied:
#' * `group_id(iris)` returns a vector of ones
#' * `row_id(iris)` returns the plain row id numbers
#' * `group_order(iris) == row_id(iris)`.
#'
#' One can specify groups in the second argument like so:
#' * `group_id(iris, Species)`
#' * `row_id(iris, across(all_of("Species")))`
#' * `group_order(iris, across(where(is.numeric), desc))`
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(ggplot2)
#' group_id(iris) # No groups
#' group_id(iris, Species) # Species groups
#' row_id(iris) # Plain row IDs
#' row_id(iris, Species) # Row IDs by group
#' # Order of Species + descending Petal.Width
#' group_order(iris, Species, desc(Petal.Width))
#'
#' # Tidy data-masking/tidyselect can be used
#' group_id(iris, across(where(is.numeric))) # Groups across numeric values
#' # Alternatively using tidyselect
#' group_id(iris, .by = where(is.numeric))
#'
#' # Group IDs using a mixtured order
#' group_id(iris, desc(Species), Sepal.Length, desc(Petal.Width))
#'
#' # add_ helpers
#' iris %>%
#'   add_group_id(Sepal.Length) %>%
#'   add_row_id(Sepal.Length) %>%
#'   add_group_order(Sepal.Length) %>%
#'   distinct(Sepal.Length, group_id, row_id, group_order)
#'
#' mm_mpg <- mpg %>%
#'   select(manufacturer, model) %>%
#'   arrange(desc(pick(everything())))
#'
#' # Sorted/non-sorted groups
#' mm_mpg %>%
#'   add_group_id(across(everything()),
#'                .name = "sorted_id", order = TRUE) %>%
#'   add_group_id(manufacturer, model,
#'                .name = "not_sorted_id", order = FALSE) %>%
#'   distinct()
#' @rdname group_id
#' @export
group_id <- function(data, ...,
                     order = TRUE,
                     ascending = TRUE,
                     .by = NULL, .cols = NULL,
                     as_qg = FALSE){
  UseMethod("group_id")
}
#' @export
group_id.default <- function(data, ..., order = TRUE,
                             ascending = TRUE,
                             as_qg = FALSE){
  if (order){
    g <- GRP2(safe_ungroup(data),
                sort = TRUE,
                decreasing = !ascending,
                na.last = TRUE,
                return.groups = FALSE,
                return.order = FALSE,
                method = "auto",
                call = FALSE)
    out <- GRP_group_id(g)
  } else {
    out <- group2(data)
  }
  if (as_qg && order){
    out <- group_id_to_qg(out, n_groups = GRP_n_groups(g), ordered = TRUE)
  }
  if (!as_qg && !order){
    out <- qg_to_integer(out)
  }
  out
}
#' @export
group_id.Interval <- function(data, ..., order = TRUE,
                              ascending = TRUE, as_qg = FALSE){
  g <- GRP.Interval(data, sort = order, decreasing = !ascending,
                      call = FALSE, return.groups = FALSE)
  out <- GRP_group_id(g)
  if (as_qg){
    out <- group_id_to_qg(out, n_groups = GRP_n_groups(g),
                          ordered = order)
  }
  out
}
#' @export
group_id.data.frame <- function(data, ...,
                                order = TRUE,
                                ascending = TRUE,
                                .by = NULL, .cols = NULL,
                                as_qg = FALSE){
  N <- nrow2(data)
  group_info <- group_info(data, ..., .by = {{ .by }},
                           .cols = .cols,
                           ungroup = TRUE,
                           rename = FALSE)
  all_groups <- group_info[["all_groups"]]
  # Usual Method for when data does not contain interval
  if (length(all_groups) == 0L){
    out <- alloc(1L, N)
    n_groups <- min(N, 1L)
    # Method for grouped_df
  } else {
    g <- GRP2(group_info[["data"]], by = all_groups,
                sort = order,
                decreasing = !ascending,
                na.last = TRUE,
                return.groups = FALSE,
                return.order = FALSE,
                method = "auto",
                call = FALSE)
    out <- GRP_group_id(g)
    n_groups <- GRP_n_groups(g)
  }
  if (as_qg){
    out <- group_id_to_qg(out, n_groups = n_groups,
                          ordered = order)
  }
  out
}
#' @export
group_id.grouped_df <- function(data, ...,
                                order = TRUE,
                                ascending = TRUE,
                                .by = NULL, .cols = NULL,
                                as_qg = FALSE){
  n_dots <- dots_length(...)
  # Error checking on .by
  check_by(data, .by = {{ .by }})
  if (n_dots == 0 && is.null(.cols) && order && ascending){
    out <- dplyr::group_indices(data)
    if (as_qg){
      out <- group_id_to_qg(out, n_groups = nrow2(group_data(data)),
                            ordered = order)
    }
  } else {
    group_info <- group_info(data, ..., .by = {{ .by }},
                             .cols = .cols,
                             ungroup = TRUE,
                             rename = FALSE)
    all_groups <- group_info[["all_groups"]]
    out <- group_id(group_info[["data"]], .cols = all_groups,
                    order = order, ascending = ascending,
                    as_qg = as_qg)
  }
  out
}
#' @rdname group_id
#' @export
add_group_id <- function(data, ...,
                         order = TRUE,
                         ascending = TRUE,
                         .by = NULL, .cols = NULL,
                         .name = NULL,
                         as_qg = FALSE){
  if (is.null(.name)) .name <- new_var_nm(names(data), "group_id")
  dplyr::dplyr_col_modify(data, setnames(list(group_id(data, ...,
                                                       order = order,
                                                       ascending = ascending,
                                                       .by = {{ .by }},
                                                       .cols = .cols,
                                                       as_qg = as_qg)),
                                         .name))
}
#' @rdname group_id
#' @export
row_id <- function(data, ..., ascending = TRUE,
                   .by = NULL, .cols = NULL){
  UseMethod("row_id")
}
#' @export
row_id.default <- function(data, ..., ascending = TRUE){
  frowid(safe_ungroup(data), ascending = ascending)
}
#' @export
row_id.data.frame <- function(data, ...,
                              ascending = TRUE,
                              .by = NULL, .cols = NULL){
  N <- nrow2(data)
  group_info <- group_info(data, ..., .by = {{ .by }},
                           .cols = .cols,
                           ungroup = TRUE,
                           rename = FALSE)
  data <- group_info[["data"]]
  vars <- group_info[["all_groups"]]
  if (length(vars) == 0L){
    g <- NULL
  } else {
    g <- GRP2(data, by = vars,
              sort = TRUE,
              decreasing = FALSE,
              return.groups = FALSE, return.order = TRUE,
              call = FALSE)
  }
  frowid(data, g = g, ascending = ascending)
}
#' @export
row_id.grouped_df <- row_id.data.frame
#' @rdname group_id
#' @export
add_row_id <- function(data, ..., ascending = TRUE,
                       .by = NULL, .cols = NULL,
                       .name = NULL){
  if (is.null(.name)) .name <- new_var_nm(names(data), "row_id")
  dplyr::dplyr_col_modify(data, setnames(list(row_id(data, ...,
                                                     ascending = ascending,
                                                     .by = {{ .by }}, .cols = .cols)),
                                         .name))
}
#' @rdname group_id
#' @export
group_order <- function(data, ..., ascending = TRUE,
                        .by = NULL, .cols = NULL){
  UseMethod("group_order")
}
#' @export
group_order.default <- function(data, ..., ascending = TRUE){
  as.integer(radixorderv2(data, decreasing = !ascending,
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
  # for (a in names(attributes(out))){
  #   attr(out, a) <- NULL
  # }
  # if (is.null(out)){
  #   radix_order(GRP_group_id(g))
  # } else {
  #  out
  # }
}
#' @export
group_order.Interval <- function(data, ..., ascending = TRUE){
  x <- interval_separate(x)
  as.integer(collapse::radixorderv(x, decreasing = !ascending))
}
#' @export
group_order.data.frame <- function(data, ..., ascending = TRUE,
                                   .by = NULL, .cols = NULL){
  N <- nrow2(data)
  group_info <- group_info(data, ..., .by = {{ .by }},
                           .cols = .cols,
                           ungroup = TRUE,
                           rename = FALSE)
  all_groups <- group_info[["all_groups"]]
  if (length(all_groups) == 0L){
    if (ascending){
      out <- seq_len(N)
    } else {
      out <- seq.int(from = N,
                     to = min(N, 1L),
                     by = -1L)
    }
  } else {
    out <- as.integer(radixorderv2(collapse::fselect(group_info[["data"]],
                                                     all_groups),
                                   decreasing = !ascending,
                                   na.last = TRUE, starts = FALSE,
                                   group.sizes = FALSE, sort = TRUE))
  }
  out
}
#' @export
group_order.grouped_df <- group_order.data.frame
#' @rdname group_id
#' @export
add_group_order <- function(data, ..., ascending = TRUE,
                            .by = NULL, .cols = NULL,
                            .name = NULL){
  if (is.null(.name)) .name <- new_var_nm(names(data), "group_order")
  dplyr::dplyr_col_modify(data, setnames(list(group_order(data, ...,
                                                          .by = {{ .by }}, .cols = .cols,
                                                          ascending = ascending)),
                                         .name))
}
# group_sort <- function(data, ..., ascending = TRUE, .by = NULL){
#   UseMethod("group_sort")
# }
# group_sort.default <- function(data, ..., ascending = TRUE, .by = NULL){
#   gorder <- group_order.default(data, ascending = ascending)
#   if (is_strictly_increasing(gorder)){
#     data
#   } else {
#     vctrs::vec_slice(data, gorder)
#   }
# }
# group_sort.data.frame <- function(data, ..., ascending = TRUE, .by = NULL){
#   n_dots <- dots_length(...)
#   out <- safe_ungroup(data)
#   group_vars <- get_groups(data, .by = {{  .by }})
#   if (n_dots > 0){
#     out <- mutate2(out, ...)
#     dot_vars <- tidy_transform_names(out, ...)
#     group_vars <- c(group_vars, dot_vars)
#   }
#   gorder <- group_order.default(collapse::fselect(out, group_vars),
#                         ascending = ascending)
#   if (length(group_vars) == 0L || is_strictly_increasing(gorder)){
#     data
#   } else {
#     vctrs::vec_slice(data, gorder)
#   }
# }
GRP.Interval <- function(X, ...){
  X <- interval_separate(X)
  collapse::GRP(X, ...)
}
group2 <- function(X, ...){
  if (is_interval(X)){
    X <- interval_separate(X)
  }
  if (is.list(X) && has_interval(X, quiet = TRUE)){
    X <- mutate_intervals_to_ids(X)
  }
  collapse::group(X, ...)
}
qG2 <- function(x, sort = TRUE, na.exclude = FALSE, ...){
  if (is_interval(x)){
    g <- GRP.Interval(x, sort = sort, call = FALSE, return.groups = FALSE)
    out <- GRP_group_id(g)
    if (na.exclude){
      setv(out, collapse::whichNA(x), NA_integer_, vind1 = TRUE)
    }
    out <- group_id_to_qg(out, GRP_n_groups(g))
  } else {
    out <- collapse::qG(x, sort = sort, na.exclude = na.exclude, ...)
  }
  out
}
# data frame Wrapper around GRP to convert lubridate intervals to group IDs
GRP2 <- function(X, ...){
  args <- as.list(match.call())[-1]
  if (is.list(X) && has_interval(X, quiet = TRUE)){
    X <- mutate_intervals_to_ids(X)
    args[["X"]] <- X
  }
  do.call(get("GRP", asNamespace("collapse")),
          args, envir = parent.frame())
}
# Mutate interval columns to group IDs
mutate_intervals_to_ids <- function(data){
  which_int <- which(vapply(data, FUN = is_interval, FUN.VALUE = logical(1)))
  for (i in seq_along(which_int)){
    data[[which_int[[i]]]] <- group_id.Interval(data[[which_int[[i]]]],
                                                order = TRUE, as_qg = FALSE)
  }
  data
}
interval_separate <- function(x){
  list("start" = lubridate::int_start(x),
       "data" = lubridate::int_length(x))
}
radixorderv2 <- function(x, ...){
  if (is_interval(x)){
    x <- interval_separate(x)
  }
  if (is.list(x) && has_interval(x, quiet = TRUE)){
    x <- mutate_intervals_to_ids(x)
  }
  collapse::radixorderv(x, ...)
}
group_id_to_qg <- function(x, n_groups = NULL, ordered = FALSE){
  if (is.null(n_groups)){
    n_groups <- collapse::fndistinct(x, na.rm = FALSE)
  }
  attr(x, "N.groups") <- n_groups
  if (ordered){
    attr(x, "class") <- c("ordered", "qG", "na.included")
  } else {
    attr(x, "class") <- c("qG", "na.included")
  }
  x
}
# Efficiently convert qG to integer
qg_to_integer <- function(x){
  attributes(x) <- NULL
  x
}
# group_id_to_GRP <- function(x, ordered = TRUE, return_order = FALSE){
#   if (length(x) == 0L){
#     g_sizes <- integer(0)
#   } else {
#     g_sizes <- collapse::fnobs(x, g = x, use.g.names = FALSE)
#   }
#   n_groups <- length(g_sizes)
#   if (ordered){
#     sorted <- !is.unsorted(x)
#     # Placeholder
#     # if (return_order){
#     #
#     # }
#   } else {
#     sorted <- NA
#   }
#   out <- list("N.groups" = n_groups,
#               "group.id" = x,
#               "group.sizes" = g_sizes,
#               "groups" = NULL,
#               "group.vars" = NULL,
#               "ordered" = c("ordered" = ordered,
#                             "sorted" = sorted),
#               "order" = NULL,
#               "group.starts" = NULL,
#               "call" = NULL)
#   class(out) <- "GRP"
#   out
# }
