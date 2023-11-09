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
#' When order is `TRUE` (the default) the group IDs will be
#' ordered but not sorted.\cr
#' The expression
#' \preformatted{
#' identical(order(x, na.last = TRUE),
#'           order(group_id(x, order = TRUE)))
#' }
#' or in the case of a data frame
#' \preformatted{
#' identical(order(x1, x2, x3, na.last = TRUE),
#'           order(group_id(data, x1, x2, x3, order = TRUE)))
#' }
#' should always hold.\cr
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
#' @param .name Name of the added ID column which should be a
#' character vector of length 1.
#' If `.name = NULL` (the default),
#' `add_group_id()` will add a column named "group_id",
#' and if one already exists, a unique name will be used.
#' @param as_qg Should the group IDs be returned as a
#' collapse "qG" class? The default (`FALSE`) always returns
#' an integer vector.
#'
#' @returns
#' An integer vector.
#'
#' @details
#' It's important to note for data frames, these functions by default assume
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
#'
#' If you want `group_id` to always use all the columns of a data frame
#' for grouping
#' while simultaneously utilising the `group_id` methods, one can use the below
#' function.
#'
#' \preformatted{
#' group_id2 <- function(data, ...){
#'  group_id(data, ..., .cols = names(data))
#' }
#' }
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(ggplot2)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' group_id(iris) # No groups
#' group_id(iris, Species) # Species groups
#' row_id(iris) # Plain row IDs
#' row_id(iris, Species) # Row IDs by group
#' # Order of Species + descending Petal.Width
#' group_order(iris, Species, desc(Petal.Width))
#' # Same as
#' order(iris$Species, -xtfrm(iris$Petal.Width))
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
#'   distinct(Species) %>%
#'   add_group_id(Species)
#' iris %>%
#'   add_row_id(Species) %>%
#'   pull(row_id)
#'
#' # Usage in data.table
#' library(data.table)
#' iris_dt <- as.data.table(iris)
#' iris_dt[, group_id := group_id(.SD, .cols = names(.SD)),
#'         .SDcols = "Species"]
#'
#' # Or if you're using this often you can write a wrapper
#' set_add_group_id <- function(x, ..., .name = "group_id"){
#'   id <- group_id(x, ...)
#'   data.table::set(x, j = .name, value = id)
#' }
#' set_add_group_id(iris_dt, desc(Species))[]
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
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
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
  g <- GRP2(safe_ungroup(data),
            sort = order,
            decreasing = !ascending,
            na.last = TRUE,
            return.groups = FALSE,
            return.order = FALSE,
            method = "auto",
            call = FALSE)
  out <- GRP_group_id(g)
  if (as_qg){
    out <- group_id_to_qg(out,
                          n_groups = GRP_n_groups(g),
                          # group_starts = g[["group.starts"]],
                          group_sizes = GRP_group_sizes(g),
                          ordered = order)
  }
  out
}
#' @export
group_id.factor <- function(data, ..., order = TRUE,
                             ascending = TRUE,
                             as_qg = FALSE){
  group_id(strip_attrs(unclass(data)),
           order = order, ascending = ascending, as_qg = as_qg)
}
# No need to have this anymore as there is a collapse::GRP.interval method..
#' @export
group_id.Interval <- function(data, ..., order = TRUE,
                              ascending = TRUE, as_qg = FALSE){
  X <- interval_separate(data)
  # X[[1L]][is.na(X[[2L]])] <- NA
  groups <- collapse::GRP(X, sort = order,
                          decreasing = !ascending,
                          call = FALSE,
                          return.groups = FALSE,
                          return.order = FALSE)
  out <- GRP_group_id(groups)
  if (as_qg){
    out <- group_id_to_qg(out,
                          n_groups = GRP_n_groups(groups),
                          # group_starts = groups[["group.starts"]],
                          group_sizes = GRP_group_sizes(groups),
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
  N <- df_nrow(data)
  group_info <- group_info(data, ..., .by = {{ .by }},
                           .cols = .cols,
                           ungroup = TRUE,
                           rename = FALSE)
  all_groups <- group_info[["all_groups"]]
  # Usual Method for when data does not contain interval
  if (length(all_groups) == 0L){
    out <- collapse::alloc(1L, N)
    n_groups <- min(N, 1L)
    group_sizes <- N
    # group_starts <- n_groups
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
    group_sizes <- GRP_group_sizes(g)
    # group_starts <- g[["group.starts"]]
  }
  if (as_qg){
    out <- group_id_to_qg(out,
                          n_groups = n_groups,
                          # group_starts = group_starts,
                          group_sizes = group_sizes,
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
    out <- df_group_id(data)
    if (as_qg){
      out <- group_id_to_qg(out,
                            n_groups = df_nrow(group_data(data)),
                            group_sizes = collapse::vlengths(group_data(data)[[".rows"]],
                                                             use.names = FALSE),
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
#' @export
group_id.GRP <- function(data, ..., order = TRUE, as_qg = FALSE){
  group_ids <- GRP_group_id(data)
  if (!order && GRP_is_ordered(data)){
    out <- group_id(group_ids, order = order, as_qg = as_qg)
  } else {
    out <- group_ids
    if (as_qg){
      out <- group_id_to_qg(out,
                            n_groups = GRP_n_groups(data),
                            group_sizes = GRP_group_sizes(data),
                            # group_starts = data[["group.starts"]],
                            ordered = order)
    }
  }
  out
}
#' @export
group_id.NULL <- function(data, ...){
  NULL
}
#' @rdname group_id
#' @export
add_group_id <- function(data, ...,
                         order = TRUE,
                         ascending = TRUE,
                         .by = NULL, .cols = NULL,
                         .name = NULL,
                         as_qg = FALSE){
  if (is.null(.name)){
    .name <- new_var_nm(names(data), "group_id")
  }
  group_ids <- group_id(data, ...,
                        order = order,
                        ascending = ascending,
                        .by = {{ .by }},
                        .cols = .cols,
                        as_qg = as_qg)
  col_to_add <- add_names(list(group_ids), .name)
  dplyr::dplyr_col_modify(data, col_to_add)
}
#' @rdname group_id
#' @export
row_id <- function(data, ..., ascending = TRUE,
                   .by = NULL, .cols = NULL){
  UseMethod("row_id")
}
#' @export
row_id.default <- function(data, ..., ascending = TRUE, order = TRUE){
  frowid(safe_ungroup(data), ascending = ascending, order = order)
}
#' @export
row_id.data.frame <- function(data, ...,
                              ascending = TRUE, order = TRUE,
                              .by = NULL, .cols = NULL){
  N <- df_nrow(data)
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
              sort = order,
              decreasing = FALSE,
              return.groups = FALSE, return.order = TRUE,
              call = FALSE)
  }
  frowid(data, g = g, ascending = ascending, order = order)
}
#' @export
row_id.grouped_df <- row_id.data.frame
#' @rdname group_id
#' @export
add_row_id <- function(data, ..., ascending = TRUE,
                       .by = NULL, .cols = NULL,
                       .name = NULL){
  if (is.null(.name)){
    .name <- new_var_nm(names(data), "row_id")
  }
  row_ids <- row_id(data, ...,
                    ascending = ascending,
                    .by = {{ .by }}, .cols = .cols)
  col_to_add <- add_names(list(row_ids), .name)
  dplyr::dplyr_col_modify(data, col_to_add)
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
  N <- df_nrow(data)
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
  if (is.null(.name)){
    .name <- new_var_nm(names(data), "group_order")
  }
  dplyr::dplyr_col_modify(data, add_names(list(group_order(data, ...,
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
group2 <- function(X, ...){
  if (is_interval(X)){
    X <- interval_separate(X)
  }
  if (is.list(X) && list_has_interval(X)){
    X <- mutate_intervals_to_ids(X)
  }
  collapse::group(X, ...)
}
qG2 <- function(x, sort = TRUE, ordered = FALSE, na.exclude = FALSE, ...){
  if (is_interval(x)){
    if (na.exclude){
      which_not_na <- cpp_which(int_is_na(x), invert = TRUE)
      out <- collapse::alloc(NA_integer_, length(x))
      qgroup <- group_id(x[which_not_na], order = sort, as_qg = TRUE)
      n_groups <- attr(qgroup, "N.groups")
      out[which_not_na] <- qg_to_integer(qgroup)
      if (ordered){
        collapse::setattrib(out, list("N.groups" = n_groups,
                                      "class" = c("qG", "ordered")))
      } else {
        collapse::setattrib(out, list("N.groups" = n_groups,
                                      "class" = "qG"))
      }
    } else {
      out <- group_id(x, order = sort, as_qg = TRUE)
      n_groups <- attr(out, "N.groups")
      out <- qg_to_integer(out)
      out <- group_id_to_qg(out, n_groups = n_groups, ordered = ordered)
    }
  } else {
    out <- collapse::qG(x, sort = sort,
                        ordered = ordered,
                        na.exclude = na.exclude, ...)
    if (ordered){
      class(out) <- c("qG", "ordered", setdiff(class(out), c("qG", "ordered")))
    }
  }
  out
}
# Mutate interval columns to group IDs
mutate_intervals_to_ids <- function(data){
  which_int <- which(list_item_is_interval(data))
  for (i in seq_along(which_int)){
    data[[.subset2(which_int, i)]] <- group_id.Interval(.subset2(data, .subset2(which_int, i)))
  }
  data
}
interval_separate <- function(x){
  list(start = attr(x, "start"),
       data = strip_attrs(unclass(x)))
}
# list("start" = lubridate::int_start(x),
#      "data" = lubridate::int_length(x))
radixorderv2 <- function(x, ...){
  if (is_interval(x)){
    x <- interval_separate(x)
  }
  if (is.list(x) && list_has_interval(x)){
    x <- mutate_intervals_to_ids(x)
  }
  collapse::radixorderv(x, ...)
}
group_id_to_qg <- function(x,
                           n_groups = NULL,
                           group_starts = NULL,
                           group_sizes = NULL,
                           ordered = FALSE){
  if (is.null(n_groups)){
    n_groups <- collapse::fnunique(x)
  }
  attr(x, "N.groups") <- n_groups
  if (!is.null(group_starts)){
    attr(x, "starts") <- group_starts
  }
  if (!is.null(group_sizes)){
    attr(x, "group.sizes") <- group_sizes
  }
  if (ordered){
    class(x) <- c("qG", "ordered", "na.included")
  } else {
    class(x) <- c("qG", "na.included")
  }
  x
}
# Efficiently convert qG to integer
qg_to_integer <- function(x){
  strip_attrs(x)
}
