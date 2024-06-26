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
  g <- GRP3(safe_ungroup(data),
            sort = order,
            decreasing = !ascending,
            na.last = TRUE,
            return.groups = FALSE,
            return.order = order,
            method = "auto",
            call = FALSE)
  out <- GRP_group_id(g)
  if (as_qg){
    out <- group_id_to_qg(out, n_groups = GRP_n_groups(g),
                   group_starts = GRP_starts(g),
                   group_sizes = GRP_group_sizes(g),
                   ordered = order)
  }
  out
}
#' @export
group_id.factor <- function(data, ..., order = TRUE,
                             ascending = TRUE,
                             as_qg = FALSE){
  out <- unclass(data)
  if (order && ascending && !as_qg){
    out <- strip_attrs(out)
    out[cheapr::which_na(out)] <- length(levels(data)) + 1L
  } else {
    out <- group_id(out, order = order,
                    ascending = ascending, as_qg = as_qg)
  }
  out
}
# No need to have this anymore as there is a collapse::GRP.interval method..
#' @export
group_id.Interval <- function(data, ..., order = TRUE,
                              ascending = TRUE, as_qg = FALSE){
  X <- interval_separate(data)
  groups <- GRP3(X, sort = order,
                 decreasing = !ascending,
                 call = FALSE,
                 return.groups = FALSE,
                 return.order = order)
  out <- GRP_group_id(groups)
  if (as_qg){
    out <- group_id_to_qg(out,
                          n_groups = GRP_n_groups(groups),
                          group_starts = GRP_starts(groups),
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
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = FALSE)
  all_groups <- group_info[["all_groups"]]
  # Usual Method for when data does not contain interval
  if (length(all_groups) == 0L){
    out <- collapse::alloc(1L, N)
    n_groups <- min(N, 1L)
    group_sizes <- N
    group_starts <- n_groups
  } else {
    g <- GRP3(group_info[["data"]],
              by = all_groups,
              sort = order,
              decreasing = !ascending,
              na.last = TRUE,
              return.groups = FALSE,
              return.order = order,
              method = "auto",
              call = FALSE)
    out <- GRP_group_id(g)
    n_groups <- GRP_n_groups(g)
    group_sizes <- GRP_group_sizes(g)
    group_starts <- GRP_starts(g)
  }
  if (as_qg){
    out <- group_id_to_qg(out,
                          n_groups = n_groups,
                          group_starts = group_starts,
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
      groups <- group_data(data)
      out <- group_id_to_qg(out,
                            n_groups = df_nrow(groups),
                            group_starts = GRP_loc_starts(groups[[".rows"]]),
                            group_sizes = cheapr::lengths_(groups[[".rows"]]),
                            ordered = order)
    }
  } else {
    group_info <- tidy_group_info(data, ..., .by = {{ .by }},
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
                            group_starts = GRP_starts(data),
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
  df_add_cols(data, col_to_add)
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
  N <- df_nrow(data)
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = FALSE)
  data <- group_info[["data"]]
  vars <- group_info[["all_groups"]]
  if (length(vars) == 0L){
    if (ascending){
      seq_len(N)
    } else {
      seq.int(length.out = N, from = N, by = -1L)
    }
  } else {
    frowid(fselect(data, .cols = vars), ascending = ascending)
  }
}
#' @export
row_id.grouped_df <- row_id.data.frame
#' @rdname group_id
#' @export
row_id.GRP <- function(data, ascending = TRUE, ...){
  size <- GRP_data_size(data)
  # If groups are sorted we can use sequence()
  if (GRP_is_sorted(data)){
    group_sizes <- GRP_group_sizes(data)
    if (ascending){
      start <- 1L
      every <- 1L
    } else {
      start <- group_sizes
      every <- -1L
    }
    out <- sequences(group_sizes, from = start, by = every)
  } else {
    if (!ascending){
      o <- seq.int(length.out = size, from = size, by = -1L)
      out <- collapse::fcumsum(seq_ones(size), g = data, na.rm = FALSE,
                               o = o, check.o = FALSE)
    } else {
      out <- collapse::fcumsum(seq_ones(size), g = data, na.rm = FALSE)
    }
  }
  out
}
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
  df_add_cols(data, col_to_add)
}
#' @rdname group_id
#' @export
group_order <- function(data, ..., ascending = TRUE,
                        .by = NULL, .cols = NULL){
  UseMethod("group_order")
}
#' @export
group_order.default <- function(data, ..., ascending = TRUE){
  radixorderv2(data, decreasing = !ascending,
               na.last = TRUE, starts = FALSE,
               group.sizes = FALSE, sort = TRUE)
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
  collapse::radixorderv(x, decreasing = !ascending,
                        na.last = TRUE, starts = FALSE,
                        group.sizes = FALSE, sort = TRUE)
}
#' @export
group_order.data.frame <- function(data, ..., ascending = TRUE,
                                   .by = NULL, .cols = NULL){
  N <- df_nrow(data)
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
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
    out <- radixorderv2(fselect(group_info[["data"]], .cols = all_groups),
                                   decreasing = !ascending,
                                   na.last = TRUE, starts = FALSE,
                                   group.sizes = FALSE, sort = TRUE)
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
  df_add_cols(data, add_names(list(group_order(data, ...,
                                               .by = {{ .by }}, .cols = .cols,
                                               ascending = ascending)),
                              .name))
}

qG2 <- function(x, sort = TRUE, ordered = FALSE, na.exclude = FALSE, ...){
  if (is_interval(x)){
    if (na.exclude){
      which_not_na <- cheapr::which_not_na(x)
      out <- rep_len(NA_integer_, length(x))
      qgroup <- group_id(x[which_not_na], order = sort, as_qg = TRUE)
      n_groups <- attr(qgroup, "N.groups")
      out[which_not_na] <- qg_to_integer(qgroup)
      if (ordered){
        set_add_attributes(out, list("N.groups" = n_groups,
                                     "class" = c("qG", "ordered")))
      } else {
        set_add_attributes(out, list("N.groups" = n_groups,
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
      set_add_attr(out, "class", c("qG", "ordered", setdiff(class(out), c("qG", "ordered"))))
    }
  }
  out
}
# Mutate interval columns to group IDs
mutate_intervals_to_ids <- function(data, order = TRUE){
  which_int <- which_(list_item_is_interval(data))
  for (i in seq_along(which_int)){
    data[[.subset2(which_int, i)]] <- group_id(.subset2(data, .subset2(which_int, i)), order = order)
  }
  data
}
interval_separate <- function(x){
  list(start = interval_start(x),
       end = interval_end(x))
}

group_id_to_qg <- function(x,
                           n_groups = NULL,
                           group_starts = NULL,
                           group_sizes = NULL,
                           ordered = FALSE,
                           set = FALSE){
  if (is.null(n_groups)){
    n_groups <- collapse::fnunique(x)
  }
  x <- add_attr(x, "N.groups", n_groups, set = set)
  if (!is.null(group_starts)){
    x <- add_attr(x, "starts", group_starts, set = set)
  }
  if (!is.null(group_sizes)){
    x <- add_attr(x, "group.sizes", group_sizes, set = set)
  }
  if (ordered){
    x <- add_attr(x, "class", c("qG", "ordered", "na.included"), set = set)
  } else {
    x <- add_attr(x, "class", c("qG", "na.included"), set = set)
  }
  x
}
# Efficiently convert qG to integer
qg_to_integer <- function(x, set = FALSE){
  strip_attrs(x, set = set)
}
# Cool alternative to qG() because S3 method for group_id() kicks in
quick_group <- function(x, ..., order = TRUE, ascending = TRUE){
  # if (na_exclude){
  #   out <- rep_len(NA_integer_, vec_length(x))
  #   not_na <- cheapr::which_not_na(x)
  #   group_ids <- group_id(cheapr::sset(x, not_na), ..., order = order, as_qg = TRUE, ascending = ascending)
  #   out[not_na] <- group_ids
  #   out <- group_id_to_qg(out, n_groups = attr(group_ids, "N.groups"),
  #                         # group_starts = attr(group_ids, "starts"),
  #                         group_sizes = attr(group_ids, "group.sizes"))
  #
  # } else {
  #   out <- group_id(x, ..., order = order, as_qg = TRUE, ascending = ascending)
  # }
  # out
  group_id(x, ..., order = order, as_qg = TRUE, ascending = ascending)
}
