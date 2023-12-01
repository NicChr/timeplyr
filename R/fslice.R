#' Faster `dplyr::slice()`
#'
#' @description When there are lots of groups, the `fslice()` functions are much faster.
#'
#' @details
#' `fslice()` and friends allow for more flexibility in how you order the by-group slicing. \cr
#' Furthermore, you can control whether the returned data frame is sliced in
#' the order of the supplied row indices, or whether the
#' original order is retained (like `dplyr::filter()`).
#'
#' `fslice_head()` and `fslice_tail()` are very fast with large numbers of groups.
#'
#' `fslice_sample()` is arguably more intuitive as it by default
#' resamples each entire group without replacement, without having to specify a
#' maximum group size like in `dplyr::slice_sample()`.
#'
#' @param data Data frame
#' @param ... See `?dplyr::slice` for details.
#' @param sort_groups If `TRUE` (the default) the by-group slices will be
#' done in order of the sorted groups.
#' If `FALSE` the group order is determined by first-appearance in the data.
#' @param keep_order Should the sliced data frame be returned in its original order?
#' The default is `FALSE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param n Number of rows.
#' @param prop Proportion of rows.
#' @param order_by Variables to order by.
#' @param with_ties Should ties be kept together? The default is `TRUE`.
#' @param na_rm Should missing values in `fslice_max()` and `fslice_min()` be removed?
#' The default is `FALSE`.
#' @param replace Should `fslice_sample()` sample with or without replacement?
#' Default is `FALSE`, without replacement.
#' @param weights Probability weights used in `fslice_sample()`.
#' @param seed Seed number defining RNG state.
#' If supplied, this is only applied \bold{locally} within the function
#' and the seed state isn't retained after sampling.
#' To clarify, whatever seed state was in place before the function call,
#' is restored to ensure seed continuity.
#' If left `NULL` (the default), then the seed is never modified.
#'
#' @returns
#' A `data.frame` of specified rows.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(nycflights13)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' flights <- flights %>%
#'   group_by(origin, dest)
#'
#' # First row repeated for each group
#' flights %>%
#'   fslice(1, 1)
#' # First row per group
#' flights %>%
#'   fslice_head(n = 1)
#' # Last row per group
#' flights %>%
#'   fslice_tail(n = 1)
#' # Earliest flight per group
#' flights %>%
#'   fslice_min(time_hour, with_ties = FALSE)
#' # Last flight per group
#' flights %>%
#'   fslice_max(time_hour, with_ties = FALSE)
#' # Random sample without replacement by group
#' # (or stratified random sampling)
#' flights %>%
#'   fslice_sample()
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname fslice
#' @export
fslice <- function(data, ..., .by = NULL,
                   keep_order = FALSE, sort_groups = TRUE){
  dots <- list(...)
  N <- df_nrow(data)
  n <- unlist(dots, recursive = TRUE, use.names = FALSE)
  if (length(n) == 0L) n <- 0L
  n_rng <- collapse::frange(n)
  sum_n_rng <- sum(n_rng)
  if (abs(sum_n_rng) != sum(abs(n_rng))){
    stop("Can't mix negative and positive locations")
  }
  # range_sign <- sign(sum(sign(1/n_rng))) # This can deal with -0
  range_sign <- sign(sum_n_rng)
  n <- as.integer(n)
  # Groups
  group_vars <- get_groups(data, .by = {{ .by }})
  if (length(group_vars) == 0L){
    if (any(abs(n_rng) > N)){
      i <- n[cpp_which(data.table::between(n, -N, N))]
    } else {
      i <- n
    }
  } else {
    group_df <- group_collapse(data, .by = {{ .by }},
                               order = sort_groups, sort = sort_groups,
                               id = FALSE, loc = TRUE,
                               # loc_order = FALSE,
                               size = TRUE, start = FALSE, end = FALSE)
    # Constrain n to <= max GRPN
    GN <-  max(group_df[[".size"]])
    n <- n[cpp_which(data.table::between(n, -GN, GN))]
    rows <- group_df[[".loc"]]
    row_lens <- group_df[[".size"]]
    if (range_sign >= 1){
      size <- pmin.int(max(n), row_lens)
    } else {
      size <- pmax.int(0L, row_lens - max(abs(n)))
    }
    keep <- cpp_which(size > 0)
    if (length(rows) - length(keep) > 0L){
      rows <- rows[keep]
      row_lens <- row_lens[keep]
      size <- size[keep]
    }
    i <- vector("list", length(rows))
    for (j in seq_along(i)){
      i[[j]] <- .subset(.subset2(rows, j),
                        .subset(n, cpp_which(n <= .subset2(row_lens, j))))
    }
    i <- unlist(i, use.names = FALSE, recursive = FALSE)
    if (is.null(i)){
      i <- integer(0)
    }
  }
  if (keep_order){
    i <- conditional_sort(i)
  }
  df_row_slice(data, i)
}
#' @rdname fslice
#' @export
fslice_head <- function(data, ..., n, prop, .by = NULL,
                        keep_order = FALSE, sort_groups = TRUE){
  rlang::check_dots_empty0(...)
  slice_info <- df_slice_prepare(data, n, prop,
                                 .by = {{ .by }},
                                 sort_groups = sort_groups,
                                 default_n = 1L)
  group_sizes <- slice_info[["group_sizes"]]
  slice_sizes <- slice_info[["slice_sizes"]]
  # Start indices of sequences
  start <- calc_sorted_group_starts(group_sizes)
  # Vectorised sequences
  sequences <- sequence(slice_sizes, from = start, by = 1L)
  if (length(slice_sizes) > 1L){
    i <- unlist(slice_info[["rows"]], recursive = FALSE, use.names = FALSE)[sequences]
  } else {
    i <- sequences
  }
  if (keep_order){
    i <- conditional_sort(i)
  }
  df_row_slice(data, i)
}
#' @rdname fslice
#' @export
fslice_tail <- function(data, ..., n, prop, .by = NULL,
                        keep_order = FALSE, sort_groups = TRUE){
  rlang::check_dots_empty0(...)
  slice_info <- df_slice_prepare(data, n, prop,
                                 .by = {{ .by }},
                                 sort_groups = sort_groups,
                                 default_n = 1L)
  group_sizes <- slice_info[["group_sizes"]]
  slice_sizes <- slice_info[["slice_sizes"]]
  start <- calc_sorted_group_ends(group_sizes)
  sequences <- sequence(slice_sizes, from = start - slice_sizes + 1L, by = 1L)
  if (length(slice_sizes) > 1L){
    i <- unlist(slice_info[["rows"]], recursive = FALSE, use.names = FALSE)[sequences]
  } else {
    i <- sequences
  }
  if (keep_order){
    i <- conditional_sort(i)
  }
  df_row_slice(data, i)
}
#' @rdname fslice
#' @export
fslice_min <- function(data, order_by, ..., n, prop, .by = NULL,
                       with_ties = TRUE, na_rm = FALSE,
                       keep_order = FALSE, sort_groups = TRUE){
  rlang::check_dots_empty0(...)
  group_vars <- get_groups(data, .by = {{ .by }})
  grp_nm1 <- new_var_nm(names(data), "g")
  out <- safe_ungroup(data)
  g1 <- group_id(data, .by = {{ .by }}, order = sort_groups)
  out[[grp_nm1]] <- g1
  out <- mutate2(out,
                 !!enquo(order_by),
                 .keep = "none",
                 .by = all_of(grp_nm1))
  order_by_nm <- tidy_transform_names(data, !!enquo(order_by))
  row_nm <- new_var_nm(names(out), "row_id")
  out[[row_nm]] <- df_seq_along(out)
  g2 <- group_id(out[[order_by_nm]])
  # Order by Groups + desc order by var
  grp_nm <- new_var_nm(names(out), "g")
  if (length(group_vars) == 0){
    out[[grp_nm]] <- g2
  } else {
    out[[grp_nm]] <- group_id(list(g1, g2))
  }
  out <- farrange(out, .cols = grp_nm)
  out1 <- fslice_head(out, n = n, prop = prop, .by = all_of(grp_nm1),
                      sort_groups = sort_groups)
  if (with_ties){
    i <- out[[row_nm]][cpp_which(out[[grp_nm]] %in% out1[[grp_nm]])]
  } else {
    i <- out1[[row_nm]]
  }
  if (na_rm){
    i2 <- out[[row_nm]][cpp_which(is.na(out[[order_by_nm]]))]
    i <- setdiff(i, i2)
  }
  if (is.null(i)){
    i <- integer(0)
  }
  if (keep_order){
    i <- conditional_sort(i)
  }
  df_row_slice(data, i)
}
#' @rdname fslice
#' @export
fslice_max <- function(data, order_by, ..., n, prop, .by = NULL,
                       with_ties = TRUE, na_rm = FALSE,
                       keep_order = FALSE, sort_groups = TRUE){
  rlang::check_dots_empty0(...)
  group_vars <- get_groups(data, .by = {{ .by }})
  grp_nm1 <- new_var_nm(names(data), "g")
  out <- safe_ungroup(data)
  g1 <- group_id(data, .by = {{ .by }}, order = sort_groups)
  out[[grp_nm1]] <- g1
  out <- mutate2(out,
                 !!enquo(order_by),
                 .keep = "none",
                 .by = all_of(grp_nm1))
  order_by_nm <- tidy_transform_names(data, !!enquo(order_by))
  row_nm <- new_var_nm(names(out), "row_id")
  out[[row_nm]] <- df_seq_along(out)
  g2 <- group_id(out[[order_by_nm]], ascending = FALSE)
  # Order by Groups + desc order by var
  grp_nm <- new_var_nm(names(out), "g")
  if (length(group_vars) == 0){
    out[[grp_nm]] <- g2
  } else {
    out[[grp_nm]] <- group_id(list(g1, g2))
  }
  out <- farrange(out, .cols = grp_nm)
  out1 <- fslice_head(out, n = n, prop = prop, .by = all_of(grp_nm1),
                      sort_groups = sort_groups)
  if (with_ties){
    i <- out[[row_nm]][cpp_which(out[[grp_nm]] %in% out1[[grp_nm]])]
  } else {
    i <- out1[[row_nm]]
  }
  if (na_rm){
    i2 <- out[[row_nm]][cpp_which(is.na(out[[order_by_nm]]))]
    i <- setdiff(i, i2)
  }
  if (is.null(i)){
    i <- integer(0)
  }
  if (keep_order){
    i <- conditional_sort(i)
  }
  df_row_slice(data, i)
}
#' @rdname fslice
#' @export
fslice_sample <- function(data, n, replace = FALSE, prop,
                          .by = NULL,
                          keep_order = FALSE, sort_groups = TRUE,
                          weights = NULL, seed = NULL){
  # Check if a seed already exists in global environment
  seed_exists <- exists(".Random.seed")
  # Save it in the first instance
  if (seed_exists){
    old <- .Random.seed
  }
  # Does user want to use local seed?
  seed_is_null <- is.null(seed)
  has_weights <- !rlang::quo_is_null(enquo(weights))
  if (has_weights){
    data <- mutate2(data, !!enquo(weights))
    weights_var <- tidy_transform_names(data, !!enquo(weights))
  }
  slice_info <- df_slice_prepare(data, n, prop,
                                 .by = {{ .by }},
                                 sort_groups = sort_groups,
                                 bound_n = (missing(n) && missing(prop)) || !replace,
                                 default_n = df_nrow(data))
  group_sizes <- slice_info[["group_sizes"]]
  slice_sizes <- slice_info[["slice_sizes"]]
  rows <- vector("list", length(slice_info[["rows"]]))
  if (has_weights){
    g <- group_id(data, .by = {{ .by }}, order = sort_groups)
    weights <- collapse::gsplit(data[[weights_var]], g = g)
  } else {
    weights <- NULL
  }
  # If user wants to use local seed
  # We must first save the current seed
  # Set the new seed
  # Discard the newly created seed after sampling
  # Restore the old seed (if there existed an old seed)
  if (!seed_is_null){
    set.seed(seed)
  }
  for (i in seq_along(rows)){
    rows[[i]] <- sample.int(.subset2(group_sizes, i),
                            size = .subset2(slice_sizes, i),
                            replace = replace,
                            prob = .subset2(weights, i))
  }
  if (seed_exists && !seed_is_null){
    on.exit({ assign(".Random.seed", old, envir = globalenv())})
  } else if (!seed_is_null){
    on.exit({remove(".Random.seed", envir = globalenv())})
  }
  rows <- unlist(rows, use.names = FALSE, recursive = FALSE)
  if (length(rows) > 0L){
    rows <- rows + rep.int(c(0L, cumsum(group_sizes)[-length(group_sizes)]),
                           times = slice_sizes)
  }
  i <- unlist(slice_info[["rows"]], use.names = FALSE, recursive = FALSE)[rows]
  if (is.null(i)){
    i <- integer()
  }
  if (keep_order){
    i <- conditional_sort(i)
  }
  df_row_slice(data, i)
}
df_slice_prepare <- function(data, n, prop, .by = NULL, sort_groups = TRUE,
                             bound_n = TRUE, default_n = 1L){
  N <- df_nrow(data)
  missing_n <- missing(n)
  missing_prop <- missing(prop)
  if (!missing_n && !missing_prop){
    stop("Either n or prop must be supplied, not both.")
  }
  if (missing_n && missing_prop){
    n <- default_n
    type <- "n"
  }
  if (!missing_n && missing_prop){
    check_length(n, 1L)
    type <- "n"
  }
  if (missing_n && !missing_prop){
    check_length(prop, 1L)
    type <- "prop"
  }

  group_df <- group_collapse(data, .by = {{ .by }},
                             order = sort_groups, sort = sort_groups,
                             id = FALSE, loc = TRUE,
                             # loc_order = FALSE,
                             size = TRUE, start = FALSE, end = FALSE)
  rows <- group_df[[".loc"]]
  group_sizes <- group_df[[".size"]]
  if (type == "n"){
    # USING N
    if (bound_n){
      GN <- collapse::fmax(group_sizes, use.g.names = FALSE, na.rm = FALSE)
      if (sign(1/n) >= 1){
        n <- as.integer(min(n, GN))
        slice_sizes <- pmin.int(n, group_sizes)
      } else {
        n <- as.integer(max(n, -GN))
        slice_sizes <- pmax.int(0L, group_sizes + n)
      }
    } else {
      slice_sizes <- rep_len(as.integer(n), length(rows))
    }
  } else {
    # USING prop
    if (bound_n){
      if (sign(1/prop) >= 1){
        prop <- min(1, prop)
        slice_sizes <- floor(prop * group_sizes)
      } else {
        prop <- max(-1, prop)
        slice_sizes <- ceiling( (1 + prop) * group_sizes)
      }
    } else {
      slice_sizes <- prop * group_sizes
    }
    slice_sizes <- as.integer(slice_sizes)
  }
  keep <- cpp_which(slice_sizes > 0)
  if (length(rows) - length(keep) > 0L){
    rows <- rows[keep]
    group_sizes <- group_sizes[keep]
    slice_sizes <- slice_sizes[keep]
  }
  list("rows" = rows,
       "group_sizes" = group_sizes,
       "slice_sizes" = slice_sizes)
}
# slice_info <- function(n, prop, default_n = 1L){
#   missing_n <- missing(n)
#   missing_prop <- missing(prop)
#   if (!missing_n && !missing_prop){
#     stop("Either n or prop must be supplied, not both.")
#   }
#   if (missing_n && missing_prop){
#     n <- default_n
#     type <- "n"
#     prop <- numeric(0)
#   }
#   if (!missing_n && missing_prop){
#     stopifnot(length(n) == 1L)
#     type <- "n"
#     prop <- numeric(0)
#   }
#   if (missing_n && !missing_prop){
#     stopifnot(length(prop) == 1L)
#     type <- "prop"
#     n <- integer(0)
#   }
#   list("n" = n,
#        "prop" = prop,
#        "type" = type)
# }
