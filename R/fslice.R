#' Faster `dplyr::slice()`
#'
#' @description When there are lots of groups, the `fslice()` functions are much faster.
#'
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
#' @param na_rm Should missing values in `fslice_max()` and `fslice_min()` be kept?
#' The default is `FALSE`.
#' @param replace Should `fslice_sample()` sample with or without replacement?
#' Default is `FALSE`, without replacement.
#' @param weights Probability weights used in `fslice_sample()`.
#' @param seed Seed number defining RNG state.
#' If supplied, this is only applied locally within the function
#' and the seed state isn't retained after sampling.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(nycflights13)
#'
#' \dontrun{
#' library(microbenchmark)
#' data(flights)
#' # This may take a couple of minutes
#' microbenchmark(fslice(flights, 1:3, .by = c(origin, dest, tailnum)),
#'                slice(flights, 1:3, .by = c(origin, dest, tailnum)),
#'                times = 3)
#' microbenchmark(fslice(flights, 1:3, .by = everything()),
#'                slice(flights, 1:3, .by = everything()),
#'                times = 3)
#' }
#' @rdname fslice
#' @export
fslice <- function(data, ..., .by = NULL,
                   keep_order = FALSE, sort_groups = TRUE){
  dots <- list(...)
  N <- nrow2(data)
  n <- unlist(dots, recursive = TRUE, use.names = FALSE)
  if (length(n) == 0L) n <- 0L
  n <- as.integer(n)
  range_sign <- sign(collapse::frange(n, na.rm = FALSE))
  if (sum(abs(range_sign)) != abs(sum(range_sign))){
    stop("Can't mix negative and positive locations")
  }
  # Groups
  group_vars <- get_groups(data, .by = {{ .by }})
  if (length(group_vars) == 0L){
    g <- NULL
    i <- n[data.table::between(n, -N, N)]
  } else {
      group_df <- group_collapse(data, .by = {{ .by }},
                                 order = sort_groups, sort = sort_groups,
                                 loc = TRUE,
                                 # loc_order = FALSE,
                                 size = TRUE, start = FALSE, end = FALSE)
      # Constrain n to <= max GRPN
      GN <-  max(group_df[[".size"]])
      n <- n[data.table::between(n, -GN, GN)]
      rows <- group_df[[".loc"]]
      row_lens <- group_df[[".size"]]
      if (sum(range_sign) >= 0){
        size <- pmin(max(n), row_lens)
      } else {
        size <- pmax(0L, row_lens - max(abs(n)))
      }
      keep <- which(size > 0)
      if (length(rows) - length(keep) > 0L){
        rows <- rows[keep]
        row_lens <- row_lens[keep]
        size <- size[keep]
      }
      i <- unlist(lapply(rows, function(x) x[n]),
                  use.names = FALSE,
                  recursive = FALSE)
      i <- collapse::na_rm(i)
      if (is.null(i)){
        i <- integer(0)
      }
  }
  if (keep_order){
    i <- radix_sort(i)
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
  # Start indices of sequences
  start <- cumsum(c(1L, group_sizes[-length(group_sizes)]))
  # Vectorised sequences
  sequences <- sequence(slice_info[["slice_sizes"]], from = start, by = 1L)
  i <- unlist(slice_info[["rows"]], recursive = FALSE, use.names = FALSE)[sequences]
  if (is.null(i)){
    i <- integer(0)
  }
  if (keep_order){
    i <- radix_sort(i)
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
  slice_sizes <- slice_info[["slice_sizes"]]
  start <- cumsum(slice_info[["group_sizes"]])
  sequences <- sequence(slice_sizes, from = start - slice_sizes + 1L, by = 1L)
  i <- unlist(slice_info[["rows"]], use.names = FALSE, recursive = FALSE)[sequences]
  if (is.null(i)){
    i <- integer(0)
  }
  if (keep_order){
    i <- radix_sort(i)
  }
  df_row_slice(data, i)
}
#' @rdname fslice
#' @export
fslice_min <- function(data, order_by, ..., n, prop, .by = NULL,
                       with_ties = TRUE, na_rm = TRUE,
                       keep_order = FALSE, sort_groups = TRUE){
  rlang::check_dots_empty0(...)
  group_vars <- get_groups(data, .by = {{ .by }})
  out <- dplyr::mutate(data,
                       !!enquo(order_by),
                       .keep = "none",
                       .by = {{ .by }})
  order_by_nm <- tidy_transform_names(safe_ungroup(data),
                                      !!enquo(order_by))
  row_nm <- new_var_nm(names(out), "row_id")
  out[[row_nm]] <- seq_along(attr(out, "row.names"))
  grp_nm <- new_var_nm(names(out), "g")
  out[[grp_nm]] <- group_id(out, .by = {{ .by }},
                            order = sort_groups)
  grp_nm2 <- new_var_nm(names(out), "g")
  out[[grp_nm2]] <- group_id(out[[order_by_nm]], order = TRUE)
  # Order by Groups + desc order by var
  grp_nm3 <- new_var_nm(names(out), "g")
  out[[grp_nm3]] <- group_id(safe_ungroup(out), all_of(c(grp_nm, grp_nm2)), order = TRUE)
  out <- df_row_slice(out, radix_order(out[[grp_nm3]]), reconstruct = FALSE)

  out1 <- fslice_head(out, n = n, prop = prop, .by = {{ .by }},
                      sort_groups = sort_groups)
  if (with_ties){
    i <- out[[row_nm]][out[[grp_nm3]] %in% out1[[grp_nm3]]]
    # i <- df_row_slice(out, which(out[[grp_nm]] %in% out1[[grp_nm]]))[[row_nm]]
  } else {
    i <- out1[[row_nm]]
  }
  if (na_rm){
    i2 <- out[[row_nm]][is.na(out[[order_by_nm]])]
    i <- setdiff(i, i2)
  }
  if (is.null(i)){
    i <- integer(0)
  }
  if (keep_order){
    i <- radix_sort(i)
  }
  df_row_slice(data, i)
}
#' @rdname fslice
#' @export
fslice_max <- function(data, order_by, ..., n, prop, .by = NULL,
                       with_ties = TRUE, na_rm = TRUE,
                       keep_order = FALSE, sort_groups = TRUE){
  rlang::check_dots_empty0(...)
  group_vars <- get_groups(data, .by = {{ .by }})
  out <- dplyr::mutate(data,
                       !!enquo(order_by),
                       .keep = "none",
                       .by = {{ .by }})
  order_by_nm <- tidy_transform_names(safe_ungroup(data),
                                      !!enquo(order_by))
  row_nm <- new_var_nm(names(out), "row_id")
  out[[row_nm]] <- seq_along(attr(out, "row.names"))
  grp_nm <- new_var_nm(names(out), "g")
  out[[grp_nm]] <- group_id(out, .by = {{ .by }},
                            order = sort_groups)
  grp_nm2 <- new_var_nm(names(out), "g")
  out[[grp_nm2]] <- group_id(out[[order_by_nm]],
                             order = TRUE, ascending = FALSE)
  # Order by Groups + desc order by var
  grp_nm3 <- new_var_nm(names(out), "g")
  out[[grp_nm3]] <- group_id(safe_ungroup(out), all_of(c(grp_nm, grp_nm2)), order = TRUE)
  out <- df_row_slice(out, radix_order(out[[grp_nm3]]), reconstruct = FALSE)

  out1 <- fslice_head(out, n = n, prop = prop, .by = {{ .by }})
  if (with_ties){
    i <- out[[row_nm]][out[[grp_nm3]] %in% out1[[grp_nm3]]]
  } else {
    i <- out1[[row_nm]]
  }
  if (na_rm){
    i2 <- out[[row_nm]][is.na(out[[order_by_nm]])]
    i <- setdiff(i, i2)
  }
  if (is.null(i)){
    i <- integer(0)
  }
  if (keep_order){
    i <- radix_sort(i)
  }
  df_row_slice(data, i)
}
#' @rdname fslice
#' @export
fslice_sample <- function(data, ..., n, prop,
                          .by = NULL,
                          keep_order = FALSE, sort_groups = TRUE,
                          replace = FALSE, weights = NULL, seed = NULL){
  rlang::check_dots_empty0(...)
  has_weights <- !rlang::quo_is_null(enquo(weights))
  if (has_weights){
    data <- dplyr::mutate(data, !!enquo(weights))
    weights_var <- tidy_transform_names(safe_ungroup(data),
                                        !!enquo(weights))
  }
  slice_info <- df_slice_prepare(data, n, prop,
                                 .by = {{ .by }},
                                 sort_groups = sort_groups,
                                 bound_n = (missing(n) && missing(prop)) || !replace,
                                 default_n = nrow2(data))
  group_sizes <- slice_info[["group_sizes"]]
  slice_sizes <- slice_info[["slice_sizes"]]
  seed_exists <- exists(".Random.seed")
  seed_is_null <- is.null(seed)
  if (!seed_is_null){
    if (seed_exists){
      old <- .Random.seed
    }
    set.seed(seed)
  }
  # Pre-allocate a list with lengths = slice sizes
  rows <- vctrs::vec_chop(collapse::alloc(0L, sum(slice_sizes)),
                          sizes = slice_sizes)
  if (has_weights){
    g <- group_id(data, .by = {{ .by }}, order = sort_groups)
    weights <- collapse::gsplit(data[[weights_var]], g = g)
    for (i in seq_along(rows)){
      rows[[i]] <- sample.int(group_sizes[[i]],
                              size = slice_sizes[[i]],
                              replace = replace,
                              prob = weights[[i]])
    }
    # 2nd version
    # rows <- purrr::pmap(list(group_sizes,
    #                          slice_sizes,
    #                          weights),
    #                     function(x, y, z) sample.int(x, size = y,
    #                                                  replace = replace,
    #                                                  prob = z))
    # Original
    # rows <- purrr::pmap(list(slice_info[["rows"]], slice_info[["slice_sizes"]],
    #                          weights),
    #                     function(x, y, z) sample2(x, size = y, prob = z,
    #                                               replace = replace))
  } else {
    for (i in seq_along(rows)){
      rows[[i]] <- sample.int(group_sizes[[i]],
                              size = slice_sizes[[i]],
                              replace = replace)
    }
    # 2nd Version
    # rows <- purrr::map2(group_sizes,
    #                     slice_sizes,
    #                     ~ sample.int(.x, size = .y, replace = replace))
    # Original
    # rows <- purrr::map2(slice_info[["rows"]],
    #                     slice_info[["slice_sizes"]],
    #                     ~ sample2(.x, size = .y, replace = replace))
  }
  rows <- unlist(rows, use.names = FALSE, recursive = FALSE)
  if (length(rows) > 0L){
    rows <- rows + rep.int(c(0L, cumsum(group_sizes)[-length(group_sizes)]),
                           times = slice_sizes)
  }
  i <- unlist(slice_info[["rows"]], use.names = FALSE, recursive = FALSE)[rows]
  if (seed_exists && !seed_is_null){
    .Random.seed <<- old
  } else if (!seed_is_null){
    remove(.Random.seed, envir = .GlobalEnv)
  }
  if (is.null(i)){
    i <- integer(0)
  }
  if (keep_order){
    i <- radix_sort(i)
  }
  df_row_slice(data, i)
}
df_slice_prepare <- function(data, n, prop, .by = NULL, sort_groups = TRUE,
                             bound_n = TRUE, default_n = 1L){
  N <- nrow2(data)
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
    stopifnot(length(n) == 1L)
    type <- "n"
  }
  if (missing_n && !missing_prop){
    stopifnot(length(prop) == 1L)
    type <- "prop"
  }

  group_df <- group_collapse(data, .by = {{ .by }},
                             order = sort_groups, sort = sort_groups,
                             loc = TRUE,
                             # loc_order = FALSE,
                             size = TRUE, start = FALSE, end = FALSE)
  rows <- group_df[[".loc"]]
  group_sizes <- group_df[[".size"]]
  if (type == "n"){
    # USING N
    n <- as.integer(n)
    if (bound_n){
      GN <- collapse::fmax(group_sizes, use.g.names = FALSE, na.rm = FALSE)
      if (n >= 0){
        n <- min(n, GN)
        slice_sizes <- pmin(n, group_sizes)
      } else {
        n <- max(n, -GN)
        slice_sizes <- pmax(0L, group_sizes + n)
      }
    } else {
      slice_sizes <- rep_len(n, length(rows))
    }
  } else {
    # USING prop
    if (bound_n){
      if (prop >= 0){
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
  keep <- which(slice_sizes > 0)
  if (length(rows) - length(keep) > 0L){
    rows <- rows[keep]
    group_sizes <- group_sizes[keep]
    slice_sizes <- slice_sizes[keep]
  }
  list("rows" = rows,
       "group_sizes" = group_sizes,
       "slice_sizes" = slice_sizes)
}
slice_info <- function(n, prop, default_n = 1L){
  missing_n <- missing(n)
  missing_prop <- missing(prop)
  if (!missing_n && !missing_prop){
    stop("Either n or prop must be supplied, not both.")
  }
  if (missing_n && missing_prop){
    n <- default_n
    type <- "n"
    prop <- numeric(0)
  }
  if (!missing_n && missing_prop){
    stopifnot(length(n) == 1L)
    type <- "n"
    prop <- numeric(0)
  }
  if (missing_n && !missing_prop){
    stopifnot(length(prop) == 1L)
    type <- "prop"
    n <- integer(0)
  }
  list("n" = n,
       "prop" = prop,
       "type" = type)
}
