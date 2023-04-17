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
#' @param replace Should `slice_sample()` sample with or without replacement?
#' Default is `FALSE`, without replacement.
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
fslice_head <- function(data, ..., n = 1, .by = NULL,
                        keep_order = FALSE, sort_groups = TRUE){
  rlang::check_dots_empty0(...)
  N <- nrow2(data)
  slice_info <- df_slice_prepare(data, n = n,
                                 .by = {{ .by }},
                                 sort_groups = sort_groups)
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
fslice_tail <- function(data, ..., n = 1, .by = NULL,
                        keep_order = FALSE, sort_groups = TRUE){
  rlang::check_dots_empty0(...)
  N <- nrow2(data)
  slice_info <- df_slice_prepare(data, n = n,
                                 .by = {{ .by }},
                                 sort_groups = sort_groups)
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
fslice_sample <- function(data, ..., n, .by = NULL,
                          keep_order = FALSE, sort_groups = TRUE,
                          replace = FALSE, seed = NULL){
  rlang::check_dots_empty0(...)
  N <- nrow2(data)
  if (missing(n)){
    n <- N
  }
  # if (!rlang::quo_is_null(enquo(prob))){
  #   data <- dplyr::mutate(data, !!enquo(prob),
  #                         .by = {{ .by }})
  # }
  slice_info <- df_slice_prepare(data, n = n,
                                 .by = {{ .by }},
                                 sort_groups = sort_groups)
  seed_exists <- exists(".Random.seed")
  seed_is_null <- is.null(seed)
  if (!seed_is_null){
    if (seed_exists){
      old <- .Random.seed
    }
    set.seed(seed)
  }
  rows <- purrr::map2(slice_info[["rows"]],
                      slice_info[["slice_sizes"]],
                      ~ sample2(.x, size = .y, replace = replace))
  if (seed_exists && !seed_is_null){
    .Random.seed <<- old
  } else if (!seed_is_null){
    remove(.Random.seed, envir = .GlobalEnv)
  }
  i <- unlist(rows, use.names = FALSE, recursive = FALSE)
  if (is.null(i)){
    i <- integer(0)
  }
  if (keep_order){
    i <- radix_sort(i)
  }
  df_row_slice(data, i)
}
df_slice_prepare <- function(data, n, .by = NULL, sort_groups = TRUE){
  N <- nrow2(data)
  stopifnot(length(n) == 1L)
  group_df <- group_collapse(data, .by = {{ .by }},
                             order = sort_groups, sort = sort_groups,
                             loc = TRUE,
                             # loc_order = FALSE,
                             size = TRUE, start = FALSE, end = FALSE)
  rows <- group_df[[".loc"]]
  group_sizes <- group_df[[".size"]]
  GN <- max(group_sizes)
  if (n >= 0){
    n <- as.integer(min(n, GN))
    slice_sizes <- pmin(n, group_sizes)
  } else {
    n <- as.integer(max(n, -GN))
    slice_sizes <- pmax(0L, group_sizes + n)
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
