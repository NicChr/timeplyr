#' Much faster version of `dplyr::slice()` when there are lots of groups.
#'
#' @param data Data frame
#' @param ... See `?dplyr::slice` for details.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
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
#' @export
fslice <- function(data, ..., .by = NULL){
  dots <- list(...)
  n <- unlist(dots, recursive = TRUE, use.names = FALSE)
  if (length(n) == 0L) n <- integer(0)
  range_sign <- sign(c(collapse::fmin(n), collapse::fmax(n)))
  if (sum(abs(range_sign)) != abs(sum(range_sign))){
    stop("Can't mix negative and positive locations")
  }
  group_vars <- get_groups(data, .by = {{ .by }})
  if (length(group_vars) == 0L){
    g <- NULL
  } else {
    g <- group_id(data, .by = {{ .by }},
                  sort = TRUE, as_qg = FALSE)
  }
    rowids <- gseq_len(nrow2(data), g = g)
    if (sum(range_sign) < 0){
      i <- which(!rowids %in% abs(n))
    } else {
      i <- which(rowids %in% n)
    }
  data[i, , drop = FALSE]
}
fslice_head <- function(data, ..., n = 1, .by = NULL){
  rlang::check_dots_empty0(...)
  stopifnot(length(n) == 1L)
  N <- nrow2(data)
  if (n >= 0){
    n <- bound_to(n, N)
    n_slice <- seq_len(n)
    out <- fslice(data, n_slice, .by = {{ .by }})
  }
  if (n < 0) stop("n < 0 is currently not supported")
  out
  # if (n < 0){
  #   n <- bound_from(n, -N)
  #   group_vars <- get_groups(data, .by = {{ .by }})
  #   if (length(group_vars) == 0L){
  #     g <- NULL
  #     n_slice <- seq_len(nrow2(data) - abs(n))
  #     out <- fslice(data, n_slice)
  #   } else {
  #     g <- group_id(data, .by = {{ .by }},
  #                   sort = TRUE, as_qg = FALSE)
  #   }
  # }
}
# fslice_head <- function(data, ..., n = 1, .by = NULL){
#   rlang::check_dots_empty0(...)
#   stopifnot(length(n) == 1L)
#   N <- nrow2(data)
#   if (n >= 0){
#     n <- bound_to(n, N)
#     n_slice <- seq_len(n)
#   }
#
#   if (n < 0){
#     n <- bound_from(n, -N)
#     n_slice <- seq_len(nrow2(data) - abs(n))
#   }
#   fslice(data, n_slice, .by = {{ .by }})
# }
# fslice_tail <- function(data, ..., n = 1, .by = NULL){
#   rlang::check_dots_empty0(...)
#   stopifnot(length(n) == 1L)
#   N <- nrow2(data)
#   if (n >= 0){
#     n <- min(n, N)
#     n_slice <- seq(from = N, length.out = n, by = -1)
#   }
#
#   if (n < 0){
#     n <- max(n, -N)
#     n_slice <- seq(from = N, length.out = N - abs(n),
#                        by = -1)
#   }
#   fslice(data, n_slice, .by = {{ .by }})
# }
