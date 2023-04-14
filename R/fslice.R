#' Faster `dplyr::slice()`/`dplyr::slice_head()`
#'
#' @description When there are lots of groups, `fslice()` and `fslice_head()` are much faster than
#' the dplyr equivalents.
#'
#' Unlike `dplyr`, `fslice()` and `fslice_head()` always return a sliced data frame in the same
#' order it was given, similar to how `filter()` works.
#'
#' @param data Data frame
#' @param ... See `?dplyr::slice` for details.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param n Number of rows.
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
fslice <- function(data, ..., .by = NULL){
  dots <- list(...)
  n <- unlist(dots, recursive = TRUE, use.names = FALSE)
  if (length(n) == 0L) n <- integer(0)
  range_sign <- sign(c(collapse::fmin(n), collapse::fmax(n)))
  if (sum(abs(range_sign)) != abs(sum(range_sign))){
    stop("Can't mix negative and positive locations")
  }
  # Groups
  group_vars <- get_groups(data, .by = {{ .by }})
  if (length(group_vars) == 0L){
    g <- NULL
  } else {
    g <- group_id(data, .by = {{ .by }},
                  order = TRUE, as_qg = FALSE)
  }
  # Grouped row IDs
  rowids <- gseq_len(nrow2(data), g = g)
  if (sum(range_sign) < 0){
    i <- which(!rowids %in% abs(n))
  } else {
    i <- which(rowids %in% n)
  }
  data[i, , drop = FALSE]
}
#' @rdname fslice
#' @export
fslice_head <- function(data, ..., n = 1, .by = NULL){
  rlang::check_dots_empty0(...)
  stopifnot(length(n) == 1L)
  n <- as.integer(n)
  N <- nrow2(data)
  if (n >= 0){
    n <- bound_to(n, N)
    n_slice <- seq_len(n)
    out <- fslice(data, n_slice, .by = {{ .by }})
  }
  if (n < 0){
    n <- bound_from(n, -N)
    group_vars <- get_groups(data, .by = {{ .by }})
    if (length(group_vars) == 0L){
      g <- NULL
      n_slice <- seq_len(nrow2(data) - abs(n))
      out <- fslice(data, n_slice)
    } else {
      # g <- group_id(data, .by = {{ .by }}, order = TRUE)
      # g <- collapse::qG(g, sort = FALSE) # Unsorted group IDs
      # rowids <- seq_along(attr(data, "row.names"))
      # rows <- collapse::gsplit(x = rowids, g = g)
      rows <- group_loc(data, .by = {{ .by }}, order = TRUE, sort = FALSE)[[".rows"]]
      row_lens <- collapse::vlengths(rows, use.names = FALSE)
      size <- pmax(0L, row_lens + n)
      keep <- which(size > 0)
      rows <- rows[keep]
      row_lens <- row_lens[keep]
      size <- size[keep]
      start <- cumsum(c(1L, row_lens[-length(row_lens)]))
      sequences <- sequence(size, from = start, by = 1L)
      # Alternate method
      # for (i in seq_along(rows)){
      #   length(rows[[i]]) <- size[[i]]
      # }
      # i <- unlist(rows, use.names = FALSE, recursive = FALSE)
      i <- unlist(rows, recursive = FALSE, use.names = FALSE)[sequences]
      if (is.null(i)){
        i <- integer(0)
      }
      i <- radix_sort(i)
      out <- data[i, , drop = FALSE]
    }
  }
  out
}
# fslice_tail <- function(data, ..., n = 1, .by = NULL){
#   rlang::check_dots_empty0(...)
#   stopifnot(length(n) == 1L)
#   n <- as.integer(n)
#   N <- nrow2(data)
#   rows <- group_loc(data, .by = {{ .by }}, sort = TRUE, order = FALSE)[[".rows"]]
#   row_lens <- collapse::vlengths(rows, use.names = FALSE)
#   if (n >= 0){
#     size <- pmin(row_lens, n)
#   } else {
#     size <- pmax(0, row_lens - abs(n))
#   }
#   start <- pmax(0L, cumsum(c(1L, row_lens[-length(row_lens)])) - abs(n) + 1L)
#   keep <- which(abs(size) > 0)
#   rows <- rows[keep]
#   row_lens <- row_lens[keep]
#   start <- start[keep]
#   size <- size[keep]
#   # for (i in seq_along(rows)){
#   #   rows[[i]] <- sequence(abs(size[[i]]),
#   #                         from = abs(start[[i]]),
#   #                         by = 1L * sign(n))
#   # }
#   sequences <- sequence(size, from = start, by = 1L * sign(n))
#   i <- unlist(rows, use.names = FALSE, recursive = FALSE)[sequences]
#   # sequences <- sequence(abs(size), from = start, by = 1L * sign(size))
#   # i <- unlist(rows, recursive = FALSE, use.names = FALSE)[sequences]
#   if (is.null(i)){
#     i <- integer(0)
#   }
#   i <- radix_sort(i)
#   out <- data[i, , drop = FALSE]
#   out
# }
