#' Fast grouped row numbers
#'
#' @description
#' `frowid()` is like `data.table::rowid()` but uses
#' `collapse` for the grouping.
#'
#' For a more  tidyverse friendly version for data frames, see `?row_id`.
#'
#' @param x A vector or data frame.
#' @param g (Optional) Group IDs passed directly to `collapse::GRP()`.
#' This can be a vector, list or data frame. \cr
#' To specify no groups, set `g = NULL`. \cr
#' If g is not supplied, the unique groups of x are used (the default).
#' @param ascending When `ascending = TRUE` the row IDs are in
#' increasing order. When `ascending = FALSE` the row IDs are in
#' decreasing order.
#' @param order Should the groups treated as ordered groups?
#' This makes no difference on the result but can sometimes be faster for
#' unsorted vectors.
#'
#' @returns
#' An integer vector of row IDs
#' or double if `length > .Machine$integer.max`
#' If `x` is a vector, a vector `length(x)` will be returned.\cr
#' If `x` is a data frame, a vector `nrow(x)` will be returned.\cr
#' If `x` is a list, a vector `unique(lengths(x))` will be returned as
#' long as the number of unique lengths is `<= 1`.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(data.table)
#' library(nycflights13)
#' \dontshow{
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # Simple row numbers
#' frowid(flights, g = NULL)
#' # Row numbers by origin
#' frowid(flights, g = flights$origin)
#'
#' # Fast duplicate rows
#' frowid(flights) > 1
#'
#' # On vectors, this is like base::seq_len()
#' frowid(flights$year, g = NULL)
#'
#' # Comparison to rowidv()
#' \dontrun{
#'   bench::mark(rowidv(flights),
#'               frowid(flights))
#' }
#'
#' # Comparison to dplyr
#' flights %>%
#'   mutate(id1 = row_number(),
#'          .by = c(origin, dest)) %>%
#'   mutate(id2 = frowid(pick(origin, dest))) %>%
#'   filter(id1 != id2)
#' @rdname frowid
#' @export
frowid <- function(x, g, ascending = TRUE, order = TRUE){
  len <- vec_length(x)
  if (missing(g)){
    g <- GRP2(x, sort = order, call = FALSE, return.groups = FALSE,
              return.order = TRUE)
  }
  if (is.null(g)){
    out <- seq_len(len)
    if (!ascending){
      out <- rev(out)
    }
  } else {
    g <- GRP2(g, sort = order, call = FALSE, return.groups = FALSE,
              return.order = TRUE)
    # If groups are sorted we can use sequence()
    if (GRP_is_sorted(g)){
      seq_sizes <- GRP_group_sizes(g)
      if (ascending){
        start <- 1L
        every <- 1L
      } else {
        start <- seq_sizes
        every <- -1L
      }
      out <- sequence2(seq_sizes, from = start, by = every)
    } else {
      o <- NULL
      if (!ascending){
        o <- seq.int(from = len, to = min(1L, len), by = -1L)
      }
      out <- fcumsum(seq_ones(len),
                     na.rm = FALSE,
                     check.o = FALSE,
                     o = o,
                     g = g)
    }
  }
  out
}
# Alternate version that uses re-ordering instead of cumulative sums
# Still a work-in-progress
# frowid <- function(x, g, ascending = TRUE, order = TRUE){
#   len <- vec_length(x)
#   if (missing(g)){
#     g <- GRP2(x, sort = order, call = FALSE, return.groups = FALSE,
#               return.order = TRUE)
#   }
#   if (is.null(g)){
#     out <- seq_len(len)
#     if (!ascending){
#       out <- rev(out)
#     }
#   } else {
#     o <- NULL
#     g <- GRP2(g, sort = order, call = FALSE, return.groups = FALSE,
#               return.order = TRUE)
#       if (ascending){
#         out <- sequence2(GRP_group_sizes(g))
#       } else {
#         out <- sequence2(GRP_group_sizes(g),
#                          from = GRP_group_sizes(g),
#                          by = -1L)
#       }
#     if (!GRP_is_sorted(g)){
#       out <- collapse::greorder(out, g = g)
#     }
#   }
#   out
# }
