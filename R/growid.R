#' Fast grouped row numbers
#'
#' @description `growid()` is like `data.table::rowid()` but uses
#' `collapse` for the grouping and allows you to specify the
#' grouping structure. \cr
#' `gseq_len()`is the same but accepts similar arguments to `seq_len()` with
#' an additional group argument. \cr
#' Both produce the same thing, namely a running integer sequence for
#' each group that increments by 1 and starts at 1. \cr
#' This is most useful for calculating row numbers. \cr
#' @param x A vector or data frame.
#' @param length Sequence length.
#' @param g Group IDs passed directly to `collapse::GRP()`.
#' This can be a vector, list or data frame.
#' @param ascending When `ascending = TRUE` the row IDs are in
#' increasing order. When `ascending = FALSE` the row IDs are in
#' decreasing order.
#'
#' @return An integer vector of row IDs
#' or double if `length > .Machine$integer.max`
#' If `x` is a vector, a vector `length(x)` will be returned.\cr
#' If `x` is a data frame, a vector `nrow(x)` will be returned.\cr
#' If `x` is a list, a vector `unique(lengths(x))` will be returned as
#' long as the number of unique lengths is `<= 1`.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(data.table)
#' library(nycflights13)
#'
#' # Simple row numbers
#' growid(flights, g = NULL)
#' # Row numbers by origin
#' growid(flights, g = flights$origin)
#'
#' # Fast duplicate rows
#' growid(flights) > 1
#'
#' # On vectors, this is like base::seq_len()
#' growid(flights$year, g = NULL)
#'
#' # Comparison to rowidv()
#' all.equal(rowidv(flights),
#'           growid(flights))
#'
#' # Comparison to dplyr
#' flights %>%
#'   mutate(id1 = row_number(),
#'          .by = c(origin, dest)) %>%
#'   mutate(id2 = growid(pick(origin, dest))) %>%
#'   filter(id1 != id2)
#'
#' flights %>%
#'   mutate(id1 = row_number(),
#'          .by = c(origin, dest)) %>%
#'   add_group_id(origin, dest) %>%
#'   mutate(id2 = gseq_len(nrow(.), g = group_id)) %>%
#'   filter(id1 != id2)
#' @rdname growid
#' @export
growid <- function(x, g = x, ascending = TRUE){
  if (is.list(x)){
    if (is_df(x)){
      len <- nrow2(x)
    } else {
      stopifnot(is_list_df_like(x))
      len <- vec_head(collapse::vlengths(x, use.names = FALSE), n = 1L)
    }
    sort <- TRUE
  } else {
    len <- length(x)
    sort <- FALSE
  }
  if (is.null(g)){
    out <- seq_len(len)
    if (!ascending){
      out <- rev(out)
    }
  } else {
    o <- NULL
    g <- GRP2(g, sort = sort, call = FALSE, return.groups = FALSE,
              return.order = FALSE)
    if (!ascending){
      if (len > 0L){
        o <- seq(from = len, to = 1L, by = -1L)
      }
    }
    out <- collapse::fcumsum(seq_ones(len),
                             na.rm = FALSE,
                             check.o = FALSE,
                             o = o,
                             g = g)
  }
  out
}
#' @rdname growid
#' @export
gseq_len <- function(length, g = NULL){
  if (is.null(g)){
    seq_len(length)
  }  else {
    g <- GRP2(g, sort = TRUE,
              call = FALSE, return.groups = FALSE)
    collapse::fcumsum(seq_ones(length),
                      na.rm = FALSE,
                      g = g)
  }
}
