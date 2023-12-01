#' Fast grouped row numbers
#'
#' @description
#' Very fast row numbers by group.
#'
#' @param x A vector, data frame or `GRP` object.
#' @param ascending When `ascending = TRUE` the row IDs are in
#' increasing order. When `ascending = FALSE` the row IDs are in
#' decreasing order.
#'
#' @details
#' `frowid()` is like `data.table::rowid()` but uses
#' an alternative method for calculating row numbers.
#' When `x` is a collapse `GRP` object, it is considerably faster.
#' It is also faster for character vectors.
#'
#' @returns
#' An integer vector of row IDs.
#'
#' @seealso [row_id] [add_row_id]
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(data.table)
#' library(nycflights13)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # Simple row numbers
#' head(row_id(flights))
#' # Row numbers by origin
#' head(frowid(flights$origin))
#' head(row_id(flights, origin))
#'
#' # Fast duplicate rows
#' head(frowid(flights) > 1)
#'
#' # With data frames, better to use row_id()
#' flights %>%
#'   add_row_id() %>% # Plain row ids
#'   add_row_id(origin, dest, .name = "grouped_row_id") # Row IDs by group
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname frowid
#' @export
frowid <- function(x, ascending = TRUE){
  if (is_GRP(x)){
    GRP_row_id(x, ascending = ascending)
  } else {
    grouped_row_id(x, ascending = ascending)
  }
}
