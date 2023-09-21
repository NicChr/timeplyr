#' Vectorised sequence with ID
#'
#' @description An efficient method of calculating multiple sequences
#' with unique IDs for each sequence.
#'
#' @param from Vector of sequence starts.
#' @param to Vector of sequence ends.
#' @param by Vector of sequence increments.
#'
#' @return
#' A `tibble` of 2 columns with the sequence IDs and sequences.
#' @export
seq_tbl <- function(from = 1L, to = 1L, by = 1L){
  size <- seq_size(from = from, to = to, by = by)
  seq_out <- sequence2(size, from = from, by = by)
  seq_id <- seq_id(size)
  new_tbl(id = seq_id, x = seq_out)
}
