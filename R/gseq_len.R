#' Grouped version of `seq_len()`
#' @description This is an extension to `seq_len()`
#' that produces a running integer sequence for each group that
#' increments by 1 and starts at 1.
#' This is most useful for calculating row numbers.
#' It is similar to `data.table::rowid()`.
#'
#' @param length Sequence length.
#' @param g (Optional) vector of group IDs.
#' @return An integer vector,
#' or double if `length > .Machine$integer.max`
#' @export
gseq_len <- function(length, g = NULL){
  if (is.null(g)){
    seq_len(length)
  }  else {
    collapse::fcumsum(seq_ones(length),
                      na.rm = FALSE,
                      g = g)
  }
}
