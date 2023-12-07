#' By-group rolling functions
#'
#' @description
#' Apply any function on a rolling basis
#' for each group using one-pass through the data.
#'
#' @param x Numeric vector, data frame, or list.
#' @param partial Should calculations be done using partial windows?
#' Default is \code{TRUE}.
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame.
#' @param fun A function.
#' @param before A number denoting how many indices
#' to look backward on a rolling basis.
#' @param after A number denoting how many indices to look forward on a rolling
#' basis.
#' @param unlist If `TRUE`, the result is passed to `unlist()`.
#' The default is `FALSE`.
#' @param default Default value for each list element.
#'
#' @details
#' `roll_apply` accepts any user function which makes it more flexible
#' than the other rolling functions but much less efficient. \cr
#' `roll_apply2` is an alternative to `roll_apply` that instead accepts
#' a vector of window sizes. The window sizes can be easily created
#' using `window_sequence()`.
#'
#' @returns
#' A list the same length as `x` unless `unlist` is `TRUE`.
#'
#' @seealso [time_roll_apply] [roll_sum] [roll_growth_rate]
#' @rdname roll_apply
#' @export
roll_apply <- function(x, fun, before = Inf, after = 0L,
                       g = NULL, partial = TRUE,
                       default = NULL, unlist = FALSE){
  check_is_num(before)
  check_is_num(after)
  check_length(before, 1L)
  check_length(after, 1L)
  stopifnot(is.function(fun))
  g <- GRP2(g)
  sorted_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
  group_sizes <- fpluck(sorted_info, "group_sizes")
  x <- fpluck(sorted_info, "x")
  before_seq <- before_sequence(group_sizes, k = before)
  after_seq <- after_sequence(group_sizes, k = after)
  x_size <- length(x)
  out <- new_list(x_size, default)
  if (partial){
    ind <- seq_len(x_size)
  } else {
    ind <- which((before_seq + after_seq) == (before + after))
  }
  for (i in ind){
    start <- i - .subset(before_seq, i)
    end <- .subset(after_seq, i) + i
    out[[i]] <- fun(x[start:end])
  }
  if (!fpluck(sorted_info, "sorted")){
    out <- out[collapse::greorder(seq_along(out), g = g)]
  }
  if (unlist){
    out <- unlist(out, use.names = FALSE)
  }
  out
}
roll_apply2 <- function(x, fun, window = window_sequence(length(x), Inf),
                        default = NULL, unlist = FALSE,
                        align = c("right", "left")){
  align <- rlang::arg_match(align)
  stopifnot(is.function(fun))
  check_length(window, length(x))
  x_size <- length(x)
  out <- new_list(x_size, default)
  which_gt_zero <- cpp_which(window > 0)
  if (align == "right"){
    for (i in which_gt_zero){
      start <- i - .subset2(window, i) + 1L
      out[[i]] <- fun(x[start:i])
    }
  } else {
    for (i in which_gt_zero){
      start <- i + .subset2(window, i) - 1L
      out[[i]] <- fun(x[start:i])
    }
  }
  # else {
  #   for (i in which_gt_zero){
  #     w <- .subset2(window, i - 1L)
  #     start <- i - (w %/% 2L)
  #     end <- i + (w %/% 2L)
  #     out[[i]] <- fun(x[start:end])
  #   }
  # }

  if (unlist){
    out <- unlist(out, use.names = FALSE)
  }
  out
}
