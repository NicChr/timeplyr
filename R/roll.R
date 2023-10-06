#' Fast by-group rolling sum/mean
#'
#' @description An efficient method for rolling sum/mean for many groups. \cr
#'
#' @param x Numeric vector, data frame, or list.
#' @param window Rolling window size, default is `Inf`.
#' @param partial Should calculations be done using partial windows?
#' Default is \code{TRUE}.
#' @param weights Importance weights. Must be the same length as x.
#' Currently, no normalisation of weights occurs.
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame.
#' @param na.rm Should missing values be removed for the calculation?
#' The default is `TRUE`.
#' @param fun A function.
#' @param before A number denoting how many indices to look backward on a rolling
#' basis.
#' @param after A number denoting how many indices to look forward on a rolling
#' basis.
#' @param ... Additional arguments passed to `data.table::frollmean` and
#' `data.table::frollsum`.
#' @details `roll_sum` and `roll_mean` support parallel computations when
#' `x` is a data frame of multiple columns. \cr
#' `roll_geometric_mean` and `roll_harmonic_mean` are convenience functions that
#' utilise `roll_mean`. \cr
#' `roll_apply` accepts any user function and is more flexible but much
#' less efficient. It also only accepts vector input. \cr
#' Please note that `roll_apply` and `time_roll_apply` are still experimental.
#' @return Excluding `roll_apply`, these return a numeric vector the
#' same length as `x` when `x` is a vector, and a list when `x` is a `data.frame`. \cr
#' `roll_apply` returns a list the same length as `x`.
#' @seealso [time_roll_mean] [roll_growth_rate]
#' @examples
#' library(timeplyr)
#' x <- 1:10
#' roll_sum(x) # Simple rolling total
#' roll_mean(x) # Simple moving average
#' roll_sum(x, window = 3)
#' roll_mean(x, window = 3)
#' roll_sum(x, window = 3, partial = FALSE)
#' roll_mean(x, window = 3, partial = FALSE)
#'
#' # Plot of expected value of 'coin toss' over many flips
#' set.seed(42)
#' x <- sample(c(1, 0), 10^3, replace = TRUE)
#' ev <- roll_mean(x)
#' plot(ev)
#' abline(h = 0.5, lty = 2)
#'
#' all.equal(roll_sum(iris$Sepal.Length, g = iris$Species),
#'           ave(iris$Sepal.Length, iris$Species, FUN = cumsum))
#' # The below is run using parallel computations where applicable
#' roll_sum(iris[, 1:4], window = 7, g = iris$Species)
#' \dontrun{
#' library(data.table)
#' library(bench)
#' df <- data.table(g = sample.int(10^5, 10^6, TRUE),
#'                  x = rnorm(10^6))
#' mark(e1 = df[, mean := frollmean(x, n = 7, align = "right", na.rm = FALSE), by = "g"]$mean,
#'      e2 = df[, mean := roll_mean(x, window = 7, g = get("g"), partial = FALSE, na.rm = FALSE)]$mean)
#' }
#' @rdname roll_sum
#' @export
roll_sum <- function(x, window = Inf,
                     g = NULL, partial = TRUE,
                     weights = NULL, na.rm = TRUE, ...){
  if (length(window) != 1L){
    stop("window must be of length 1")
  }
  window <- min(window, .Machine[["integer.max"]])
  sorted_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
  group_sizes <- fpluck(sorted_info, "group_sizes")
  group_order <- fpluck(sorted_info, "group_order")
  x <- fpluck(sorted_info, "x")
  if (!is.null(group_order) && !is.null(weights)){
    weights <- weights[group_order]
  }
  roll_window <- window_sequence(group_sizes,
                                 k = window,
                                 partial = partial,
                                 ascending = TRUE)
  if (!is.null(weights)){
    x <- x * weights
  }
  out <- data.table::frollsum(x,
                              n = roll_window,
                              adaptive = TRUE,
                              align = "right",
                              na.rm = na.rm, ...)
  if (!fpluck(sorted_info, "sorted")){
    out <- greorder2(out, g = fpluck(sorted_info, "GRP"))
  }
  out
}
#' @rdname roll_sum
#' @export
roll_mean <- function(x, window = Inf, g = NULL, partial = TRUE,
                      weights = NULL, na.rm = TRUE, ...){
  if (length(window) != 1L){
    stop("window must be of length 1")
  }
  window <- min(window, .Machine[["integer.max"]])
  sorted_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
  group_sizes <- fpluck(sorted_info, "group_sizes")
  group_order <- fpluck(sorted_info, "group_order")
  x <- fpluck(sorted_info, "x")
  if (!is.null(group_order) && !is.null(weights)){
    weights <- weights[group_order]
  }
  roll_window <- window_sequence(group_sizes,
                                 k = window,
                                 partial = partial,
                                 ascending = TRUE)
  out <- frollmean3(x,
                    n = roll_window,
                    weights = weights,
                    adaptive = TRUE, align = "right",
                    na.rm = na.rm, ...)
  if (!fpluck(sorted_info, "sorted")){
    out <- greorder2(out, g = fpluck(sorted_info, "GRP"))
  }
  out
}
#' @rdname roll_sum
#' @export
roll_geometric_mean <- function(x, window = Inf, g = NULL, partial = TRUE,
                                weights = NULL, na.rm = TRUE, ...){
  exp(roll_mean(log(x), window = window, g = g, partial = partial,
                weights = weights, na.rm = na.rm, ...))
}
#' @rdname roll_sum
#' @export
roll_harmonic_mean <- function(x, window = Inf, g = NULL, partial = TRUE,
                                weights = NULL, na.rm = TRUE, ...){
  1 / roll_mean(1 / x, window = window, g = g, partial = partial,
                weights = weights, na.rm = na.rm, ...)
}
# roll_max <- function(x, before = 0L, after = 0L,
#                      g = NULL,
#                      # partial = TRUE,
#                      na.rm = TRUE){
#   stopifnot(is.numeric(x))
#   sorted_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
#   group_sizes <- fpluck(sorted_info, "group_sizes")
#   before_seq <- before_sequence(group_sizes,
#                                 k = before)
#   after_seq <- after_sequence(group_sizes,
#                               k = after)
#   out <- roll_apply_max(fpluck(sorted_info, "x"),
#                         before = before_seq,
#                         after = after_seq,
#                         na_rm = na.rm)
#   if (!fpluck(sorted_info, "sorted")){
#     out <- collapse::greorder(out, g = fpluck(sorted_info, "GRP"))
#   }
#   out
# }
# roll_min <- function(x, before = 0L, after = 0L,
#                      g = NULL,
#                      # partial = TRUE,
#                      na.rm = TRUE){
#   stopifnot(is.numeric(x))
#   sorted_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
#   group_sizes <- fpluck(sorted_info, "group_sizes")
#   before_seq <- before_sequence(group_sizes,
#                                 k = before)
#   after_seq <- after_sequence(group_sizes,
#                               k = after)
#   out <- roll_apply_min(fpluck(sorted_info, "x"),
#                         before = before_seq,
#                         after = after_seq,
#                         na_rm = na.rm)
#   if (!fpluck(sorted_info, "sorted")){
#     out <- collapse::greorder(out, g = fpluck(sorted_info, "GRP"))
#   }
#   out
# }
#' @rdname roll_sum
#' @export
roll_apply <- function(x, fun, before = 0L, after = 0L,
                       g = NULL, partial = TRUE){
  check_is_num(before)
  check_is_num(after)
  check_length(before, 1L)
  check_length(after, 1L)
  stopifnot(is.function(fun))
  sorted_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
  group_sizes <- fpluck(sorted_info, "group_sizes")
  x <- fpluck(sorted_info, "x")
  before_seq <- before_sequence(group_sizes, k = before)
  after_seq <- after_sequence(group_sizes, k = after)
  x_size <- length(x)
  out <- vector("list", x_size)
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
    out <- out[order(sorted_info[["group_order"]])]
  }
  out
}
