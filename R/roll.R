#' Fast by-group rolling functions
#'
#' @description
#' An efficient method for rolling sum, mean and growth rate for many groups.
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
#' @param ... Additional arguments passed to `data.table::frollmean` and
#' `data.table::frollsum`.
#' @param log For `roll_growth_rate`:
#' If `TRUE` then growth rates are calculated on the log-scale.
#' @param inf_fill For `roll_growth_rate`:
#' Numeric value to replace \code{Inf} values with.
#' Default behaviour is to keep \code{Inf} values.
#'
#' @details
#' `roll_sum` and `roll_mean` support parallel computations when
#' `x` is a data frame of multiple columns. \cr
#' `roll_geometric_mean` and `roll_harmonic_mean` are convenience functions that
#' utilise `roll_mean`. \cr
#' `roll_growth_rate` calculates the rate of percentage
#' change per unit time on a rolling basis.
#'
#' @returns
#' A numeric vector the same length as `x` when `x` is a vector,
#' or a list when `x` is a `data.frame`. \cr
#'
#' @seealso [time_roll_mean]
#'
#' @examples
#' library(timeplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
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
#' \donttest{
#'   library(data.table)
#'   library(bench)
#'   df <- data.table(g = sample.int(10^4, 10^5, TRUE),
#'                    x = rnorm(10^5))
#'   mark(e1 = df[, mean := frollmean(x, n = 7,
#'                                    align = "right", na.rm = FALSE), by = "g"]$mean,
#'        e2 = df[, mean := roll_mean(x, window = 7, g = get("g"),
#'                                    partial = FALSE, na.rm = FALSE)]$mean)
#' }
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname roll_sum
#' @export
roll_sum <- function(x, window = Inf,
                     g = NULL, partial = TRUE,
                     weights = NULL, na.rm = TRUE, ...){
  check_length(window, 1L)
  sorted_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
  group_sizes <- sorted_info[["group_sizes"]]
  group_order <- sorted_info[["group_order"]]
  x <- sorted_info[["x"]]
  if (!is.null(group_order) && !is.null(weights)){
    weights <- weights[group_order]
  }
  roll_window <- window_sequence(group_sizes,
                                 k = window,
                                 partial = partial,
                                 ascending = TRUE)

  roll_window <- cheapr::na_replace(roll_window, length(x) + 1L)

  if (!is.null(weights)){
    x <- x * weights
  }
  out <- data.table::frollsum(x,
                              n = roll_window,
                              adaptive = TRUE,
                              align = "right",
                              na.rm = na.rm, ...)
  if (!sorted_info[["sorted"]]){
    out <- greorder2(out, g = sorted_info[["GRP"]])
  }
  out
}
#' @rdname roll_sum
#' @export
roll_mean <- function(x, window = Inf, g = NULL, partial = TRUE,
                      weights = NULL, na.rm = TRUE, ...){
  check_length(window, 1L)
  sorted_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
  group_sizes <- sorted_info[["group_sizes"]]
  group_order <- sorted_info[["group_order"]]
  x <- sorted_info[["x"]]
  if (!is.null(group_order) && !is.null(weights)){
    weights <- weights[group_order]
  }
  roll_window <- window_sequence(group_sizes,
                                 k = window,
                                 partial = partial,
                                 ascending = TRUE)

  roll_window <- cheapr::na_replace(roll_window, length(x) + 1L)

  out <- frollmean3(x,
                    n = roll_window,
                    weights = weights,
                    adaptive = TRUE, align = "right",
                    na.rm = na.rm, ...)
  if (!sorted_info[["sorted"]]){
    out <- greorder2(out, g = sorted_info[["GRP"]])
  }
  out
}
#' @rdname roll_sum
#' @export
roll_geometric_mean <- function(x, window = Inf, g = NULL, partial = TRUE,
                                weights = NULL, na.rm = TRUE, ...){
  out <- roll_mean(log(x), window = window, g = g, partial = partial,
                   weights = weights, na.rm = na.rm, ...)
  if (is.list(out)){
    lapply(out, cheapr::set_exp)
  } else {
    cheapr::set_exp(out)
  }
}
#' @rdname roll_sum
#' @export
roll_harmonic_mean <- function(x, window = Inf, g = NULL, partial = TRUE,
                                weights = NULL, na.rm = TRUE, ...){
  out <- roll_mean(1 / x, window = window, g = g, partial = partial,
                   weights = weights, na.rm = na.rm, ...)
  if (is.list(out)){
    lapply(out, function(x) cheapr::set_pow(x, -1))
  } else {
    cheapr::set_pow(out, -1)
    # 1 / out
  }

}
#' @rdname roll_sum
#' @export
roll_growth_rate <- function(x, window = Inf, g = NULL,
                             partial = TRUE,
                             na.rm = FALSE,
                             log = FALSE,
                             inf_fill = NULL){
  check_is_num(x)
  check_length(window, 1)
  if (window < 1){
    stop("window must be >= 1")
  }
  sorted_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
  sorted_g <- sorted_info[["sorted_GRP"]]
  group_sizes <- sorted_info[["group_sizes"]]
  group_order <- sorted_info[["group_order"]]
  is_sorted <- sorted_info[["sorted"]]
  x <- sorted_info[["x"]]
  lag_window <- cheapr::lag_sequence(group_sizes, k = window - 1, partial = partial)
  if (na.rm){
    x_lagged <- cheapr::lag2_(x, lag_window)
    lag_window <- cpp_roll_count_na(x, window, invert = TRUE, partial = partial) - 1L
    if (log){
      gr <- exp(( log(x) - log(x_lagged) ) / lag_window)
      gr[cheapr::val_find(lag_window, 0L)] <- 1
    } else {
      gr <- ( (x / x_lagged) ^ (1 / lag_window) )
      gr[which(x == 0 & x_lagged == 0)] <- 1
    }
  } else {
    gr <- cpp_roll_growth_rate(x, lag_window, log)
  }
  if (!is.null(inf_fill)){
    # Any growth change from 0 is replaced with inf_fill
    gr[which(is.infinite(gr))] <- inf_fill
  }
  if (!fpluck(sorted_info, "sorted")){
    gr <- greorder2(gr, g = sorted_info[["GRP"]])
  }
  gr
}
