#' Supplementary fast statistical functions, `collapse` style
#'
#' @param x A vector or data frame.
#' In the case of `fn()` this can be left unused as long as g is not `NULL`,
#' otherwise it is used as a template with which to calculate group sizes.
#' For example, is `x` is a vector, lengths are calculated per-group,
#' and if `x` is a data frame, numbers of rows are calculated per-group.
#' @param g Object to be used for grouping,
#' passed directly to `collapse::GRP()`.
#' @param sort Should the grouped counts be ordered by the sorted groups?
#' If `FALSE` the result is ordered by groups of first appearance.
#' @param expand Should the grouped counts be expanded to match the length
#' and order of the data? Default is `FALSE`.
#' @param use.g.names If `TRUE` group names are added to the result as names.
#' This only applies to `fn()`. Default is `TRUE`.
#' @param ... Additional parameters passed to `collapse::fsum()`.
#' @details `fn()` Is different to the other `collapse`
#' fast statistical functions because given a data frame, it
#' operates on the entire data frame, instead of column-wise. It is similar
#' to the the other statistical functions in that order of the returned groups
#' matches that of `collapse::fnobs()`.
#' For example, `collapse::GRPN(c(2, 2, 1), expand = FALSE)`
#' returns `c(2, 1)` whereas `fn(g = c(2, 2, 1))` returns `c(1, 2)` which
#' is similar to `collapse::fnobs(rep(1, 3), g = c(2, 2, 1))`. \cr
#' While `fn()` is not entirely useful as a function, it is useful for
#' internal code that utilises `GRP` objects.
#' @examples
#' \dontrun{
#' library(timeplyr)
#' library(dplyr)
#' library(nycflights13)
#' flights <- flights
#' fn(flights)
#' fn(flights, g = flights$origin)
#' fn(flights, g = flights$origin, expand = TRUE)
#'
#' fnmiss(flights)
#' fprop_complete(flights)
#'
#' bench::mark(e1 = fnmiss(flights),
#'             e2 = colSums(is.na(flights)))
#'
#' # Compared to dplyr
#' bench::mark(e1 = fnmiss(flights, g = collapse::GRP(flights$tailnum, sort = FALSE)),
#'             e2 = flights %>%
#'               summarise(across(everything(), ~ sum(is.na(.x))),
#'                         .by = tailnum),
#'             e3 = flights %>%
#'               stat_summarise(.cols = names(flights),
#'                              stat = "nmiss",
#'                              .by = tailnum, sort = FALSE),
#'             check = FALSE)
#'
#' library(data.table)
#' dt <- data.table(g = sample.int(10, size = 10^5, replace = TRUE))
#' dt[, .N, by = g] # Usual DT method
#' dt[, list(N = fn(g = g, sort = F))] # Using fn
#' fcount(dt, g) # fcount (preferred)
#' stat_summarise(dt, .by = g) # stat_summarise
#' }
#' @rdname fast_stats
fn <- function(x, g = NULL, sort = TRUE,
               expand = FALSE, use.g.names = !expand){
  x_missing <- missing(x)
  if (is.null(g)){
    if (x_missing){
      stop("when g = NULL, x must be supplied")
    }
    nobs <- vec_length(x)
    if (expand){
      nobs <- rep_len(nobs, nobs)
    }
  } else {
    g <- GRP2(g, sort = sort, return.groups = use.g.names)
    nobs <- GRP_group_sizes(g)
    if (GRP_n_groups(g) == 0L){
      nobs <- 0L
    }
    if (use.g.names){
      names(nobs) <- GRP_names(g)
    }
    if (expand){
      nobs <- nobs[GRP_group_id(g)]
    }
    if (!x_missing){
      N <- vec_length(x)
      if (N != GRP_data_size(g)){
        stop("g must be the same size as the data")
      }
    }
  }
  nobs
}
# gn <- function(..., g = NULL){
#   n_dots <- dots_length(...)
#   if (n_dots > 1){
#     stop("Please supply <= 1 arguments to ...")
#   }
#   if (is.null(g)){
#     if (n_dots == 0){
#       stop("when g = NULL, ... must be supplied")
#     }
#     x <- list(...)[[1L]]
#     N <- vec_length(x)
#     nobs <- alloc(N, N)
#   } else {
#     g <- GRP2(g, sort = TRUE, return.groups = FALSE)
#     nobs <- collapse::GRPN(g, expand = TRUE)
#     if (n_dots == 1){
#       x <- list(...)[[1L]]
#       N <- vec_length(x)
#       if (N != length(nobs)){
#         stop("g must be the same size as the data")
#       }
#     }
#   }
#   nobs
# }
#' @rdname fast_stats
fnmiss <- function(x, ...){
  collapse::fsum(is.na(x), ...)
}
#' @rdname fast_stats
fprop_complete <- function(x, ...){
  1 - (fnmiss(x, ...) / vec_length(x))
}
