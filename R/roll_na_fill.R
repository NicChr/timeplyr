#' Fast grouped "locf" `NA` fill
#'
#' @description A fast by-group method for
#' "last-observation-carried-forward" `NA` filling.
#'
#' @param x A vector
#' @param g An object use for grouping x
#' This may be a vector or data frame for example.
#' @param fill_limit (Optional) maximum number of consecutive NAs to fill
#' per `NA` cluster.
#' @examples
#' library(dplyr)
#' library(data.table)
#'
#' words <- do.call(paste0,
#'                  do.call(expand.grid, rep(list(letters), 3)))
#' groups <- sample(words, size = 10^5, replace = TRUE)
#' x <- sample.int(10^2, 10^5, TRUE)
#' x[sample.int(10^5, 10^4)] <- NA
#'
#' dt <- data.table(x, groups)
#'
#' roll_na_fill(x, groups)
#' \dontrun{
#' library(zoo)
#' library(imputeTS)
#' library(runner)
#'
#'   # Summary
#' # Latest version of vctrs with their vec_fill_missing
#' # Is by far the best for repeated filling and memory efficiency
#' # For low repetitions and large vectors, data.table is best
#'
#' # For large numbers of repetitions (groups) and data
#' # that is sorted by groups
#' # timeplyr is fastest, but not memory efficient
#'
#' # No groups
#' bench::mark(e1 = dt[, filled := timeplyr::roll_na_fill(x)]$filled,
#'             e2 = dt[, filled := data.table::nafill(x, type = "locf")]$filled,
#'             e3 = dt[, filled := vctrs::vec_fill_missing(x)]$filled,
#'             e4 = dt[, filled := zoo:::na.locf0(x)]$filled,
#'             e5 = dt[, filled := imputeTS::na_locf(x, na_remaining = "keep")]$filled,
#'             e6 = dt[, filled := runner::fill_run(x, run_for_first = FALSE)]$filled)
#' # With group
#' bench::mark(e1 = dt[, filled := timeplyr::roll_na_fill(x, groups)]$filled,
#'             e2 = dt[, filled := data.table::nafill(x, type = "locf"), by = groups]$filled,
#'             e3 = dt[, filled := vctrs::vec_fill_missing(x), by = groups]$filled)
#' # Data sorted by groups
#' setkey(dt, groups)
#' bench::mark(e1 = dt[, filled := timeplyr::roll_na_fill(x, groups)]$filled,
#'             e2 = dt[, filled := data.table::nafill(x, type = "locf"), by = groups]$filled,
#'             e3 = dt[, filled := vctrs::vec_fill_missing(x), by = groups]$filled)
#' }
#' @export
roll_na_fill <- function(x, g = NULL, fill_limit = NULL){
  if (length(g) == 0L){
    # Ungrouped method 1
    if (is.null(fill_limit)){
      is_not_na <- !is.na(x)
      if (sum(is_not_na) %in% c(0L, length(x))){
        return(x)
      }
      rollsum_not_na <- collapse::fcumsum(is_not_na, na.rm = FALSE)
      collapse::setop(rollsum_not_na, op = "+", V = 1L)
      out <- c(x[NA_integer_], x[is_not_na])[rollsum_not_na]
    } else {
      # Ungrouped method 2 (more flexible, slower)
      which_na <- collapse::whichNA(x)
      if (length(which_na) %in% c(0L, length(x))){
        return(x)
      }
      roll_lag <- integer(length(x))
      setv(roll_lag,
           which_na,
           data.table::rleidv(x)[which_na],
           vind1 = TRUE)
      setv(roll_lag,
           which_na,
           frowid(roll_lag[which_na], order = FALSE),
           vind1 = TRUE)
      # consecutive_id <- integer(length(x))
      # consecutive_id[which_na] <- qg_to_integer(collapse::groupid(x))[which_na]
      # roll_lag <- integer(length(x))
      # roll_lag[which_na] <- frowid(consecutive_id[which_na],
      #                              order = FALSE)
      roll_lag[roll_lag >= seq_along(x)] <- 0L
      if (!is.null(fill_limit)){
        if (length(fill_limit) != 1L){
          stop("fill_limit must be a single whole number")
        }
        roll_lag[roll_lag > fill_limit] <- 0L
      }
      out <- roll_lag(x, roll_lag, check = FALSE)
    }
  } else {
    # Grouped method
    g <- GRP2(g)
    if (!GRP_is_sorted(g)){
      g2 <- collapse::GRP(GRP_group_id(g)[GRP_order(g)])
      x  <- x[GRP_order(g)]
    } else {
      g2 <- g
    }
    is_na <- is.na(x)
    which_na <- which(is_na)
    if (length(which_na) %in% c(0L, length(x))){
      if (!GRP_is_sorted(g)){
        x <- collapse::greorder(x, g = g)
      }
      return(x)
    }
    consecutive_id <- data.table::fifelse(is_na,
                                          data.table::rleidv(x),
                                          0L)
    roll_lag <- integer(length(x))
    setv(roll_lag, which_na, frowid(roll_lag, g = list(GRP_group_id(g2),
                                                       consecutive_id),
                                    order = FALSE)[which_na],
         vind1 = TRUE)
    row_id <- frowid(x, g = g2)
    roll_lag[roll_lag >= row_id] <- 0L
    if (!is.null(fill_limit)){
      if (length(fill_limit) != 1L){
        stop("fill_limit must be a single whole number")
      }
      roll_lag[roll_lag > fill_limit] <- 0L
    }
    out <- roll_lag(x, roll_lag, check = FALSE)
    if (!GRP_is_sorted(g)){
      out <- collapse::greorder(out, g = g)
    }
  }
  out
}
# roll_na_fill <- function(x, g = NULL){
#   # is_na <- is.na(x)
#   # which_na <- which(is_na)
#   # consecutive_id <- data.table::fifelse(is_na, data.table::rleidv(x), 0L)
#   if (is.null(g)){
#     which_na <- collapse::whichNA(x)
#     consecutive_id <- integer(length(x))
#     consecutive_id[which_na] <- data.table::rleidv(x)[which_na]
#     roll_lag <- integer(length(x))
#     # For every distinct rolling-group of NAs
#     # We calculate the row IDs per group
#     roll_lag[which_na] <- frowid(consecutive_id[which_na],
#                                  order = FALSE)
#     # If these row IDs exceed the overall row ID, then replace these
#     # with zero
#     roll_lag[roll_lag >= seq_along(x)] <- 0L
#     # roll_lag[roll_lag[which_na] >= seq_along(x)[which_na]] <- 0L
#     out <- rolling_lag(x, roll_lag, check = FALSE)
#   } else {
#     is_na <- is.na(x)
#     consecutive_id <- data.table::fifelse(is_na, data.table::rleidv(x), 0L)
#     g <- GRP2(g)
#     if (!GRP_is_sorted(g)){
#       g2 <- collapse::GRP(GRP_group_id(g)[GRP_order(g)])
#       x  <- x[GRP_order(g)]
#       is_na <- is_na[GRP_order(g)]
#       which_na <- which(is_na)
#       consecutive_id <- consecutive_id[GRP_order(g)]
#     } else {
#       g2 <- g
#       which_na <- which(is_na)
#     }
#     roll_lag <- integer(length(x))
#     # roll_lag[which_na] <- frowid(which_na,
#     #                              g = list(GRP_group_id(g2)[which_na],
#     #                                       consecutive_id[which_na]))
#     setv(roll_lag, which_na, frowid(roll_lag, g = list(GRP_group_id(g2),
#                                                        consecutive_id),
#                                     order = FALSE)[which_na],
#          vind1 = TRUE)
#     row_id <- frowid(x, g = g2)
#     roll_lag[roll_lag >= row_id] <- 0L
#     out <- rolling_lag(x, roll_lag, check = FALSE)
#     if (!GRP_is_sorted(g)){
#       out <- collapse::greorder(out, g = g)
#     }
#   }
#   out
# }
# roll_na_fill <- function(x, g = NULL){
#   is_na <- is.na(x)
#   consecutive_id <- data.table::fifelse(is_na, data.table::rleidv(x), 0L)
#   if (is.null(g)){
#     roll_lag <- data.table::fifelse(is_na, frowid(consecutive_id), 0L)
#     row_id <- seq_along(x)
#     roll_lag <- data.table::fifelse(roll_lag >= row_id, row_id - 1L, roll_lag)
#     out <- rolling_lag(x, roll_lag, check = FALSE)
#   } else {
#     g <- GRP2(g)
#     if (!GRP_is_sorted(g)){
#       g2 <- collapse::GRP(GRP_group_id(g)[GRP_order(g)])
#       x  <- x[GRP_order(g)]
#       is_na <- is_na[GRP_order(g)]
#       consecutive_id <- consecutive_id[GRP_order(g)]
#     } else {
#       g2 <- g
#     }
#     roll_lag <- data.table::fifelse(is_na, frowid(x, g = list(GRP_group_id(g2),
#                                                               consecutive_id)), 0L)
#     row_id <- frowid(x, g = g2)
#     roll_lag[roll_lag >= frowid(x, g = g2)] <- 0L
#     # roll_lag <- data.table::fifelse(roll_lag >= row_id, row_id - 1L,
#     #                                 roll_lag)
#     out <- rolling_lag(x, roll_lag, check = FALSE)
#     if (!GRP_is_sorted(g)){
#       out <- collapse::greorder(out, g = g)
#     }
#   }
#   out
# }
