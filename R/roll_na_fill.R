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
#' @return A filled vector of `x` the same length as `x`.
#' @examples
#' library(timeplyr)
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
  g <- GRP2(g)
  if (is.null(g)){
    # Ungrouped method 2 (more flexible, slower)
    which_na <- collapse::whichNA(x)
    if (length(which_na) %in% c(0L, length(x))){
      return(x)
    }
    roll_lag <- integer(length(x))
    roll_lag[which_na] <- data.table::rleidv(x)[which_na]
    roll_lag[which_na] <- frowid(roll_lag[which_na], order = FALSE)
    roll_lag[roll_lag >= seq_along(x)] <- 0L
    if (!is.null(fill_limit)){
      if (length(fill_limit) != 1L){
        stop("fill_limit must be a single whole number")
      }
      roll_lag[roll_lag > fill_limit] <- 0L
    }
    out <- roll_lag(x, roll_lag, check = FALSE)
  } else {
    # Grouped method
    sorted_group_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
    sorted_g <- sorted_group_info[["sorted_GRP"]]
    sorted_by_groups <- sorted_group_info[["sorted"]]
    sorted_x <- sorted_group_info[["x"]]
    sorted_group_id <- GRP_group_id(sorted_g)
    is_na <- is.na(sorted_x)
    if (sum(is_na) %in% c(0L, length(x))){
      return(x)
    }
    which_na <- which(is_na)
    consecutive_id <- data.table::fifelse(is_na,
                                          data.table::rleidv(sorted_x),
                                          0L)
    roll_lag <- integer(length(x))
    roll_lag[which_na] <- frowid(roll_lag, g = list(sorted_group_id,
                                                    consecutive_id),
                                    order = FALSE)[which_na]
    row_id <- frowid(x, g = sorted_g)
    roll_lag[roll_lag >= row_id] <- 0L
    if (!is.null(fill_limit)){
      if (length(fill_limit) != 1L){
        stop("fill_limit must be a single whole number")
      }
      roll_lag[roll_lag > fill_limit] <- 0L
    }
    out <- roll_lag(x, roll_lag, check = FALSE)
    if (!sorted_by_groups){
      out <- collapse::greorder(out, g = g)
    }
  }
  out
}
