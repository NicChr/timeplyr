#' Fast grouped "locf" `NA` fill
#'
#' @description A fast and efficient by-group method for
#' "last-observation-carried-forward" `NA` filling.
#'
#' @param x A vector
#' @param g An object use for grouping x
#' This may be a vector or data frame for example.
#' @param fill_limit (Optional) maximum number of consecutive NAs to fill
#' per `NA` cluster. Default is `Inf`.
#'
#' @returns
#' A filled vector of `x` the same length as `x`.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(data.table)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' words <- do.call(paste0,
#'                  do.call(expand.grid, rep(list(letters), 3)))
#' groups <- sample(words, size = 10^5, replace = TRUE)
#' x <- sample.int(10^2, 10^5, TRUE)
#' x[sample.int(10^5, 10^4)] <- NA
#'
#' dt <- data.table(x, groups)
#'
#' roll_na_fill(x, groups)
#' \donttest{
#' library(zoo)
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
#' bench::mark(e1 = dt[, filled1 := timeplyr::roll_na_fill(x)][]$filled1,
#'             e2 = dt[, filled2 := data.table::nafill(x, type = "locf")][]$filled2,
#'             e3 = dt[, filled3 := vctrs::vec_fill_missing(x)][]$filled3,
#'             e4 = dt[, filled4 := zoo::na.locf0(x)][]$filled4)
#' # With group
#' bench::mark(e1 = dt[, filled1 := timeplyr::roll_na_fill(x, groups)][]$filled1,
#'             e2 = dt[, filled2 := data.table::nafill(x, type = "locf"), by = groups][]$filled2,
#'             e3 = dt[, filled3 := vctrs::vec_fill_missing(x), by = groups][]$filled3)
#' # Data sorted by groups
#' setkey(dt, groups)
#' bench::mark(e1 = dt[, filled1 := timeplyr::roll_na_fill(x, groups)][]$filled1,
#'             e2 = dt[, filled2 := data.table::nafill(x, type = "locf"), by = groups][]$filled2,
#'             e3 = dt[, filled3 := vctrs::vec_fill_missing(x), by = groups][]$filled3)
#' }
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
roll_na_fill <- function(x, g = NULL, fill_limit = Inf){
  check_length(fill_limit, 1)
  if (num_na(x) %in% c(0L, length(x))){
    return(x)
  }
  g <- GRP2(g)
  sorted_group_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
  sorted_g <- sorted_group_info[["sorted_GRP"]]
  sorted_x <- sorted_group_info[["x"]]
  sorted_by_groups <- sorted_group_info[["sorted"]]
  out <- cpp_roll_na_fill_grouped(sorted_x,
                                  g = group_id(sorted_g),
                                  fill_limit = fill_limit)
  if (!sorted_by_groups){
    out <- collapse::greorder(out, g = g)
  }
  out
}
