#' Fast grouped "locf" `NA` fill
#'
#' @description A fast and efficient by-group method for
#' "last-observation-carried-forward" `NA` filling.
#'
#' @param x A vector.
#' @param g An object use for grouping x
#' This may be a vector or data frame for example.
#' @param fill_limit (Optional) maximum number of consecutive NAs to fill
#' per `NA` cluster. Default is `Inf`.
#'
#' @returns
#' A filled vector of `x` the same length as `x`.
#'
#'
#' @details
#'
#' ## Method
#'
#' When supplying groups using `g`, this method uses `radixorder(g)` to
#' specify how to loop through `x`, making this extremely efficient.
#'
#' When `x` contains zero or all `NA` values, then `x` is returned with no copy
#' made.
#'
#' `.roll_na_fill()` is the same as `roll_na_fill()` but without a g argument and
#' it performs no sanity checks. It is passed straight to c++ which makes it efficient for
#' loops.
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
#' # Is the fastest but not most memory efficient
#' # For low repetitions and large vectors, data.table is best
#'
#' # For large numbers of repetitions (groups) and data
#' # that is sorted by groups
#' # timeplyr is fastest
#'
#' # No groups
#' bench::mark(e1 = dt[, filled1 := timeplyr::roll_na_fill(x)][]$filled1,
#'             e2 = dt[, filled2 := data.table::nafill(x, type = "locf")][]$filled2,
#'             e3 = dt[, filled3 := vctrs::vec_fill_missing(x)][]$filled3,
#'             e4 = dt[, filled4 := zoo::na.locf0(x)][]$filled4,
#'             e5 = dt[, filled5 := timeplyr::.roll_na_fill(x)][]$filled5)
#' # With group
#' bench::mark(e1 = dt[, filled1 := timeplyr::roll_na_fill(x, groups)][]$filled1,
#'             e2 = dt[, filled2 := data.table::nafill(x, type = "locf"), by = groups][]$filled2,
#'             e3 = dt[, filled3 := vctrs::vec_fill_missing(x), by = groups][]$filled3,
#'             e4 = dt[, filled4 := timeplyr::.roll_na_fill(x), by = groups][]$filled4)
#' # Data sorted by groups
#' setkey(dt, groups)
#' bench::mark(e1 = dt[, filled1 := timeplyr::roll_na_fill(x, groups)][]$filled1,
#'             e2 = dt[, filled2 := data.table::nafill(x, type = "locf"), by = groups][]$filled2,
#'             e3 = dt[, filled3 := vctrs::vec_fill_missing(x), by = groups][]$filled3,
#'             e4 = dt[, filled4 := timeplyr::.roll_na_fill(x), by = groups][]$filled4)
#' }
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname roll_na_fill
#' @export
roll_na_fill <- function(x, g = NULL, fill_limit = Inf){
  check_length(fill_limit, 1)
  if (num_na(x) %in% c(0L, length(x))){
    return(x)
  }
  o <- radixorderv2(g, starts = FALSE, sort = FALSE, group.sizes = TRUE)
  if (is_GRP(g)){
    sizes <- GRP_group_sizes(g)
    # Accounting for factors
    if (collapse::anyv(sizes, 0L)){
      sizes <- sizes[cpp_which(sizes > 0L)]
    }
  } else {
    sizes <- attr(o, "group.sizes")
  }
  if (is.null(o)){
    out <- .roll_na_fill(x, fill_limit = fill_limit)
  } else {
    starts <- attr(o, "starts")
    out <- cpp_roll_na_fill_grouped(x,
                                     o = o,
                                     sizes = sizes,
                                     fill_limit = fill_limit)
  }
  out
}

#' @rdname roll_na_fill
#' @export
.roll_na_fill <- function(x, fill_limit = Inf){
  .Call(`_timeplyr_cpp_roll_na_fill`, x, fill_limit)
}

# Old version that sorts
# roll_na_fill <- function(x, g = NULL, fill_limit = Inf){
#   check_length(fill_limit, 1)
#   if (num_na(x) %in% c(0L, length(x))){
#     return(x)
#   }
#   g <- GRP2(g, sort = TRUE)
#   sorted_group_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
#   sorted_g <- sorted_group_info[["sorted_GRP"]]
#   sorted_x <- sorted_group_info[["x"]]
#   sorted_by_groups <- sorted_group_info[["sorted"]]
#   out <- cpp_roll_na_fill_grouped(sorted_x,
#                                   g = group_id(sorted_g),
#                                   fill_limit = fill_limit,
#                                   check_sorted = FALSE)
#   if (!sorted_by_groups){
#     out <- greorder2(out, g = g)
#   }
#   out
# }
