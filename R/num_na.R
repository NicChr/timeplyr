#' Efficient functions for dealing with missing values.
#'
#' @description
#' `num_na()` is a faster and more efficient alternative to `sum(is.na(x))`. \cr
#' `which_na()` is a more efficient alternative to `which(is.na(x))` \cr
#' `which_not_na()` is a more efficient alternative to `which(!is.na(x))` \cr
#' `is_na()` returns a logical vector indicating which rows have
#' a number or proportion of columns of missing values >= `threshold`. \cr
#'
#' @details
#' Long vectors, i.e vectors with length >= 2^31 are also supported. \cr
#' All these functions can be parallelised through `options(timeplyr.cores)`. \cr
#'
#' To replicate `complete.cases(x)`, use `!is_na(x, 1, "count")`.
#'
#' @param x A vector.
#' @param threshold For `is_na` the threshold of cols per row that have
#' at least this many missing values. Threshold can be a proportion or
#' absolute count/number. The default value is 1.
#' @param threshold_type Control whether the threshold is a proportion
#' or absolute count/number. Options are "count" or "prop".
#'
#' @returns
#' Number of `NA` values.
#'
#' @details
#' If `x` is a data frame, `num_na()`, `which_na()` and `which_not_na()` treat
#' rows as empty when the entire row contains `NA`s. To gain finer control, use
#' `is_na()`.
#'
#' @examples
#' library(timeplyr)
#' library(bench)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' x <- 1:10
#' x[c(1, 5, 10)] <- NA
#' num_na(x)
#' which_na(x)
#' which_not_na(x)
#' is_na(x)
#'
#' # is_na for data frames checks if row is empty
#' df <- data.frame(x = x, y = 1:10)
#' is_na(df) # Empty rows (i.e all NA)
#' is_na(df, threshold = 1, threshold_type = "count") # >= 1 col with NA
#'
#'
#' flights <- nycflights13::flights
#'
#' # We set the number of parallel cores to use
#' options(timeplyr.cores = 2)
#'
#' # num_na is more efficient than using `sum(is.na())`
#' mark(vapply(flights, num_na, integer(1)),
#'      vapply(flights, function(x) sum(is.na(x)), integer(1)),
#'      iterations = 10)
#' reset_timeplyr_options() # Set cores back to 1 safely
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#' }
#' @rdname num_na
#' @export
num_na <- function(x){
  .Call(`_timeplyr_cpp_num_na`, x)
}
#' @rdname num_na
#' @export
which_na <- cpp_which_na
#' @rdname num_na
#' @export
which_not_na <- cpp_which_not_na
#' @rdname num_na
#' @export
is_na <- function(x, threshold = 1, threshold_type = c("prop", "count")){
  if (is.atomic(x)){
    is.na(x)
  } else {
    switch(rlang::arg_match0(threshold_type, c("prop", "count")),
           count = cpp_missing_row(x, as.double(threshold), FALSE),
           prop = cpp_missing_row(x, as.double(threshold), TRUE))
  }
}
