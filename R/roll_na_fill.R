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
#' @rdname roll_na_fill
#' @export
roll_na_fill <- function(x, g = NULL, fill_limit = Inf){
  check_length(fill_limit, 1)
  num_nas <- cheapr::na_count(x, recursive = TRUE)
  if (num_nas == 0 || num_nas == cheapr::unlisted_length(x)){
    return(x)
  }

  # fastplyr::new_tbl(x = x, g = fastplyr::group_id(g)) |>
  #   fastplyr::f_fill(.cols = "x", .by = dplyr::any_of("g")) |>
  #   fastplyr::f_pull(.cols = "x")

  order_and_counts <- group_order_and_counts(g)
  o <- order_and_counts[[1L]]
  sizes <- order_and_counts[[2L]]
  if (is.null(o)){
    out <- cpp_roll_na_fill(x, fill_limit = fill_limit)
  } else {
    starts <- attr(o, "starts")
    out <- cpp_roll_na_fill_grouped(
      x,
      o = o,
      sizes = sizes,
      fill_limit = fill_limit
    )
  }
  out
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
