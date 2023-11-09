#' Efficient alternative to `which()`
#'
#' @description
#' Exactly the same as `which()` but more memory efficient.
#'
#' @param x A [logical] vector.
#' @param invert If `TRUE`, indices of values that are not `TRUE` are returned
#' (including `NA`). If `FALSE` (the default), only `TRUE` indices are returned.
#'
#' @returns
#' An unnamed integer vector.
#'
#' @details
#' This implementation is somtimes faster, sometimes slower but usually
#' a similar speed to `which()`. It is almost always more memory efficient.
#'
#' @export
which2 <- function(x, invert = FALSE){
  .Call(`_timeplyr_cpp_which`, x, invert)
}
