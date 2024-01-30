#' A faster version of `factor()`
#'
#' @returns
#' A `factor`.
#'
#' @param x A vector.
#' @param levels Optional factor levels.
#' @param order Should factor levels be sorted? Default is `TRUE`.
#' It typically is faster to set this to `FALSE`, in which case the levels
#' are sorted by order of first appearance.
#' @param na_exclude Should `NA` values be excluded from the factor levels?
#' Default is `TRUE`.
#' @param ordered Should the result be an ordered factor?
#'
#' @details
#' This operates similarly to `collapse::qF()`.
#' The main difference internally is that `collapse::funique()` is used
#' and therefore s3 methods can be written for it. It also works for
#' `vctrs_rcrd` objects and therefore works for `time_interval` objects.
#'
#' To drop unused factor levels, simply use `collapse::fdroplevels()`.
#'
#' @export
quick_factor <- function(x = integer(), levels = NULL, order = TRUE,
                         na_exclude = TRUE, ordered = is.ordered(x)){
  if (is.null(x)){
    x <- integer()
  }
  # Alternative implementation using group_id()
  # out <- quick_group(x, order = order, ascending = ascending)
  # starts <- attr(out, "starts")
  # lvls <- x[starts]
  # attributes(out) <- NULL
  # # Handling of NA values
  # if (na_exclude){
  #   which_na <- cpp_which(is.na(x))
  #   # which_na <- cpp_which_na(x)
  #   if (length(which_na) > 0){
  #     if (order){
  #       lvls <- lvls[seq_len(length(lvls) - 1L)]
  #     } else {
  #       lvl_to_exclude <- out[which_na[1L]]
  #       which_decrement_one <- cpp_which(out > lvl_to_exclude)
  #       out[which_decrement_one] <- out[which_decrement_one] - 1L
  #       lvls <- lvls[-lvl_to_exclude]
  #       # lvls <- lvls[-match(which_na[1], starts)]
  #     }
  #     out[which_na] <- NA
  #   }
  # }
  if (is.null(levels)){
    lvls <- collapse::funique(x, sort = order, na.last = TRUE)
  } else {
    lvls <- levels
  }
  if (na_exclude && anyNA(lvls)){
    if (order){
      lvls <- lvls[seq_len(length(lvls) - 1L)]
    } else {
      lvls <- lvls[cpp_which_not_na(lvls)]
      # lvls <- lvls[cpp_which(is.na(lvls), invert = TRUE)]
    }
  }
  out <- collapse::fmatch(x, lvls, overid = 2L)
  if (is_datetime(x)){
    attr(out, "levels") <- format(lvls, usetz = TRUE)
  } else {
    attr(out, "levels") <- as.character(lvls)
  }
  # attr(out, "ordered") <- order
  # attr(out, "na_excluded") <- na_exclude
  class(out) <- c(if (ordered) "ordered" else character(), "factor")
  out
}
