# Safer versions of collapse::flag and collapse::fdiff
# It is safer in the sense that if the data isn't ordered by group
# The result is reordered to be correct

# flag2 <- function(x, n = 1L, g = NULL, ...){
#   if (is.null(x)){
#     return(NULL)
#   }
#   n <- as.integer(sign(n) * min(vec_length(x), abs(n)))
#   sorted_group_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
#   g <- sorted_group_info[["GRP"]]
#   sorted_g <- sorted_group_info[["sorted_GRP"]]
#   sorted_x <- sorted_group_info[["x"]]
#   is_sorted <- sorted_group_info[["sorted"]]
#   out <- collapse::flag(sorted_x, n = n, g = sorted_g, ...)
#   if (!is_sorted){
#     out <- greorder2(out, g = g)
#   }
#   out
# }

flag2 <- function(x, n = 1L, g = NULL, fill = NULL){
  if (is.null(x)){
    return(NULL)
  }
  check_length(n, 1)
  n <- as.integer(n)
  # N <- vec_length(x)
  # n <- as.integer(sign(n) * min(N, abs(n)))
  if (is.null(g)){
    if (n < 0){
      return(cpp_roll_lead(x, abs(n), fill))
    } else {
      return(cpp_roll_lag(x, n, fill))
    }
  }
  o <- radixorderv2(g, starts = FALSE, sort = FALSE, group.sizes = TRUE)
  if (is_GRP(g)){
    sizes <- GRP_group_sizes(g)
  } else {
    sizes <- attr(o, "group.sizes")
  }
  if (n >= 0){
    cpp_roll_lag_grouped(x, n, o, sizes, fill)
  } else {
    cpp_roll_lead_grouped(x, abs(n), o, sizes, fill)
  }
}

fdiff2 <- function(x, n = 1L, g = NULL, fill = NULL){
  if (is.null(x)){
    return(NULL)
  }
  check_length(n, 1)
  n <- as.integer(n)
  o <- radixorderv2(g, starts = FALSE, sort = FALSE, group.sizes = TRUE)
  if (is_GRP(g)){
    sizes <- GRP_group_sizes(g)
  } else {
    sizes <- attr(o, "group.sizes")
  }
  if (is.null(g)){
    cpp_roll_diff(x, k = n, fill = fill)
  } else {
    cpp_roll_diff_grouped(x, k = n, fill = fill, o = o, sizes = sizes)
  }
}

window_sequence <- function(size, k, partial = TRUE, ascending = TRUE) {
  .Call(`_timeplyr_cpp_window_sequence`, size, k, partial, ascending)
}
lag_sequence <- function(size, k, partial = TRUE) {
  .Call(`_timeplyr_cpp_lag_sequence`, size, k, partial)
}
lead_sequence <- function(size, k, partial = TRUE) {
  .Call(`_timeplyr_cpp_lead_sequence`, size, k, partial)
}

# Vctrs style rolling chop
roll_chop <- function(x, sizes = collapse::alloc(1L, vec_length(x))){
  x_size <- vec_length(x)
  out_length <- length(sizes)
  if (x_size != length(sizes)){
    stop("length of x must equal length of sizes")
  }
  if (log10(sum(sizes)) >= 9){
    warning("The result contains more than 1e09 elements, this may take time",
            immediate. = TRUE)
  }
  sizes <- as.integer(sizes)
  which_size_gt_zero <- which(sizes > 0L)
  which_size_zero <- which(sizes == 0L)
  sizes_gt_zero <- sizes[which_size_gt_zero]
  seq_id_GRP <- sorted_group_id_to_GRP(seq_id(sizes_gt_zero),
                                       n_groups = length(sizes_gt_zero),
                                       group_sizes = sizes_gt_zero)
  ind <- collapse::gsplit(sequence(sizes,
                                   from = (seq_along(sizes) - sizes + 1L),
                                   by = 1L),
                          g = seq_id_GRP)
  out <- vector("list", out_length)
  out[which_size_zero] <- list(vec_head(x, n = 0L))
  out[which_size_gt_zero] <- vctrs::vec_chop(x, indices = ind)
  out
}
# slider style rolling chop
roll_chop2 <- function(x, before = 0L, after = 0L, partial = TRUE){
  complete_size <- before + after
  x_size <- vec_length(x)
  if (length(before) == 1L){
    # before <- flag2(window_sequence(length(x), before,
    #                                 ascending = TRUE,
    #                                 partial = TRUE),
    #                 fill = 0L)
    before <- before_sequence(x_size, k = before)
  }
  if (length(after) == 1L){
    after <- after_sequence(x_size, k = after)
    # after <- flag2(window_sequence(length(x),
    #                                after,
    #                                ascending = FALSE,
    #                                partial = TRUE),
    #                fill = 0L, n = max(-length(x), -1L))
  }
  # if (length(before) != length(x)){
  #   stop("length of before must equal 1 or length(x)")
  # }
  # if (length(after) != length(x)){
  #   stop("length of after must equal 1 or length(x)")
  # }
  out <- vector("list", x_size)
  if (partial){
    if (is.atomic(x)){
      for (i in seq_len(x_size)){
        islice <- seq.int(i - .subset(before, i),
                          i + .subset(after, i),
                          by = 1L)
        out[[i]] <- x[islice]
      }
    } else {
      for (i in seq_len(x_size)){
        islice <- seq.int(i - .subset(before, i),
                          i + .subset(after, i),
                          by = 1L)
        out[[i]] <- vec_slice2(x, islice)
      }
    }
  } else {
    which_complete <- collapse::whichv(before + after, complete_size)
    if (is.atomic(x)){
      for (i in which_complete){
        islice <- seq.int(i - .subset(before, i),
                          i + .subset(after, i),
                          by = 1L)
        out[[i]] <- x[islice]
      }
    } else {
      for (i in which_complete){
        islice <- seq.int(i - .subset(before, i),
                          i + .subset(after, i),
                          by = 1L)
        out[[i]] <- vec_slice2(x, islice)
      }
    }
  }
  out
}
# No partial argument, just a weights extension to data.table::frollsum()
frollsum3 <- function(x, n, weights = NULL, ...){
  if (!is.null(weights)){
    x <- x * weights
  }
  data.table::frollsum(x, n = n, ...)
}
frollmean3 <- function(x, n, weights = NULL, ...){
  if (!is.null(weights)){
    x <- x * weights
    weights[is.na(x)] <- NA_real_
    out <- data.table::frollsum(x, n = n, ...) /
      data.table::frollsum(weights, n = n, ...)
  } else {
    out <- data.table::frollmean(x, n = n, ...)
  }
  out
}
# Mostly base R rolling chop
# roll_chop3 <- function(x, sizes = collapse::alloc(1L, vec_length(x))){
#   x_size <- vec_length(x)
#   out_length <- length(sizes)
#   if (x_size != length(sizes)){
#     stop("length of x must equal length of sizes")
#   }
#   if (log10(sum(sizes)) >= 9){
#     warning("The result contains more than 1e09 elements, this may take time",
#             immediate. = TRUE)
#   }
#   sizes <- as.integer(sizes)
#   out <- vector("list", x_size)
#   if (is.atomic(x)){
#     for (i in seq_len(x_size)){
#       # out[[i]] <- x[(i - sizes[i] + 1L):i]
#       out[[i]] <- x[seq_len(.subset(sizes, i)) + (i - .subset(sizes, i))]
#     }
#   } else {
#     for (i in seq_len(x_size)){
#       out[[i]] <- vec_slice2(x, seq_len(.subset(sizes, i)) + (i - .subset(sizes, i)))
#     }
#   }
#   out
# }
