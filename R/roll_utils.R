# Get rolling window sizes for multiple groups
# window_sequence <- function(size, k, partial = TRUE, ascending = TRUE){
#   if (length(size) == 1L){
#     return(window_seq(n = size, k = k, partial = partial, ascending = ascending))
#   }
#   if (length(k) == 1L){
#     k <- rep_len(k, length(size))
#   }
#   if (length(k) != length(size)){
#     stop("k must be of length 1 or have same length as size")
#   }
#   input_k <- k
#   k <- pmin(size, k)
#   k <- pmax(k, 0L) # Bound k to >= 0
#   k <- as.integer(k)
#   k <- rep.int(k, times = size)
#   out <- pmin(sequence(size, from = 1L, by = 1L), k)
#   if (!partial){
#     input_k <- rep.int(input_k, times = size)
#     out[out < input_k] <- NA_integer_
#   }
#   if (!ascending){
#     out <- out[sequence(size, from = size, by = -1L)]
#   }
#   out
# }
# Get rolling window sizes, including partial
window_seq <- function(k, n, partial = TRUE, ascending = TRUE){
  if (length(k) != 1L) stop("k must be of length 1.")
  if (length(n) != 1L) stop("n must be of length 1.")
  if (n > .Machine[["integer.max"]]){
    stop("n must not be greater than .Machine$integer.max")
  }
  n <- as.integer(n)
  k[is.infinite(k)] <- n
  k <- as.integer(k)
  k <- max(k, 0L) # Bound k to >= 0
  pk <- min(max(k - 1L, 0L), n) # Partial k, bounded to >= 0
  p_seq <- seq_len(pk) # Partial window sequence
  out <- collapse::alloc(k, n)
  # Replace partial part with partial sequence
  if (partial){
    out[p_seq] <- p_seq
  } else {
    out[p_seq] <- NA_integer_
  }
  if (!ascending){
    out <- .subset(out, n:(min(n, 1L)))
  }
  out
}
# before_sequence <- function(size, before = 0L, partial = TRUE){
#   if (length(before) != 1L){
#     stop("before must of be length 1")
#   }
#   start_fill <- if (partial) 0L else NA_integer_
#   seq_id_GRP <- sorted_group_id_to_GRP(seq_id(size),
#                                        n_groups = length(size),
#                                        group_sizes = size,
#                                        group.starts = FALSE)
#   flag2(window_sequence(size, before, ascending = TRUE, partial = partial),
#         g = seq_id_GRP, fill = start_fill, n = 1L)
# }
# after_sequence2 <- function(size, k, partial = TRUE){
#   before_sequence(size, k = k, partial = partial)[
#     sequence(size, from = size, by = -1L)
#   ]
# }
# before_sequence2 <- function(size, k, partial = TRUE){
#   if (length(k) != 1L){
#     stop("k must be of length 1")
#   }
#   input_k <- k
#   k <- max(k, 0L)
#   k <- as.integer(k)
#   out <- pmin(sequence(size, from = 0L, by = 1L), k)
#   if (!partial){
#     out[out < input_k] <- NA_integer_
#   }
#   out
# }
# after_sequence <- function(size, after = 0L, partial = TRUE){
#   if (length(after) != 1L){
#     stop("after must of be length 1")
#   }
#   start_fill <- if (partial) 0L else NA_integer_
#   seq_id_GRP <- sorted_group_id_to_GRP(seq_id(size),
#                                        n_groups = length(size),
#                                        group_sizes = size,
#                                        group.starts = FALSE)
#   flag2(window_sequence(size, after, ascending = FALSE, partial = partial),
#         g = seq_id_GRP, fill = start_fill, n = -1L)
# }
# before_sequence <- function(x, before = 0L, g = NULL, partial = TRUE){
#   if (length(before) != 1L){
#     stop("before must of be length 1")
#   }
#   g <- GRP2(g)
#   if (is.null(g)){
#     is_sorted <- TRUE
#     group_sizes <- length(x)
#   } else {
#     is_sorted <- isTRUE(attr(GRP_order(g), "sorted"))
#     group_sizes <- GRP_group_sizes(g)
#   }
#   stopifnot(is_sorted)
#   start_fill <- if (partial) 0L else NA_integer_
#   flag2(window_sequence(group_sizes, before, ascending = TRUE, partial = partial),
#         g = g, fill = start_fill, n = 1L)
# }
# after_sequence <- function(x, after = 0L, g = NULL, partial = TRUE){
#   if (length(after) != 1L){
#     stop("after must of be length 1")
#   }
#   g <- GRP2(g)
#   if (is.null(g)){
#     is_sorted <- TRUE
#     group_sizes <- length(x)
#   } else {
#     is_sorted <- isTRUE(attr(GRP_order(g), "sorted"))
#     group_sizes <- GRP_group_sizes(g)
#   }
#   stopifnot(is_sorted)
#   start_fill <- if (partial) 0L else NA_integer_
#   flag2(window_sequence(group_sizes, after, ascending = FALSE, partial = partial),
#         g = g, fill = start_fill, n = -1L)
# }
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
  # Without vctrs
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
  which_size_gt_zero <- which(sizes > 0L)
  which_size_zero <- which(sizes == 0L)
  sizes <- sizes[which_size_gt_zero]
  seq_id_GRP <- sorted_group_id_to_GRP(seq_id(sizes),
                                       n_groups = length(sizes),
                                       group_sizes = sizes)
  ind <- collapse::gsplit(sequence(sizes,
                                   from = (seq_along(sizes) - sizes + 1L),
                                   by = 1L),
                          g = seq_id_GRP)
  # vctrs::vec_chop(x, indices = ind)
  # which_size_gt_zero <- which(sizes > 0L)
  # seq_id_GRP <- sorted_group_id_to_GRP(seq_id(.subset(sizes, which_size_gt_zero)),
  #                                      n_groups = length(sizes),
  #                                      group_sizes = sizes)
  # ind <- collapse::gsplit(sequence(.subset(sizes, which_size_gt_zero),
  #                                  from =  (seq_along(x) - sizes + 1L),
  #                                  by = 1L),
  #                         g = seq_id_GRP)
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
flag2 <- function(x, n = 1L, g = NULL, ...){
  if (is.null(x)){
    return(NULL)
  }
  n <- as.integer(sign(n) * min(length(x), abs(n)))
  sorted_group_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = FALSE)
  g <- sorted_group_info[["GRP"]]
  sorted_g <- sorted_group_info[["sorted_GRP"]]
  sorted_x <- sorted_group_info[["x"]]
  is_sorted <- sorted_group_info[["sorted"]]
  out <- collapse::flag(sorted_x, n = n, g = sorted_g, ...)
  if (!is_sorted){
    out <- greorder2(out, g = g)
  }
  out
}
fdiff2 <- function(x, n = 1L, g = NULL, ...){
  x - flag2(x, n = n, g = g, ...)
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
