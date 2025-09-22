# Vctrs style rolling chop
roll_chop <- function(x, sizes = rep_len(1L, cheapr::vector_length(x))){
  x_size <- cheapr::vector_length(x)
  out_length <- length(sizes)
  if (x_size != length(sizes)){
    stop("length of x must equal length of sizes")
  }
  sizes <- as.integer(sizes)
  which_size_gt_zero <- which(sizes > 0L)
  which_size_zero <- which(sizes == 0L)
  sizes_gt_zero <- sizes[which_size_gt_zero]
  seq_id_GRP <- sorted_group_id_to_GRP(cheapr::seq_id(sizes_gt_zero),
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

# No partial argument, just a weights extension to data.table::frollsum()
frollsum3 <- function(x, n, weights = NULL, adaptive = FALSE, ...){

  if (adaptive){
    n <- cheapr::na_replace(n, length(x) + 1L)
  }

  if (!is.null(weights)){
    x <- x * weights
  }
  data.table::frollsum(x, n = n, adaptive = adaptive, ...)
}
frollmean3 <- function(x, n, weights = NULL, adaptive = FALSE, ...){

  if (adaptive){
    n <- cheapr::na_replace(n, length(x) + 1L)
  }

  if (!is.null(weights)){
    x <- x * weights
    weights[is.na(x)] <- NA_real_
    out <- data.table::frollsum(x, n = n, adaptive = adaptive, ...) /
      data.table::frollsum(weights, n = n, adaptive = adaptive, ...)
  } else {
    out <- data.table::frollmean(x, n = n, adaptive = adaptive, ...)
  }
  out
}
# For fun.. A (mostly) base R NA locf/locb
# Just to show how useful a simple function like consecutive_na_id() can be
# na_locf <- function(x){
#   na_id <- cpp_consecutive_na_id(x, TRUE)
#   i <- seq_along(x) - na_id
#   i[which(i == 0L)] <- NA_integer_
#   x[i]
#   # na_id <- cpp_consecutive_na_id(x, TRUE)
#   # i <- seq_len(cheapr::vector_length(x)) - na_id
#   # i[which(i == 0L)] <- NA_integer_
#   # sset(x, i)
#   # # x[seq_along(x) - na_id]
# }
# na_focb <- function(x){
#   na_id <- cpp_consecutive_na_id(x, FALSE)
#   i <- seq_along(x) + na_id
#   i[which(i == 0L)] <- NA_integer_
#   x[i]
#   # na_id <- cpp_consecutive_na_id(x, FALSE)
#   # i <- seq_len(cheapr::vector_length(x)) + na_id
#   # i[which(i == 0L)] <- NA_integer_
#   # sset(x, i)
#   # # x[seq_along(x) + na_id]
# }
# Mostly base R rolling chop
# roll_chop3 <- function(x, sizes = collapse::alloc(1L, cheapr::vector_length(x))){
#   x_size <- cheapr::vector_length(x)
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
#       out[[i]] <- sset(x, seq_len(.subset(sizes, i)) + (i - .subset(sizes, i)))
#     }
#   }
#   out
# }
