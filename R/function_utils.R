#' @noRd

is_strictly_increasing <- function(x){
  isTRUE(all(x == cummax(x)))
}

lump_categories <- function(x, n = 10, factor = TRUE,
                            sort = c("frequency", "values"),
                            descending = TRUE,
                            drop_levels = FALSE
                            # na_exclude = TRUE
                            ){
  sort <- match.arg(sort)
  y <- as.character(x)
  if (is.factor(x)){
    x_unique <- levels(x)
    if (descending){
      x_unique_sorted <- rev(x_unique)
    } else {
      x_unique_sorted <- x_unique
    }
  } else {
    x_unique <- collapse::funique(x[!is.na(x)])
    # x_unique_sorted <- sort(x_unique, decreasing = descending)
    x_unique_sorted <- x_unique[radix_order(x_unique, decreasing = descending)]
  }
  if (sort == "frequency"){
    # ranked_categories <- sort(table(x), decreasing = descending)
    # ranked_categories <- collapse::qtab(x, sort = FALSE, dnn = NULL, na.exclude = na_exclude)
    ranked_categories <- collapse::qtab(x, sort = FALSE, dnn = NULL, na.exclude = TRUE)
    ranked_categories <- ranked_categories[radix_order(ranked_categories,
                                                       decreasing = descending)]
    top_n_categories <- vec_head(ranked_categories, n = n)
    top_n_categories <- names(top_n_categories[radix_order(top_n_categories,
                                                           decreasing = descending)])
  } else {
    top_n_categories <- vec_head(x_unique_sorted, n = n)
  }
  top_n_category_levels <- as.character(top_n_categories)
  if (length(x_unique) > length(top_n_categories)){
    y[(!y %in% top_n_category_levels) & !is.na(y)] <- "Other"
    top_n_category_levels <- collapse::funique(c(top_n_category_levels, "Other"))
  }
  if (factor){
    # if (na_exclude){
    #   na_factor_exclude <- NA
    # } else {
    #   na_factor_exclude <- NULL
    # }
    # y <- factor(y, levels = top_n_category_levels, exclude = na_factor_exclude)
    y <- ffactor(y, levels = top_n_category_levels)
    if (drop_levels){
      y <- droplevels(y)
    }
  }
  y
}
# Memory efficient n unique
n_unique <- function(x, na.rm = FALSE){
  if (is_interval(x)){
    dplyr::n_distinct(x, na.rm = na.rm)
  } else {
    collapse::fndistinct(x, na.rm = na.rm)
  }
}

is_length_one <- function(x){
  isTRUE(length(x) == 1)
}

# Weighted geometric mean
geometric_mean <- function(x, weights = NULL, na.rm = FALSE){
  if (!is.null(weights)){
    exp(stats::weighted.mean(log(x), w = weights, na.rm = na.rm))
  } else {
    exp(mean(log(x), na.rm = na.rm))
  }
}
# Weighted harmonic mean
harmonic_mean <- function(x, weights = NULL, na.rm = FALSE){
  if (!is.null(weights)){
    1 / stats::weighted.mean(1/x, w = weights, na.rm = na.rm)
  } else {
    1 / mean(1/x, na.rm = na.rm)
  }
}

# Transform variables using tidy data masking
tidy_transform_names <- function(data, ...){
  names(
    summarise_list(
      vec_head(safe_ungroup(data), n = 1L), ...,
      fix.names = TRUE
    )
  )
}
# tidy_transform_names2 <- function(data, ...){
#   names(dplyr::transmute(data, ...))
# }
# Select variables utilising tidyselect notation
# Original version
# tidy_select_pos <- function(data, ...){
#   tidyselect::eval_select(rlang::expr(c(...)), data = data)
# }

# Fast way of getting named col positions
col_select_pos <- function(data, .cols = character(0)){
  data_nms <- names(data)
  nm_seq <- seq_along(data_nms)
  # Method for when cols is supplied
  if (is.numeric(.cols)){
    rng_sign <- check_range_sign(.cols)
    if (rng_sign == -1){
      .cols <- setdiff(nm_seq, abs(.cols))
    } else {
      .cols <- .cols[.cols != 0]
    }
    out <- match(.cols, nm_seq, nomatch = NA_integer_)
  } else if (is.character(.cols)){
    out <- match(.cols, data_nms, nomatch = NA_integer_)
  } else {
    stop(".cols must be a numeric or character vector")
  }
  out_na <- is.na(out)
  if (any(out_na)){
    first_na_col <- .cols[which(out_na)[1L]]
    if (is.numeric(first_na_col)){
      stop(paste("Location", first_na_col, "doesn't exist",
                 sep = " "))
    } else {
      stop(paste("Column", first_na_col, "doesn't exist",
                 sep = " "))
    }
  }
  out_nms <- names(.cols)
  if (is.null(out_nms)){
    names(out) <- data_nms[out]
  } else {
    es <- !nzchar(out_nms)
    out_nms[es] <- data_nms[out[es]]
    # out_nms[out_nms == ""] <- data_nms[out[out_nms == ""]]
    names(out) <- out_nms
  }
  out
}
# Tidyselect col names
col_select_names <- function(data, ..., .cols = NULL){
  names(col_select_pos(data, ..., .cols = .cols))
}
# (Internal) Fast col rename
col_rename <- function(data, .cols = integer(0)){
  .cols <- .cols[nzchar(names(.cols))]
  out_nms <- names(.cols)
  if (length(out_nms) == 0L){
    return(data)
  }
  data_nms <- names(data)
  if (is.character(.cols)){
    pos <- setnames(match(.cols, data_nms),
                    out_nms)
  } else {
    pos <- .cols
  }
  pos_nms <- names(pos)
  renamed <- data_nms[pos] != pos_nms
  names(data)[pos[renamed]] <- out_nms[renamed]
  data
}
# Tidyselect col positions with names
tidy_select_pos <- function(data, ..., .cols = NULL){
  data_nms <- names(data)
  check_cols(dots_length(...), .cols = .cols)
  # Method for when cols is supplied
  if (!is.null(.cols)){
    out <- col_select_pos(data, .cols)
  } else {
    # If exact cols are specified, faster to use
    # col_select_pos()
    quo_select_info <- quo_select_info(enquos(...), data)
    quo_text <- quo_select_info[["quo_text"]]
    all_char <- all(quo_select_info[["is_char_var"]])
    all_num <- all(quo_select_info[["is_num_var"]])
    if (all_char){
      out <- col_select_pos(data, quo_text)
    } else if (all_num){
      pos <- as.double(quo_text)
      names(pos) <- names(quo_text)
      out <- col_select_pos(data, pos)
      # Otherwise we use tidyselect
    } else {
      out <- tidyselect::eval_select(rlang::expr(c(...)), data = data)
    }
    if (all_char || all_num){
      is_dup <- collapse::fduplicated(list(names(out), unname(out)))
      out <- out[!is_dup]
      if (anyDuplicated(names(out)) > 0){
        # Use tidyselect for error
        tidyselect::eval_select(rlang::expr(c(...)), data = data)
      }
    }
  }
  out
}
# Select variables utilising tidyselect notation
tidy_select_names <- function(data, ..., .cols = NULL){
  names(tidy_select_pos(data, ..., .cols = .cols))
}
# Basic tidyselect information for further manipulation
# Includes output and input names which might be useful
tidy_select_info <- function(data, ..., .cols = NULL){
  data_nms <- names(data)
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  out_nms <- names(pos)
  pos <- unname(pos)
  in_nms <- data_nms[pos]
  renamed <- is.na(match(out_nms, data_nms) != pos)
  list("pos" = pos,
       "out_nms" = out_nms,
       "in_nms" = in_nms,
       "renamed" = renamed)
}
# Updated version of transmute using mutate
transmute2 <- function(data, ..., .by = NULL){
  group_vars <- get_groups(data, .by = {{ .by }})
  out <- mutate2(data, ..., .by = {{ .by }}, .keep = "none")
  out_nms <- tidy_transform_names(data, ...)
  fselect(out, .cols = c(group_vars, out_nms))
}
# mutate with a special case when all expressions are just selected columns.
mutate2 <- function(data, ..., .by = NULL,
                    .keep = c("all", "used", "unused", "none"),
                    .before = NULL,
                    .after = NULL){
  dots <- enquos(...)
  before_quo <- enquo(.before)
  after_quo <- enquo(.after)
  .keep <- rlang::arg_match0(.keep, c("all", "used", "unused", "none"))
  has_dup_names <- anyDuplicated(names(data)) > 0
  quo_info <- quo_mutate_info(dots, data)
  quo_nms <- quo_info[["quo_nms"]]
  quo_text <- quo_info[["quo_text"]]
  is_identity <- quo_info[["is_identity"]]
  if (all(is_identity) &&
      !has_dup_names &&
    .keep %in% c("all", "none") &&
    rlang::quo_is_null(before_quo) &&
    rlang::quo_is_null(after_quo)){
    if (.keep == "all"){
      data
    } else {
      group_vars <- get_groups(data, .by = {{ .by }})
      other_vars <- intersect(names(data), quo_text)
      other_vars <- setdiff(other_vars, group_vars)
      out_vars <- intersect(names(data), c(group_vars, other_vars))
      collapse::fselect(data, out_vars)
    }
  } else {
    dplyr::mutate(data, !!!dots, .keep = .keep,
                  .before = !!before_quo,
                  .after = !!after_quo,
                  .by = {{ .by }})
  }
}
# This works like dplyr::summarise but evaluates each expression
# independently, and on the ungrouped data.
# The result is always a list.
# Useful way of returning the column names after supplying data-masking variables too

# KEEP THIS FOR NOW

# summarise_list <- function(data, ..., fix.names = TRUE){
#   if (inherits(data, "grouped_df")) data <- dplyr::ungroup(data)
#   quo_list <- rlang::eval_tidy(enquos(...), data)
#   out <- lapply(quo_list, function(quo) dplyr_summarise(data, !!quo))
#   # Remove NULL entries
#   out_sizes <- lengths(out, use.names = FALSE)
#   if (all(out_sizes == 0)){
#     return(setnames(list(), character(0)))
#   }
#   out <- out[out_sizes > 0]
#   # Outer names
#   outer_nms <- names(out)
#   # Lengths of each list
#   out_sizes <- lengths(out)
#   # Expand list elements that have multiple elements
#   which_less_than2 <- which(out_sizes < 2)
#   which_greater_than1 <- which(out_sizes > 1)
#   out1 <- out[which_less_than2]
#   out2 <- out[which_greater_than1]
#   out_order <- radix_order(c(which_less_than2, rep(which_greater_than1,
#                                              out_sizes[which_greater_than1])))
#   outer_nms <- c(outer_nms[which_less_than2],
#                  rep(outer_nms[which_greater_than1],
#                      out_sizes[which_greater_than1]))[out_order]
#   out2 <- unlist(out2, recursive = FALSE)
#   out1 <- unlist(unname(out1), recursive = FALSE)
#   inner_nms <- c(names(out1), names(out2))[out_order]
#   out <- c(out1, out2)[out_order]
#   out_lengths <- lengths(out)
#   if (fix.names){
#     final_nms <- character(length(out))
#     for (i in seq_along(out)){
#       if (outer_nms[[i]] == ""){
#         final_nms[[i]] <- inner_nms[[i]]
#       } else {
#         final_nms[[i]] <- outer_nms[[i]]
#       }
#     }
#     names(out) <- final_nms
#   }
#   out
# }
summarise_list <- function(data, ..., fix.names = TRUE){
  data <- safe_ungroup(data)
  dots <- enquos(...)
  quo_info <- quo_summarise_info(dots, data)
  quo_text <- quo_info[["quo_text"]]
  is_identity <- quo_info[["is_identity"]]
  # Check for dots referencing exact cols (identity)
  out <- vector("list", length(quo_text))
  quo_identity_pos <- which(is_identity)
  quo_data_nms <- quo_text[quo_identity_pos]

  quo_other_pos <- which(!is_identity)
  data_pos <- match(quo_data_nms, names(data))
  # Where expressions are identity function, just select
  for (i in seq_along(quo_identity_pos)){
    out[[quo_identity_pos[[i]]]] <- collapse::get_vars(data, vars = data_pos[[i]],
                                                       return = "data",
                                                       regex = FALSE,
                                                       rename = TRUE)
  }
  # For all other expressions, use reframe()
  if (length(quo_other_pos) > 0L){
    out[quo_other_pos] <- lapply(dots[quo_other_pos],
                                 function(quo) dplyr_summarise(data, !!quo))
  }
  names(out) <- quo_info[["quo_nms"]]
  # Remove NULL entries
  out_sizes <- lengths(out, use.names = FALSE)
  if (all(out_sizes == 0)){
    return(setnames(list(), character(0)))
  }
  # The below code takes columns of data frame summaries
  # and flattens them into separate list elements basically.
  out <- out[out_sizes > 0]
  # Outer names
  outer_nms <- names(out)
  # Lengths of each list
  out_sizes <- lengths(out)
  # Expand list elements that have multiple elements
  which_less_than2 <- which(out_sizes < 2)
  which_greater_than1 <- which(out_sizes > 1)
  out1 <- out[which_less_than2]
  out2 <- out[which_greater_than1]
  out_order <- order(c(which_less_than2, rep(which_greater_than1,
                                             out_sizes[which_greater_than1])))
  outer_nms <- c(outer_nms[which_less_than2],
                 rep(outer_nms[which_greater_than1],
                     out_sizes[which_greater_than1]))[out_order]
  out2 <- unlist(out2, recursive = FALSE)
  out1 <- unlist(unname(out1), recursive = FALSE)
  inner_nms <- c(names(out1), names(out2))[out_order]
  out <- c(out1, out2)[out_order]
  out_lengths <- lengths(out, use.names = FALSE)
  # Fix names so that list names are always output names and not empty
  if (fix.names){
    final_nms <- character(length(out))
    for (i in seq_along(out)){
      if (outer_nms[[i]] == ""){
        final_nms[[i]] <- inner_nms[[i]]
      } else {
        final_nms[[i]] <- outer_nms[[i]]
      }
    }
    names(out) <- final_nms
  }
  out
}
# This is like summarise_list but works on grouped data
# summarise_list3 <- function(data, ..., fix.names = FALSE){
#   quo_list <- rlang::eval_tidy(enquos(...), data)
#   out <- purrr::map(quo_list, function(quo) dplyr_summarise(data, !!quo))
#   if (fix.names){
#     nms_out_nms <- names(out)
#     if (is.null(nms_out_nms)) nms_out_nms <- character(length(out))
#     out_lengths <- collapse::vlengths(out)
#     out_nms <- purrr::flatten_chr(purrr::map2(out, out_lengths, function(x, y) names(x)[y]))
#     final_nms <- character(length(out))
#       for (i in seq_along(out)){
#         if (nms_out_nms[i] == ""){
#           final_nms[i] <- out_nms[i]
#         } else {
#           final_nms[i] <- nms_out_nms[i]
#         }
#       }
#     names(out) <- final_nms
#   }
#   out
# }

# N expressions in ...
dots_length <- function(...){
  nargs()
}
# Greatest common divisor (Euclidean algorithm)
# Function contributed by 'Matthew Lundberg' at:
# https://stackoverflow.com/questions/21502181/finding-the-gcd-without-looping-r
gcd <- function(x, y) {
  r <- x %% y
  ifelse(r, gcd(y, r), y)
}
# Least common multiple (using Euclidean algorithm)
lcm <- function(x, y){
  ( abs(x) / gcd(x, y) ) * abs(y)
}
# Normalise weights to sum to length x
normalise_weights <- function(x, weights, na.rm = TRUE){
  x_na <- is.na(x)
  weights[x_na] <- NA_real_
  if (na.rm){
    n <- sum(!x_na)
  } else {
    n <- length(x)
  }
  out <- n * ( weights/sum(weights, na.rm = na.rm))
  out[weights == 0] <- 0
  out
}

# Exponentially weighted moving average
ewma <- function (x, ratio) {
  c(stats::filter(x * ratio, 1 - ratio, "recursive", init = x[1]))
}
# Append columns from y to x using a common ID and a sql type join.
tbl_append <- function(x, y, id, keep_id = TRUE, y_suffix = ".x",
                       side = c("left", "right"), message = TRUE){
  side <- match.arg(side)
  if (missing(id)){
    id <- ".join.index"
    x[[".join.index"]] <- seq_len(nrow(x))
    y[[".join.index"]] <- seq_len(nrow(y))
  }
  if (n_unique(x[[id]]) != nrow(x)) stop("id must uniquely and commonly identify rows in x and y")
  if (n_unique(y[[id]]) != nrow(y)) stop("id must uniquely and commonly identify rows in x and y")
  common_cols <- setdiff(intersect(names(x), names(y)), id)
  # Join new variables onto original data
  if (side == "left"){
    init_names <- names(x)
    z <- dplyr::left_join(x, y, by = id, suffix = c("", y_suffix))
  } else {
    init_names <- names(y)
    z <- dplyr::right_join(x, y, by = id, suffix = c(y_suffix, ""))
  }
  if (!keep_id){
    z <- dplyr::select(z, -dplyr::all_of(id))
  }
  if (message){
    new_renamed_cols <- setdiff(names(z), init_names)
    message(paste0("New columns added:\n", paste(new_renamed_cols, collapse = ", ")))
  }
  z
}
# Bind columns from y to x without destroying names in x
tbl_append2 <- function(x, y,
                        suffix = ".x",
                        # .name_repair = function(x) paste0(x, ".x"),
                        quiet = FALSE){
  if (missing(y)) return(x)
  x_nms <- names(x)
  y_nms <- names(y)
  common_cols <- intersect(x_nms, y_nms)
  suffix <- rep_len(suffix, length(common_cols))
  new_col_nms <- paste0(common_cols, suffix)
  y_nms[y_nms %in% common_cols] <- new_col_nms
  names(y) <- y_nms
  z <- dplyr::bind_cols(x, y)
  if (!quiet){
    new_cols <- names(z)[seq_len(ncol(y)) + ncol(x)]
    message(paste0("New columns added:\n", paste(new_cols, collapse = ", ")))
  }
  z
}
# Fast top n
top_n <- function(x, n, na.rm = FALSE, with_ties = TRUE, sort = TRUE){
  n <- min(length(x), n)
  if (na.rm) x <- x[!is.na(x)]
  x_order <- radix_order(x, decreasing = TRUE)
  x_sorted <- x[x_order]
  if (sort) x <- x_sorted
  top_n <- x_sorted[seq_len(n)]
  top_n2 <- collapse::funique(top_n, sort = FALSE)
  if (with_ties){
    out <- x[x %in% top_n2]
  } else {
    if (sort){
      out <- top_n
    } else {
      out <- x[x %in% top_n2]
      ranks <- radix_order(out, na.last = TRUE, decreasing = TRUE)
      which_ranks <- which(ranks <= n)
      out <- out[which_ranks[radix_order(ranks[which_ranks], na.last = TRUE,
                                         decreasing = FALSE)]]
    }
  }
  out
}
# This function is for functions like count() where extra groups need
# to be created
get_group_info <- function(data, ..., type = c("select", "data-mask"),
                           .by = NULL){
  type <- rlang::arg_match0(type, c("select", "data-mask"))
  n_dots <- dots_length(...)
  group_vars <- get_groups(data, {{ .by }})
  if (n_dots == 0){
    extra_groups <- character(0)
  } else {
    if (type == "select"){
      extra_groups <- tidy_select_names(data, ...)
    } else {
      extra_groups <- tidy_transform_names(data, ...)
    }
  }
  extra_groups <- setdiff(extra_groups, group_vars)
  all_groups <- c(group_vars, extra_groups)
  list("dplyr_groups" = group_vars,
       "extra_groups" = extra_groups,
       "all_groups" = all_groups)
}
group_info <- function(data, ..., .by = NULL, .cols = NULL,
                       ungroup = TRUE, rename = TRUE,
                       dots_type = "data-mask",
                       unique_groups = TRUE){
  n_dots <- dots_length(...)
  check_cols(n_dots = n_dots, .cols = .cols)
  group_vars <- get_groups(data, {{ .by }})
  extra_groups <- character(0)
  if (ungroup){
    out <- safe_ungroup(data)
  } else {
    out <- data
  }
  # Data-masking for dots expressions
  if (n_dots > 0){
    if (dots_type == "data-mask"){
      if (ungroup){
        out <- mutate2(out, ...)
      } else {
        out <- mutate2(out, ..., .by = {{ .by }})
      }
      extra_groups <- tidy_transform_names(data, ...)
    } else {
      extra_groups <- tidy_select_names(data, ...)
      out <- frename(out, ...)
    }
  }
  if (!is.null(.cols)){
    pos <- col_select_pos(out, .cols = .cols)
    group_pos <- pos %in% match(group_vars, names(data))
    # Remove group vars from pos
    pos <- pos[!group_pos]
    if (rename){
      extra_groups <- names(pos)
      renamed <- is.na(match(extra_groups, names(out)) != pos)
      renamed_pos <- pos[renamed]
      out <- frename(out, .cols = renamed_pos)
      # if (length(renamed_pos) > 0L){
      #   names(out)[renamed_pos] <- extra_groups[renamed]
      # }
    } else {
      extra_groups <- names(out)[pos]
    }
  }
  if (unique_groups){
    extra_groups <- extra_groups[match(extra_groups, group_vars, 0L) == 0L]
    all_groups <- c(group_vars, extra_groups)
  } else {
    all_groups <- c(group_vars, extra_groups[match(extra_groups, group_vars, 0L) == 0L])
  }
  list("data" = out,
       "dplyr_groups" = group_vars,
       "extra_groups" = extra_groups,
       "all_groups" = all_groups)
}


# Faster dot nms
dot_nms <- function(..., use.names = FALSE){
  unlist(lapply(substitute(alist(...))[-1L], deparse),
         recursive = FALSE, use.names = use.names)
}
# Default arguments
match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))

  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )


  match.call(sys.function(sys.parent()), call)
}

# Fast factor()
ffactor <- function(x, levels = NULL, ordered = FALSE, na.exclude = TRUE){
  # Bug-fix when sort is TRUE and length(x) == 0 in qf()
  if (length(x) == 0L){
    out <- factor(x, levels = levels, ordered = ordered, exclude = NULL)
  } else if (is.null(levels)){
    # If no supplied levels, collapse can be used safely
    out <- collapse::qF(x, sort = TRUE, ordered = ordered,
                        na.exclude = na.exclude)
  } else {
    levels <- as.character(levels)
      if (na.exclude){
        exclude <- NA
      } else {
        exclude <- NULL
      }
    x_unique <- collapse::funique(x, sort = TRUE)
    if (na.exclude) x_unique <- x_unique[!is.na(x_unique)]
    # This check is to ensure that if there are more or less
    # supplied levels then unique categories, then base factor()
    # is used because collapse::qF() only creates categories
    # that exist in the data
    if (isTRUE(all.equal(as.character(x_unique), levels))){
      out <- collapse::qF(x, sort = TRUE, ordered = ordered,
                          na.exclude = na.exclude)
    } else {
      out <- factor(x, levels = levels, ordered = ordered, exclude = exclude)
    }
  }
  out
}
# Checks if dataset has variable named "n" and adds n
# Until it finds unique var name.
# Recursive implementation.
new_n_var_nm <- function(data, check = "n"){
  data_nms <- names(data)
  if (is.null(data_nms)) data_nms <- data
  if (check %in% data_nms){
    new_n_var_nm(data, check = paste0(check, "n"))
  } else {
    check
  }
}
# Checks if dataset has a variable name and returns unique name
new_var_nm <- function(data, check = ".group.id"){
  data_nms <- names(data)
  if (is.null(data_nms)) data_nms <- data
  i <- 1L
  grp_nm <- check
  while (check %in% data_nms){
    i <- i + 1L
    check <- paste0(grp_nm, i)
  }
  return(check)
}
# Recycle arguments
recycle_args <- function (..., length = NULL, use.names = FALSE){
  dots <- list(...)
  missing_length <- is.null(length)
  if (missing_length) {
    recycle_length <- max(lengths(dots, use.names = FALSE))
  }
  else {
    recycle_length <- length
  }
  if (missing_length && base::length(unique(lengths(dots, use.names = FALSE))) == 1L){
    out <- dots
  }
  else {
    out <- lapply(dots, function(x) rep_len(x, recycle_length))
  }
  if (use.names){
    names(out) <- dot_nms(...)
  }
  out
}
# Convenience function to set names without stats namespace call
setnames <- function(object = nm, nm){
  names(object) <- nm
  object
}
# Row products
rowProds <- function(x, na.rm = FALSE, dims = 1L){
  exp(rowSums(log(x), na.rm = na.rm, dims = dims))
}
# Wrapper around order() to use radix order
radix_order <- function(x, na.last = TRUE, ...){
  order(x, method = "radix", na.last = na.last, ...)
}
# Wrapper around order() to use radix sort
radix_sort <- function(x, na.last = TRUE, ...){
  # sort(x, na.last = na.last, ..., method = "radix")
  x[radix_order(x, na.last = na.last, ...)]
}
# Creates a sequence of ones.
# This is used primarily for sums
seq_ones <- function(length){
  if (length <= .Machine$integer.max){
    alloc(1L, length)
  } else {
    alloc(1, length)
  }
}
# Drop leading zeroes
drop_leading_zeros <- function(x, sep = "."){
  pattern <- paste0("^([^[:digit:]]{0,})0{1,}\\", sep, "{1}")
  sub(pattern, paste0("\\1", sep), x, perl = TRUE)
}

# A wrapper around sample to account for length 1 vectors.
# This is a well known problem (and solution)
sample2 <- function(x, size = length(x), replace = FALSE, prob = NULL){
  x[sample.int(length(x), size = size, replace = replace, prob = prob)]
}

setv <- getFromNamespace("setv", "collapse")
alloc <- getFromNamespace("alloc", "collapse")
fcumsum <- getFromNamespace("fcumsum", "collapse")
# set <- getFromNamespace("set", "data.table")
fsum <- getFromNamespace("fsum", "collapse")
fmin <- getFromNamespace("fmin", "collapse")
fmax<- getFromNamespace("fmax", "collapse")
fmean <- getFromNamespace("fmean", "collapse")
fmode <- getFromNamespace("fmode", "collapse")
fsd <- getFromNamespace("fsd", "collapse")
fvar <- getFromNamespace("fvar", "collapse")
fmedian <- getFromNamespace("fmedian", "collapse")
ffirst <- getFromNamespace("ffirst", "collapse")
flast <- getFromNamespace("flast", "collapse")
# Some future utils for counts and weights..
# wt_fun <- function(wt){
#   rlang::expr(sum(!!enquo(wt), na.rm = TRUE))
# }
# data %>% summarise(!!wt_fun(!!enquo(wt)))
# quo_name(enquo(wt))
# quo_is_null()

CJ <- getFromNamespace("CJ", "data.table")
# Ccj <- getFromNamespace("Ccj", "data.table")

is_whole_number <- function(x, na.rm = FALSE,
                            tol = sqrt(.Machine$double.eps)){
  if (is.integer(x)) return(TRUE) # If already integer then true
  if (na.rm){
    x <- x[!is.na(x)]
  }
  if (any(is.infinite(x))) return(FALSE)
  # if (length(x) == 0L) return(FALSE) # If length is 0 then false
  # all.equal(x, as.integer(x), check.attributes = FALSE)
  # isTRUE((sum(x %% 1) == 0))
  # x <- floor(x/10^7) * 10^7
  # x <- round(x, digits = 7)
  # all(floor(x) == x, na.rm = FALSE)
  all(abs(round(x) - x) < tol, na.rm = FALSE)
}
pair_unique <- function(x, y){
  ( ( (x + y + 1) * (x + y) ) / 2 ) + x
}
vec_slice2 <- function(x, i){
  if (is_df(x)){
    return(df_row_slice(x, i))
  }
  if (is_interval(x)){
    if (is.logical(i)){
      i <- which(i)
    }
    vctrs::vec_slice(x, i)
  } else {
    collapse::ss(x, i)
  }
}
# Vctrs version of utils::head/tail
vec_head <- function(x, n = 1L){
  if (length(n) != 1L){
    stop("n must be of length 1.")
  }
  N <- vctrs::vec_size(x)
  if (n >= 0){
    size <- min(n, N)
  } else {
    size <- max(0L, N + n)
  }
  vctrs::vec_slice(x, seq_len(size))
}
vec_tail <- function(x, n = 1L){
  if (length(n) != 1L){
    stop("n must be of length 1.")
  }
  N <- vctrs::vec_size(x)
  if (n >= 0){
    size <- min(n, N)
  } else {
    size <- max(0L, N + n)
  }
  vctrs::vec_slice(x, seq.int(from = N - size + 1L, by = 1L, length.out = size))
}
# Returns the length or nrows (if list or df)
vec_length <- function(x){
  if (is.list(x)){
    if (is_df(x)){
      out <- nrow2(x)
    } else {
      out <- collapse::vlengths(x, use.names = FALSE)
      nunique <- collapse::fnunique(out)
      if (nunique > 1L){
        stop("x must be a vector, matrix, data frame or list with equal lengths")
      } else {
        out <- out[nunique]
      }
      if (length(out) == 0L){
        out <- 0L
      }
      # stopifnot(isTRUE(n_unique(lens) <= 1))
      # out <- vec_head(lens, n = 1L)
    }
  } else if (is.array(x)){
    out <- dim(x)[1L]
  } else {
    out <- length(x)
  }
  out
}
# Returns the width or ncol (if list or df)
vec_width <- function(x){
  if (is.list(x)){
    if (is_df(x)){
      out <- collapse::fncol(x)
    } else {
      lens <- unique(collapse::vlengths(x, use.names = FALSE))
      if (length(lens) > 1L){
        stop("x must be a vector, matrix, data frame or list with equal lengths")
      }
      out <- collapse::fncol(x)
    }
  } else if (is.array(x)) {
    out <- dim(x)[2L]
  } else {
    out <- collapse::fncol(x)
  }
  out
}
getFromNamespace <- function(x, ns, pos = -1, envir = as.environment(pos)){
  if (missing(ns)) {
    nm <- attr(envir, "name", exact = TRUE)
    if (is.null(nm) || !startsWith(nm, "package:"))
      stop("environment specified is not a package")
    ns <- asNamespace(substring(nm, 9L))
  }
  else ns <- asNamespace(ns)
  get(x, envir = ns, inherits = FALSE)
}
# Checks whether dots are empty or contain NULL
# Returns TRUE if so, otherwise FALSE
# Used primarily to speed up dplyr::select()
check_null_dots <- function(...){
  squashed_quos <- rlang::quo_squash(enquos(...))
  length(squashed_quos) == 0L ||
    (length(squashed_quos) == 1L &&
       rlang::quo_is_null(squashed_quos[[1L]]))
       # is.null(rlang::quo_get_expr(squashed_quos[[1L]])))
}
# Wrapper around expand.grid without factors and df coercion
# Sorting mimics CJ()
# Overhead is small for small joins
CJ2 <- function(X){
  if (!is.list(X)) stop("X must be a list")
  nargs <- length(X)
  if (nargs == 0L){
    return(list())
  }
  if (nargs == 1L){
    return(list(X[[1L]]))
  }
  out <- vector("list", nargs)
  iArgs <- seq_len(nargs)
  rep.fac <- 1L
  d <- lengths(X, use.names = FALSE)
  orep <- prod(d)
  if (orep == 0L){
    for (i in iArgs){
      out[[i]] <- X[[i]][FALSE]
    }
  }
  else {
    for (i in seq.int(from = nargs, to = 1L, by = -1L)) {
      x <- X[[i]]
      nx <- length(X[[i]])
      orep <- orep/nx
      x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac, nx)), orep)]
      out[[i]] <- x
      rep.fac <- rep.fac * nx
    }
  }
  out
}

quo_null <- function(quos){
  vapply(quos, FUN = rlang::quo_is_null,
         FUN.VALUE = logical(1))
}
expr_nms <- function(exprs){
    vapply(exprs,
           FUN = rlang::expr_name,
           FUN.VALUE = character(1))

}
quo_exprs <- function(quos){
  lapply(quos, rlang::quo_get_expr)
}
# expr_identity <- function(exprs, data){
#   data_nms <- names(data)
#   quo_nms <- expr_nms(quos)
#   quo_nms %in% names(data)
# }
quo_identity <- function(quos, data){
  data_nms <- names(data)
  quo_nms <- quo_nms(quos)
  quo_nms %in% names(data)
}
# Somewhat safer check of the .by arg
# e.g mutate(group_by(iris, Species), .by = any_of("okay"))
# Should not produce an error with this check
check_by <- function(data, .by){
  if (!rlang::quo_is_null(enquo(.by))){
    if (inherits(data, "grouped_df")){
      by_nms <- tidy_select_names(data, {{ .by }})
      if (length(by_nms) > 0L){
        stop(".by cannot be used on a grouped_df")
      }
    }
  }
}
check_cols <- function(n_dots, .cols = NULL){
  if (n_dots > 0 && !is.null(.cols)){
    stop("Cannot supply variables through ... and .cols, use one argument.")
  }
}
# Quosure text/var check for select()
# NULL is removed.
quo_select_info <- function(quos, data){
  quo_nms <- names(quos)
  quo_text <- setnames(character(length(quos)), quo_nms)
  quo_is_null <- setnames(logical(length(quos)), quo_nms)
  for (i in seq_along(quos)){
    quo_text[[i]] <- deparse(rlang::quo_get_expr(quos[[i]]))
    # quo_text[[i]] <- rlang::expr_name(rlang::quo_get_expr(quos[[i]]))
    quo_is_null[[i]] <- rlang::quo_is_null(quos[[i]])
  }
  quo_text <- quo_text[!quo_is_null]
  quo_nms <- quo_nms[!quo_is_null]
  is_char_var <- quo_text %in% names(data)
  is_num_var <- quo_text %in% as.character(df_seq_along(data, "cols"))
  list(quo_nms = quo_nms,
       quo_text = quo_text,
       is_num_var = is_num_var,
       is_char_var = is_char_var)
}
# Quosure text/var check for mutate()
# unnamed NULL exprs are removed.
quo_mutate_info <- function(quos, data){
  quo_nms <- names(quos)
  quo_text <- setnames(character(length(quos)), quo_nms)
  quo_is_null <- setnames(logical(length(quos)), quo_nms)
  for (i in seq_along(quos)){
    quo_text[[i]] <- deparse(rlang::quo_get_expr(quos[[i]]))
    # quo_text[[i]] <- rlang::expr_name(rlang::quo_get_expr(quos[[i]]))
    quo_is_null[[i]] <- rlang::quo_is_null(quos[[i]]) && quo_nms[[i]] == ""
  }
  quo_text <- quo_text[!quo_is_null]
  quo_nms <- quo_nms[!quo_is_null]
  is_identity <- quo_text %in% names(data) & quo_nms == ""
  list(quo_nms = quo_nms,
       quo_text = unname(quo_text),
       is_identity = is_identity)
}
# Used only for summarise_list()
quo_summarise_info <- function(quos, data){
  quo_nms <- names(quos)
  quo_text <- setnames(character(length(quos)), quo_nms)
  quo_is_null <- setnames(logical(length(quos)), quo_nms)
  for (i in seq_along(quos)){
    quo_text[[i]] <- deparse(rlang::quo_get_expr(quos[[i]]))
    # quo_text[[i]] <- rlang::expr_name(rlang::quo_get_expr(quos[[i]]))
    quo_is_null[[i]] <- rlang::quo_is_null(quos[[i]])
  }
  quo_text <- quo_text[!quo_is_null]
  quo_nms <- quo_nms[!quo_is_null]
  is_identity <- quo_text %in% names(data)
  list(quo_nms = quo_nms,
       quo_text = unname(quo_text),
       is_identity = is_identity)
}
conditional_sort <- function(x){
  if (is.unsorted(x)){
    radix_sort(x)
  } else {
    x
  }
}
# Check if signs are all equal
# Special function to handle -0 selection
check_range_sign <- function(x){
  out <- sum(sign(1/x))
  if (abs(out) != length(x)){
    stop("Can't mix negative and positive locations")
  }
  sign(out)
}
# Collapse/vctrs style complete rate
prop_complete <- function(x, ...){
  1 - (fnmiss(x, ...) / vec_length(x))
}
# Base R version of purrr::pluck, alternative to [[
fpluck <- function(x, .cols = NULL, .default = NULL){
  if (is.null(.cols)){
   return(x)
  }
  if (length(.cols) != 1L){
    stop(".cols must have length 1")
  }
  if (is.numeric(.cols)){
    icol <- match(.cols, seq_along(x))
  } else {
    icol <- match(.cols, names(x))
  }
  # If no match just return .default
  if (is.na(icol)){
    return(.default)
  }
  x[[icol]]
}

# round down to nearest n
floor_nearest_n <- function(x, n){
  floor(x / n) * n
}
# Round up to nearest n
ceiling_nearest_n <- function(x, n){
  ceiling(x / n) * n
}
# How many 10s is a number divisible by?
log10_divisibility <- function(x){
  x[x == 0] <- 1
  floor(log10(abs(x)))
}
# Sensible rounding
pretty_floor <- function(x){
  floor_nearest_n(x, n = 10^(log10_divisibility(x)))
}
pretty_ceiling <- function(x){
  ceiling_nearest_n(x, n = 10^(log10_divisibility(x)))
}
# collapse flag/fdiff gives basically
# wrong answers if your data isn't sorted
# And yes I've read the documentation
fdiff2 <- function(x, n = 1, g = NULL, diff = 1, ...){
  if (length(x) == 1L){
    n <- 0L
    diff <- 0L
  }
  if (!is.null(g)){
    g <- GRP2(g)
  }
  if (!is.null(g) && !GRP_is_sorted(g)){
    group_order <- GRP_order(g)
    x <- x[group_order]
    group_id <- group_id_to_qg(
      GRP_group_id(g)[group_order],
      n_groups = GRP_n_groups(g)
    )
    out <- collapse::fdiff(x, n = n, g = group_id, ...)
    out <- collapse::greorder(out, g = g)
    # g <- collapse::GRP(GRP_group_id(g)[group_order])
    # out <- collapse::fdiff(x, n = n, g = g, ...)
    # out <- out[order(group_order)]
  } else {
    out <- collapse::fdiff(x, n = n, g = g, ...)
  }
  out
}
flag2 <- function(x, n = min(length(x), 1L), g = NULL, ...){
  if (!is.null(g)){
    g <- GRP2(g)
  }
  if (!is.null(g) && !GRP_is_sorted(g)){
    group_order <- GRP_order(g)
    x <- x[group_order]
    group_id <- group_id_to_qg(
      GRP_group_id(g)[group_order],
      n_groups = GRP_n_groups(g)
    )
    out <- collapse::flag(x, n = n, g = group_id, ...)
    out <- collapse::greorder(out, g = g)
    # g <- GRP_group_id(g)[group_order]
    # out <- collapse::flag(x, n = n, g = g, ...)
    # out <- out[order(group_order)]
  } else {
    out <- collapse::flag(x, n = n, g = g, ...)
  }
  out
}
fill_with_na <- function(x, n = NULL, prop = NULL){
  if (!is.null(n) && !is.null(prop)){
    stop("either n or prop must be supplied")
  }
  if (!is.null(n)){
    x[sample.int(length(x), size = n, replace = FALSE)] <- NA
  }
  if (!is.null(prop)){
    x[sample.int(length(x),
                 size = floor(prop * length(x)),
                 replace = FALSE)] <- NA
  }
  x
}
# double_precision <- function(x){
#   y <- log2(pmax(.Machine$double.xmin, abs(x)))
#   ifelse(x < 0 & floor(y) == y, 2^(y-1), 2^floor(y)) * .Machine$double.eps
# }
sqrt_double_eps <- function(){
  sqrt(.Machine$double.eps)
}
