#' @noRd

# Memory efficient n unique
n_unique <- function(x, na.rm = FALSE){
  na_offset <- 0L
  if (is_interval(x)){
    x <- interval_separate(x)
  }
  out <- collapse::fnunique(x)
  if (na.rm){
   if (is.list(x)){
     any_na <- any(collapse::missing_cases(x, prop = 1))
   } else {
     any_na <- anyNA(x)
   }
    na_offset <- as.integer(any_na)
  }
  out - na_offset
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
col_select_pos <- function(data, .cols = character()){
  data_nms <- names(data)
  nm_seq <- seq_along(data_nms)
  # Method for when cols is supplied
  if (is.numeric(.cols)){
    rng_sign <- slice_sign(.cols)
    if (rng_sign == -1){
      .cols <- nm_seq[match(nm_seq, abs(.cols), 0L) == 0L]
    } else {
      .cols <- .subset(.cols, .cols != 0)
    }
    out <- match(.cols, nm_seq)
  } else if (is.character(.cols)){
    out <- match(.cols, data_nms)
  } else {
    stop(".cols must be a numeric or character vector")
  }
  is_na <- is.na(out)
  if (any(is_na)){
    first_na_col <- .subset(.cols, .subset(cpp_which(is_na), 1L))
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
    names(out) <- .subset(data_nms, out)
  } else {
    es <- !nzchar(out_nms)
    out_nms[es] <- .subset(data_nms, .subset(out, es))
    names(out) <- out_nms
  }
  out
}
# Tidyselect col names
col_select_names <- function(data, ..., .cols = NULL){
  names(col_select_pos(data, ..., .cols = .cols))
}
# (Internal) Fast col rename
col_rename <- function(data, .cols = integer()){
  .cols <- .subset(.cols, nzchar(names(.cols)))
  out_nms <- names(.cols)
  if (length(out_nms) == 0L){
    return(data)
  }
  data_nms <- names(data)
  if (is.character(.cols)){
    pos <- add_names(match(.cols, data_nms), out_nms)
  } else {
    pos <- .cols
  }
  pos_nms <- names(pos)
  renamed <- .subset(data_nms, pos) != pos_nms
  names(data)[.subset(pos, renamed)] <- .subset(out_nms, renamed)
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
      if (anyduplicated(names(out))){
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

mutate_cols <- getFromNamespace("mutate_cols", "dplyr")
dplyr_quosures <- getFromNamespace("dplyr_quosures", "dplyr")
compute_by <- getFromNamespace("compute_by", "dplyr")

mutate_summary_ungrouped <- function(.data, ...,
                                     .keep = c("all", "used", "unused", "none"),
                                     error_call = rlang::caller_env()){
  .keep <- rlang::arg_match(.keep)
  original_cols <- names(.data)
  bare_data <- safe_ungroup(.data)
  group_data <- new_tbl(".rows" = add_attr(list(seq_len(df_nrow(bare_data))),
                                           "class",
                                           c("vctrs_list_of", "vctrs_vctr", "list")))
  by <- add_attr(
    list(
      type = "ungrouped",
      names = character(),
      data = group_data
    ),
    "class",
    "dplyr_by"
  )
  cols <- mutate_cols(bare_data, dplyr_quosures(...),
                      by = by, error_call = error_call)
  out_data <- dplyr::dplyr_col_modify(bare_data, cols)
  final_cols <- names(cols)
  used <- attr(cols, "used")
  keep_cols <- switch(.keep,
                      all = names(used),
                      none = final_cols,
                      used = names(used)[cpp_which(used)],
                      unused = names(used)[cpp_which(used, invert = TRUE)])
  out_data <- fselect(out_data, .cols = keep_cols)
  out <- list(data = out_data, cols = final_cols)
  out
}

mutate_summary_grouped <- function(.data, ...,
                                   .keep = c("all", "used", "unused", "none"),
                                   .by = NULL,
                                   error_call = rlang::caller_env()){
  .keep <- rlang::arg_match(.keep)
  original_cols <- names(.data)
  by <- compute_by(by = {{ .by }}, data = .data,
                   by_arg = ".by", data_arg = ".data")
  group_vars <- get_groups(.data, .by = {{ .by }})
  cols <- mutate_cols(.data, dplyr_quosures(...),
                      by = by, error_call = error_call)
  out_data <- dplyr::dplyr_col_modify(.data, cols)
  final_cols <- names(cols)
  used <- attr(cols, "used")
  keep_cols <- switch(.keep,
                      all = names(used),
                      none = final_cols,
                      used = names(used)[cpp_which(used)],
                      unused = names(used)[cpp_which(used, invert = TRUE)])
  # Add missed group vars
  keep_cols <- c(group_vars, keep_cols[match(keep_cols, group_vars, 0L) == 0L])
  # Match the original ordering of columns
  keep_cols <- keep_cols[radix_order(match(keep_cols, original_cols))]
  out_data <- fselect(out_data, .cols = keep_cols)
  out <- list(data = out_data, cols = final_cols)
  out
}

# Updated version of transmute using mutate
transmute2 <- function(data, ..., .by = NULL){
  group_vars <- get_groups(data, .by = {{ .by }})
  out <- mutate_summary_grouped(data, ..., .by = {{ .by }}, .keep = "none")
  fselect(out[["data"]], .cols = c(group_vars, out[["cols"]]))
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
  has_dup_names <- anyduplicated(names(data))
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
      fselect(data, .cols = out_vars)
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

summarise_list <- function(data, ..., fix.names = TRUE){
  data <- safe_ungroup(data)
  dots <- enquos(...)
  quo_info <- quo_summarise_info(dots, data)
  quo_text <- .subset2(quo_info, "quo_text")
  is_identity <- .subset2(quo_info, "is_identity")
  # Check for dots referencing exact cols (identity)
  out <- vector("list", length(quo_text))
  quo_identity_pos <- which(is_identity)
  quo_data_nms <- .subset(quo_text, quo_identity_pos)

  quo_other_pos <- which(!is_identity)
  data_pos <- match(quo_data_nms, names(data))
  # Where expressions are identity function, just select
  for (i in seq_along(quo_identity_pos)){
    out[[.subset2(quo_identity_pos, i)]] <- collapse::get_vars(data,
                                                               vars = .subset2(data_pos, i),
                                                               return = "data",
                                                               regex = FALSE,
                                                               rename = TRUE)
  }
  # For all other expressions, use reframe()
  if (length(quo_other_pos) > 0L){
    out[quo_other_pos] <- lapply(.subset(dots, quo_other_pos),
                                 function(quo) dplyr_summarise(data, !!quo))
  }
  names(out) <- .subset2(quo_info, "quo_nms")
  # Remove NULL entries
  out_sizes <- lengths(out, use.names = FALSE)
  if (sum(out_sizes) == 0){
    return(add_names(list(), character(0)))
  }
  # The below code takes columns of data frame summaries
  # and flattens them into separate list elements basically.
  out <- .subset(out, out_sizes > 0)
  # Outer names
  outer_nms <- names(out)
  # Lengths of each list
  out_sizes <- lengths(out)
  # Expand list elements that have multiple elements
  which_less_than2 <- which(out_sizes < 2)
  which_greater_than1 <- which(out_sizes > 1)
  out1 <- .subset(out, which_less_than2)
  out2 <- .subset(out, which_greater_than1)
  out_order <- order(c(which_less_than2, rep.int(which_greater_than1,
                                                 .subset(out_sizes, which_greater_than1))))
  outer_nms <- .subset(
    c(.subset(outer_nms, which_less_than2),
      rep.int(.subset(outer_nms, which_greater_than1),
              .subset(out_sizes, which_greater_than1))),
    out_order
  )
  out2 <- unlist(out2, recursive = FALSE)
  out1 <- unlist(unname(out1), recursive = FALSE)
  inner_nms <- c(names(out1), names(out2))[out_order]
  out <- .subset(c(out1, out2), out_order)
  out_lengths <- lengths(out, use.names = FALSE)
  # Fix names so that list names are always output names and not empty
  if (fix.names){
    final_nms <- character(length(out))
    for (i in seq_along(out)){
      if (.subset(outer_nms, i) == ""){
        final_nms[[i]] <- .subset(inner_nms, i)
      } else {
        final_nms[[i]] <- .subset(outer_nms, i)
      }
    }
    names(out) <- final_nms
  }
  out
}

# N expressions in ...
dots_length <- function(...){
  nargs()
}
# Greatest common divisor (Euclidean algorithm)
# Function contributed by 'Matthew Lundberg' at:
# https://stackoverflow.com/questions/21502181/finding-the-gcd-without-looping-r

# gcd <- function(x,y) {
#   r <- x%%y;
#   return(ifelse(r, gcd(y, r), y))
# }

# gcd2 <- function(x, y, tol = 0) {
#   while(isTRUE(abs(y) > tol)){
#     r <- x %% y
#     x <- y
#     y <- r
#   }
#   x
# }
# Original function I wrote using Matthew Lundberg's gcd function above
# gcd2 <- function(x){
#   if (!is.numeric(x)){
#     stop("x must be a numeric vector")
#   }
#   if (length(x) <= 1L){
#     return(x)
#   }
#   Reduce(gcd, x)
# }

# Exponentially weighted moving average
# ewma <- function (x, ratio) {
#   c(stats::filter(x * ratio, 1 - ratio, "recursive", init = x[1]))
# }

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

# tidy_group_info_tidyselect <- function(data, ..., .by = NULL, .cols = NULL,
#                                   ungroup = TRUE, rename = TRUE,
#                                   unique_groups = TRUE){
#   n_dots <- dots_length(...)
#   # check_cols(n_dots = n_dots, .cols = .cols)
#   group_vars <- get_groups(data, {{ .by }})
#   group_pos <- match(group_vars, names(data))
#   extra_groups <- character()
#   if (ungroup){
#     out <- safe_ungroup(data)
#   } else {
#     out <- data
#   }
#   # Data-masking for dots expressions
#   if (n_dots > 0){
#     extra_groups <- tidy_select_names(out, ...)
#     if (rename){
#       out <- frename(out, ...)
#     }
#   }
#   if (!is.null(.cols)){
#     extra_group_pos <- col_select_pos(out, .cols = .cols)
#     if (rename){
#       out <- col_rename(out, .cols = .cols)
#       extra_groups <- names(extra_group_pos)
#     } else {
#       extra_groups <- names(data)[extra_group_pos]
#     }
#   }
#   # Recalculate group vars in case they were renamed
#   group_vars <- names(out)[group_pos]
#   if (unique_groups){
#     extra_groups <- setdiff2(extra_groups, group_vars)
#     all_groups <- c(group_vars, extra_groups)
#   } else {
#     all_groups <- c(group_vars, setdiff2(extra_groups, group_vars))
#   }
#   address_equal <- add_names(cpp_address_equal(
#     data, df_select(safe_ungroup(out), names(data))
#   ), names(data))
#   any_groups_changed <- !all(address_equal[group_vars])
#   # any_groups_changed <- cpp_any_address_changed(df_select(safe_ungroup(data), group_vars),
#   #                                               df_select(safe_ungroup(out), group_vars))
#   list("data" = out,
#        "dplyr_groups" = group_vars,
#        "extra_groups" = extra_groups,
#        "all_groups" = all_groups,
#        "groups_changed" = any_groups_changed,
#        "address_equal" = address_equal)
# }

tidy_group_info_tidyselect <- function(data, ..., .by = NULL, .cols = NULL,
                                       ungroup = TRUE, rename = TRUE,
                                       unique_groups = TRUE){
  n_dots <- dots_length(...)
  group_vars <- get_groups(data, {{ .by }})
  group_pos <- match(group_vars, names(data))
  extra_groups <- character()
  if (ungroup){
    out <- safe_ungroup(data)
  } else {
    out <- data
  }
  extra_group_pos <- tidy_select_pos(out, ..., .cols = .cols)
  if (!rename){
    names(extra_group_pos) <- names(data)[extra_group_pos]
  }
  out <- frename(out, .cols = extra_group_pos)
  extra_groups <- names(extra_group_pos)
  # Recalculate group vars in case they were renamed
  group_vars <- names(out)[group_pos]
  address_equal <- rep_len(TRUE, df_ncol(data))
  address_equal[extra_group_pos] <-
    names(data)[extra_group_pos] == names(extra_group_pos)
  names(address_equal) <- names(data)
  any_groups_changed <- !all(address_equal[group_vars])
  if (unique_groups){
    extra_groups <- setdiff2(extra_groups, group_vars)
    all_groups <- c(group_vars, extra_groups)
  } else {
    all_groups <- c(group_vars, setdiff2(extra_groups, group_vars))
  }
  list("data" = out,
       "dplyr_groups" = group_vars,
       "extra_groups" = extra_groups,
       "all_groups" = all_groups,
       "groups_changed" = any_groups_changed,
       "address_equal" = address_equal)
}

tidy_group_info_datamask <- function(data, ..., .by = NULL,
                                     ungroup = TRUE,
                                     unique_groups = TRUE){
  n_dots <- dots_length(...)
  group_vars <- get_groups(data, {{ .by }})
  group_pos <- match(group_vars, names(data))
  extra_groups <- character()
  if (ungroup){
    out <- safe_ungroup(data)
  } else {
    out <- data
  }
  # Data-masking for dots expressions
  if (n_dots > 0){
    if (ungroup){
      out_info <- mutate_summary_ungrouped(out, ...)
    } else {
      out_info <- mutate_summary_grouped(out, ..., .by = {{ .by }})
    }
    out <- out_info[["data"]]
    extra_groups <- out_info[["cols"]]
  }
  if (unique_groups){
    extra_groups <- setdiff2(extra_groups, group_vars)
    all_groups <- c(group_vars, extra_groups)
  } else {
    all_groups <- c(group_vars, setdiff2(extra_groups, group_vars))
  }
  address_equal <- add_names(cpp_address_equal(
    data, df_select(safe_ungroup(out), names(data))
  ), names(data))
  any_groups_changed <- !all(address_equal[group_vars])
  # any_groups_changed <- cpp_any_address_changed(df_select(safe_ungroup(data), group_vars),
  #                                               df_select(safe_ungroup(out), group_vars))
  list("data" = out,
       "dplyr_groups" = group_vars,
       "extra_groups" = extra_groups,
       "all_groups" = all_groups,
       "groups_changed" = any_groups_changed,
       "address_equal" = address_equal)
}

tidy_group_info <- function(data, ..., .by = NULL, .cols = NULL,
                            ungroup = TRUE, rename = TRUE,
                            dots_type = "data-mask",
                            unique_groups = TRUE){
  check_cols(n_dots = dots_length(...), .cols = .cols)
  if (is.null(.cols) && dots_type == "data-mask"){
    tidy_group_info_datamask(data, ..., .by = {{ .by }},
                             ungroup = ungroup,
                             unique_groups = unique_groups)

  } else {
    tidy_group_info_tidyselect(data, ..., .by = {{ .by }},
                               .cols = .cols,
                               ungroup = ungroup,
                               rename = rename,
                               unique_groups = unique_groups)
  }
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
  out <- list3(...)
  lens <- cpp_lengths(out)
  uniq_lens <- collapse::fnunique(lens)
  if (is.null(length)) {
    if (length(lens)){
      recycle_length <- max(lens)
    } else {
      recycle_length <- 0L
    }
  } else {
    recycle_length <- length
  }
  recycle_length <- recycle_length * (!collapse::anyv(lens, 0L))
  recycle <- cpp_which(lens != recycle_length)
  out[recycle] <- lapply(out[recycle], function(x) rep_len(x, recycle_length))
  if (use.names){
    names(out) <- dot_nms(...)
  }
  out
}
set_recycle_args <- function(..., length = NULL, use.names = TRUE){
  if (identical(base::parent.frame(n = 1), base::globalenv())){
    stop("Users cannot use set_recycle_args from the global environment")
  }
  recycled_list <- recycle_args(..., length = length, use.names = use.names)
  out_nms <- names(recycled_list)
  for (i in seq_along(recycled_list)){
    assign(out_nms[i], recycled_list[[i]], envir = parent.frame(n = 1))
  }
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
seq_ones <- function(length){
  collapse::alloc(1L, length)
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

# double_equal <- cppdoubles::double_equal
# double_gte <- cppdoubles::double_gte
# double_lte <- cppdoubles::double_lte
# double_gt <- cppdoubles::double_gt
# double_lt <- cppdoubles::double_lt
setv <- getFromNamespace("setv", "collapse")
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
fndistinct <- getFromNamespace("fndistinct", "collapse")

are_whole_numbers <- function(x){
  if (is.integer(x)){
    return(rep_len(TRUE, length(x)))
  }
  abs(x - round(x)) < sqrt(.Machine$double.eps)
}
# Unique number from positive numbers
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
vec_slice3 <- function(x, i){
  if (is.logical(i)){
    i <- cpp_which(i)
  }
  if (is.atomic(x) || is_interval(x)){
    x[i]
  } else if (is_df(x)){
    df_row_slice(x, i)
  } else {
    collapse::ss(x, i)
  }
}
# Vctrs version of utils::head/tail
vec_head <- function(x, n = 1L){
  check_length(n, 1L)
  N <- vctrs::vec_size(x)
  if (n >= 0){
    size <- min(n, N)
  } else {
    size <- max(0L, N + n)
  }
  vctrs::vec_slice(x, seq_len(size))
}
vec_tail <- function(x, n = 1L){
  check_length(n, 1L)
  N <- vctrs::vec_size(x)
  if (n >= 0){
    size <- min(n, N)
  } else {
    size <- max(0L, N + n)
  }
  vctrs::vec_slice(x, seq.int(from = N - size + 1L, by = 1L, length.out = size))
}

# The below 2 functions CANNOT HANDLE MATRICES
# They are treated as regular vectors

# Returns the length or nrows (if list or df)
vec_length <- cpp_vector_size
# Returns the width or ncol (if list or df)
vec_width <- cpp_vector_width

# Taken from utils package
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
packageName <- function (env = parent.frame()){
  if (!is.environment(env))
    stop("'env' must be an environment")
  env <- topenv(env)
  if (!is.null(pn <- get0(".packageName", envir = env, inherits = FALSE)))
    pn
  else if (identical(env, .BaseNamespaceEnv))
    "base"
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
  nargs <- length(X)
  if (nargs <= 1L){
    return(X)
  }
  out <- vector("list", nargs)
  d <- cpp_lengths(X)
  orep <- prod(d)
  if (orep == 0L){
    for (i in seq_len(nargs)){
      out[[i]] <- .subset(.subset2(X, i), FALSE)
    }
    return(out)
  }
  rep.fac <- 1L
  for (i in seq.int(from = nargs, to = 1L, by = -1L)){
    x <- .subset2(X, i)
    nx <- .subset2(d, i)
    orep <- orep/nx
    x <- x[rep.int(rep(seq_len(nx), each = rep.fac), times = orep)]
    out[[i]] <- x
    rep.fac <- rep.fac * nx
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
  quo_text <- add_names(character(length(quos)), quo_nms)
  quo_is_null <- add_names(logical(length(quos)), quo_nms)
  for (i in seq_along(quos)){
    quo <- quos[[i]]
    quo_text[[i]] <- deparse1(rlang::quo_get_expr(quo))
    # quo_text[[i]] <- rlang::expr_name(rlang::quo_get_expr(quo))
    quo_is_null[[i]] <- rlang::quo_is_null(quo)
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
  quo_text <- add_names(character(length(quos)), quo_nms)
  quo_is_null <- add_names(logical(length(quos)), quo_nms)
  for (i in seq_along(quos)){
    quo <- quos[[i]]
    quo_text[[i]] <- deparse1(rlang::quo_get_expr(quo))
    quo_is_null[[i]] <- rlang::quo_is_null(quo) && !nzchar(quo_nms[[i]])
  }
  quo_text <- quo_text[!quo_is_null]
  quo_nms <- quo_nms[!quo_is_null]
  is_identity <- quo_text %in% names(data) & !nzchar(quo_nms)
  list(quo_nms = quo_nms,
       quo_text = unname(quo_text),
       is_identity = is_identity)
}
# Used only for summarise_list()
quo_summarise_info <- function(quos, data){
  quo_nms <- names(quos)
  quo_text <- add_names(character(length(quos)), quo_nms)
  quo_is_null <- add_names(logical(length(quos)), quo_nms)
  for (i in seq_along(quos)){
    quo <- quos[[i]]
    quo_text[[i]] <- deparse1(rlang::quo_get_expr(quo))
    quo_is_null[[i]] <- rlang::quo_is_null(quo)
  }
  quo_text <- quo_text[!quo_is_null]
  quo_nms <- quo_nms[!quo_is_null]
  is_identity <- quo_text %in% names(data)
  list(quo_nms = quo_nms,
       quo_text = unname(quo_text),
       is_identity = is_identity)
}
conditional_sort <- function(x){
  if (is_sorted(x)){
    x
  } else {
    radix_sort(x)
  }
}
# Check if signs are all equal
# Special function to handle -0 selection
# Returns 1 or -1, with special handling of -0 to allow slicing of all rows
slice_sign <- function(x){
  if (length(x)){
    rng <- collapse::frange(x, na.rm = FALSE)
  } else {
    rng <- integer(2L)
  }
  rng_sum <- sum(sign(1 / rng))
  if (abs(rng_sum) != 2){
    stop("Can't mix negative and positive locations")
  }
  as.integer(sign(rng_sum))
}
# Base R version of purrr::pluck, alternative to [[
fpluck <- function(x, .cols = NULL, .default = NULL){
  if (is.null(.cols)){
    return(x)
  }
  if (length(.cols) > 1L){
    stop(".cols must have length 1")
  }
  if (is.numeric(.cols)){
    icol <- match(.cols, seq_along(x))
  } else {
    icol <- match(.cols, names(x))
  }
  # If no match just return .default
  if (length(icol) == 0L || is.na(icol)){
    return(.default)
  }
  .subset2(x, icol)
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

na_fill <- function(x, n = NULL, prop = NULL){
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
sqrt_double_eps <- function(){
  sqrt(.Machine$double.eps)
}

# Taken from base R to avoid needing R >= 4
deparse1 <- function(expr, collapse = " ", width.cutoff = 500L, ...){
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}
# Bin x by breaks for each group in g
# Function that takes x (sorted by g) and
# breaks (sorted by g and itself)
# fbincode <- function(x, breaks, right = TRUE, include.lowest = FALSE,
#                      gx = NULL, gbreaks = NULL){
#   x_list <- gsplit2(x, g = gx)
#   breaks_list <- gsplit2(breaks, g = gbreaks)
#   out <- vector("list", length(x_list))
#   for (i in seq_along(out)){
#     out[[i]] <- .bincode(.subset2(x_list, i),
#                          .subset2(breaks_list, i),
#                          right = right,
#                          include.lowest = include.lowest)
#   }
#   unlist(out, recursive = FALSE, use.names = FALSE)
#   # Parallel options
#   # out <- furrr::future_map2(x_list, breaks_list,
#   #                           function(x, y) .bincode(x, y,
#   #                                                   right = right,
#   #                                                   include.lowest = include.lowest))
# }
bin_grouped <- function(x, breaks, gx = NULL, gbreaks = NULL, codes = TRUE,
                        right = TRUE,
                        include_lowest = FALSE,
                        include_oob = FALSE){
  x_list <- gsplit2(x, g = gx)
  breaks_list <- gsplit2(breaks, g = gbreaks)
  out <- cpp_bin_grouped(x_list, breaks_list, codes = codes,
                         include_lowest = include_lowest,
                         right = right,
                         include_oob = include_oob)
  ptype <- if (codes) integer() else x[0L]
  vctrs::list_unchop(out, ptype = ptype)
}
# get_cores <- function(){
#   out <- floor(parallel::detectCores() / 2)
#   if (length(out) != 1 || !is.numeric(out) || is.na(out)){
#     out <- 1
#   }
#   as.integer(out)
# }
# parallel_bincode <- function(x, breaks, right = TRUE, include.lowest = FALSE,
#                              gx = NULL, gbreaks = NULL){
#   n_cores <- get_cores()
#   cluster <- parallel::makeCluster(n_cores)
#   doParallel::registerDoParallel(cluster)
#   x_list <- gsplit2(x, g = gx)
#   breaks_list <- gsplit2(breaks, g = gbreaks)
#   out <- foreach::`%dopar%`(foreach::foreach(i = seq_along(x_list)),
#                             {
#                               .bincode(.subset2(x_list, i),
#                                        .subset2(breaks_list, i),
#                                        right = right,
#                                        include.lowest = include.lowest)
#                             })
#   parallel::stopCluster(cluster)
#   unlist(out, recursive = FALSE, use.names = FALSE)
# }
# Is x numeric and not S4?
is_s3_numeric <- function(x){
  typeof(x) %in% c("integer", "double") && !isS4(x)
}

check_is_num <- function(x){
  if (!is.numeric(x)){
    stop(paste(deparse1(substitute(x)), "must be numeric"))
  }
}
check_is_double <- function(x){
  if (!is.double(x)){
    stop(paste(deparse1(substitute(x)), "must be a double"))
  }
}
# TRUE when x is sorted and contains no NA
is_sorted <- function(x){
  isTRUE(!is.unsorted(x))
}
check_sorted <- function(x){
  if (!is_sorted(x)){
    stop(paste(deparse1(substitute(x)), "must be in ascending order"))
  }
}
# Retains integer class of a if b is 1 and a is integer
divide <- function(a, b){
  if (is.integer(a) && allv2(b, 1)){
    a
  } else {
    a / b
  }
}
# Initialise a single NA value of correct type
na_init <- function(x, size = 1L){
  rep_len(x[NA_integer_], size)
}
strip_attrs <- function(x){
  attributes(x) <- NULL
  x
}
strip_attr <- function(x, which){
  attr(x, which) <- NULL
  x
}
is_integerable <- function(x){
  abs(x) <= .Machine$integer.max
}
all_integerable <- function(x, shift = 0){
  all(
    (abs(collapse::frange(x, na.rm = TRUE)) + shift ) <= .Machine$integer.max,
    na.rm = TRUE
  )
}
add_attr <- function(x, which, value){
  attr(x, which) <- value
  x
}
add_attrs <- function(x, value){
  attributes(x) <- value
  x
}
add_names <- function(x, value){
  names(x) <- value
  x
}
# Use data.table matching if both are character, otherwise base R
fmatch <- function(x, table, nomatch = NA_integer_){
  if (is.character(x) && is.character(table)){
    data.table::chmatch(x, table, nomatch = nomatch)
  } else {
    match(x, table, nomatch = nomatch)
  }
}
match_and_factor <- function(x, table){
  out <- fmatch(x, table)
  levels(out) <- as.character(table)
  class(out) <- "factor"
  out
}
check_is_list <- function(x){
  if (!is.list(x)){
    stop(paste(deparse1(substitute(x)), "must be a list"))
  }
}
check_length <- function(x, size){
  if (length(x) != size){
    stop(paste(deparse1(substitute(x)), "must be of length", size))
  }
}
check_length_lte <- function(x, size){
  if (!(length(x) <= size)){
    stop(paste(deparse1(substitute(x)), "must have length <=", size))
  }
}
# collapse allv and allna with extra length check
allv2 <- function(x, value){
  if (!length(x)){
   return(FALSE)
  }
  collapse::allv(x, value)
}
allNA2 <- function(x){
  if (!length(x)){
    return(FALSE)
  }
  collapse::allNA(x)
}
# collapse::whichv but handles the case when both x and value are length zero
# whichv2 <- function(x, value, invert = FALSE){
#   if (!length(x) && !length(value)){
#     integer()
#   } else {
#     collapse::whichv(x, value, invert)
#   }
# }
# Build on top of any and all
# Are none TRUE?
none <- function(..., na.rm = FALSE){
  !any(..., na.rm = na.rm)
}
# Are some TRUE? Must specify number or proportion
some <- function(..., n = NULL, prop = NULL, na.rm = FALSE){
  if ( ( !is.null(n) && !is.null(prop) ) ||
       ( is.null(n) && is.null(prop) ) ){
    stop("either n or prop must be supplied")
  }
  dots <- list(...)
  if (length(dots) == 1L){
    dots <- dots[[1L]]
  } else {
    dots <- unlist(dots)
  }
  stopifnot(is.logical(dots))
  if (na.rm){
    dots <- dots[!is.na(dots)]
  }
  N <- length(dots)
  num_true <- sum(dots)
  if (!is.null(n)){
    out <- num_true >= n
  }
  if (!is.null(prop)){
    out <- (num_true / N) >= prop
  }
  out
}

list_of_empty_vectors <- function(x){
  lapply(x, function(x) x[0L])
}
# anyDuplicated but returns a logical(1)
anyduplicated <- function(x){
  anyDuplicated.default(x) > 0L
}
simple_deparse <- function(expr){
  deparse(expr, backtick = FALSE, control = NULL)
}
# Taken from stats
hasTsp <- function(x){
  if (is.null(attr(x, "tsp"))){
    attr(x, "tsp") <- c(1, NROW(x), 1)
  }
  x
}
tsp <- function(x){
  attr(x, "tsp")
}
# Simple wrapper around collapse::join
collapse_join <- function(x, y, on, how, sort = FALSE, ...){
  out <- collapse::join(x, y,
                        on = on, sort = sort, how = how,
                        verbose = FALSE,
                        keep.col.order = FALSE,
                        drop.dup.cols = FALSE,
                        overid = 2,
                        ...)
  fselect(out,
          .cols = c(names(x), intersect(setdiff(names(y), names(x)), names(out))))
}
# Use this as an automated tolerance estimate when dealing with small numbers
# Doesn't handle small differences between large numbers though.
# Experimental
# get_tolerance <- function(x){
#   min_tol <- .Machine$double.eps
#   max_tol <- sqrt(min_tol)
#   xmin <- collapse::fmin(abs(x[x != 0]))
#   tol_est <- 10^(-ceiling(abs(log10(xmin))))
#   max(tol_est, min_tol)
#   min(max(tol_est, min_tol), max_tol)
# }
# rng_used <- function(expr){
#   curr <- globalenv()$.Random.seed
#   on.exit({print(paste("RNG USED:", !identical(curr, .Random.seed)))})
#   invisible(eval(expr, envir = parent.frame(n = 1)))
# }

# Sort x with no copy
# If y is supplied, sort x using y
# set_order <- function(x, y = NULL){
#   df <- collapse::qDT(list3(x = x, y = y))
#   data.table::setorderv(df, cols = names(df)[df_ncol(df)])
#   invisible(x)
# }
# Remove NULL list elements
list_rm_null <- function(x){
  .subset(x, cpp_list_which_not_null(x))
}

# nth element
# returns empty vector if n > length(x)
nth <- function(x, n){
  N <- length(x)
  if (n > N){
    x[0L]
  } else {
    x[n]
  }
}

# setdiff where x and y are unique vectors
setdiff2 <- function(x, y){
  # x[collapse::whichNA(collapse::fmatch(x, y, overid = 2L))]
  x[match(x, y, 0L) == 0L]
}
intersect2 <- function(x, y){
  if (is.null(x) || is.null(y)){
    return(NULL)
  }
  c(x[match(x, y, 0L) > 0L], y[numeric()])
}
last_class <- function(x){
  .subset2(class(x), length(class(x)))
}
trunc2 <- function(x){
  if (is.integer(x)) x else trunc(x)
}
round2 <- function(x, digits = 0){
  if (is.integer(x)) x else round(x, digits)
}
floor2 <- function(x){
  if (is.integer(x)) x else floor(x)
}
ceiling2 <- function(x){
  if (is.integer(x)) x else ceiling(x)
}
# Convert typeof x to typeof template
cast2 <- function(x, template){
  type <- typeof(template)
  if (identical(typeof(x), type)){
    x
  } else {
    coerce <- get(tolower(paste0("as.", type)))
    coerce(x)
  }
}
# Exactly the same as .bincode except that
# breaks can be returned as well as codes
# One-sided out-of-bounds values can be included
# Just like in findInterval()

# BREAKS MUST BE SORTED OR THIS WILL CRASH.
# Use with caution

bin <- function(x, breaks,
                right = TRUE,
                include_lowest = FALSE,
                include_oob = FALSE,
                codes = TRUE) {
  .Call(`_timeplyr_cpp_bin`, x, breaks, codes, right, include_lowest, include_oob)
}
# Subset 1 element from each list element
# list items with zero-length vectors are replaced
# with the default value
# out-of-bounds subsets are also replaced with the default
# the idea is that this identity always holds: length(list_subset(x)) == length(x)
list_subset <- function(x, i, default = NA, copy_attributes = FALSE){
  check_length(default, 1)
  if (length(x) == 0){
    first_element <- NULL
    ptype <- NULL
  } else {
    first_element <- x[[1]]
    ptype <- first_element[0]
  }
  out <- cpp_list_subset(x, ptype, as.integer(i), default)
  if (copy_attributes){
    attributes(out) <- attributes(first_element)
  }
  out
}
# Like vector("list", length) but with a default to fill the list elements
new_list <- function(length = 0L, default = NULL){
  check_length(length, 1)
  cpp_new_list(as.numeric(length), default)
}

# Very fast and memory efficient setdiff and intersect
fsetdiff <- function(x, y){
  vec_slice3(x, cpp_which_na(collapse::fmatch(x, y, overid = 2L)))
}
fintersect <- function(x, y){
  vec_slice3(x, cpp_which_not_na(collapse::fmatch(x, y, overid = 2L)))
}

# levels() but the result is its own factor
levels_factor <- function(x){
  lvls <- levels(x)
  out <- seq_along(lvls)
  attr(out, "levels") <- lvls
  # levels(out) <- lvls
  class(out) <- class(x)
  out
}
used_levels <- function(x){
  as.character(fintersect(levels_factor(x), x))
  # levels(quick_factor(x))
}
unused_levels <- function(x){
  as.character(fsetdiff(levels_factor(x), x))
  # fsetdiff(levels(x), used_levels(x))
}

# Apply a function using a LOCAL seed
# This has the nice property that the seed that was previously
# set is NOT interrupted
# This function acts as a way of applying RNG functions
# completely separately to the user environment,
# without affecting the global seed
with_local_seed <- function(expr, .seed = NULL, ...){
  old <- globalenv()[[".Random.seed"]]
  if (is.null(old)){
    set.seed(NULL)
    old <- globalenv()[[".Random.seed"]]
  }
  # Does user want to use local seed?
  if (!is.null(.seed)){
    set.seed(.seed, ...)
  }
  on.exit({assign(".Random.seed", old, envir = globalenv())})
  eval(expr, envir = parent.frame())
}
