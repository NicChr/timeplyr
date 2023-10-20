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

# Weighted mean
# weighted_mean <- function (x, w = NULL, na.rm = FALSE) {
#   if (is.null(w)) {
#     N <- length(x)
#     if (na.rm){
#       N <- N - num_na(x)
#     }
#     out <- sum(x, na.rm = na.rm) / N
#   }
#   else {
#     if (length(w) != length(x)){
#       stop("'x' and 'w' must have the same length")
#     }
#     denom <- x * w
#     if (na.rm){
#       complete <- !is.na(denom)
#       x <- x[complete]
#       w <- w[complete]
#     }
#     out <- sum(denom, na.rm = na.rm) / sum(w)
#   }
#   out
# }

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
    rng_sign <- check_range_sign(.cols)
    if (rng_sign == -1){
      .cols <- setdiff(nm_seq, abs(.cols))
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
    first_na_col <- .subset(.cols, .subset(which(is_na), 1L))
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
gcd <- function(x, y) {
  r <- x %% y
  ifelse(r, gcd(y, r), y)
}
# gcd3 <- function(x, y) {
#   r <- x %% y
#   out <- y
#   while(r > 0){
#     out <- r
#     x <- y
#     r <- x %% r
#   }
#   out
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
# Least common multiple (using Euclidean algorithm)
lcm <- function(x, y){
  ( abs(x) / gcd(x, y) ) * abs(y)
}

# Exponentially weighted moving average
# ewma <- function (x, ratio) {
#   c(stats::filter(x * ratio, 1 - ratio, "recursive", init = x[1]))
# }
# Append columns from y to x using a common ID and a sql type join.
# tbl_append <- function(x, y, id, keep_id = TRUE, y_suffix = ".x",
#                        side = c("left", "right"), message = TRUE){
#   side <- match.arg(side)
#   if (missing(id)){
#     id <- ".join.index"
#     x[[".join.index"]] <- seq_len(nrow(x))
#     y[[".join.index"]] <- seq_len(nrow(y))
#   }
#   if (n_unique(x[[id]]) != nrow(x)) stop("id must uniquely and commonly identify rows in x and y")
#   if (n_unique(y[[id]]) != nrow(y)) stop("id must uniquely and commonly identify rows in x and y")
#   common_cols <- setdiff(intersect(names(x), names(y)), id)
#   # Join new variables onto original data
#   if (side == "left"){
#     init_names <- names(x)
#     z <- dplyr::left_join(x, y, by = id, suffix = c("", y_suffix))
#   } else {
#     init_names <- names(y)
#     z <- dplyr::right_join(x, y, by = id, suffix = c(y_suffix, ""))
#   }
#   if (!keep_id){
#     z <- dplyr::select(z, -dplyr::all_of(id))
#   }
#   if (message){
#     new_renamed_cols <- setdiff(names(z), init_names)
#     message(paste0("New columns added:\n", paste(new_renamed_cols, collapse = ", ")))
#   }
#   z
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
  lens <- collapse::vlengths(dots, use.names = FALSE)
  if (missing_length) {
    recycle_length <- max(lens)
  } else {
    recycle_length <- length
  }
  recycle_length <- recycle_length * (!collapse::anyv(lens, 0L))
  if (missing_length && collapse::fnunique(lens) == 1L){
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
# Some future utils for counts and weights..
# wt_fun <- function(wt){
#   rlang::expr(sum(!!enquo(wt), na.rm = TRUE))
# }
# data %>% summarise(!!wt_fun(!!enquo(wt)))
# quo_name(enquo(wt))
# quo_is_null()


are_whole_numbers <- function(x){
  if (is.integer(x)){
    return(rep_len(TRUE, length(x)))
  }
  double_equal(x, round(x))
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
  if (is.atomic(x)){
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
# Returns the length or nrows (if list or df)
vec_length <- function(x){
  if (is.list(x)){
    if (inherits(x, "data.frame")){
      out <- df_nrow(x)
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
      out <- df_ncol(x)
    } else {
      lens <- collapse::vlengths(x, use.names = FALSE)
      if (collapse::fnunique(lens) > 1L){
        stop("x must be a vector, matrix, data frame or list with equal lengths")
      }
      out <- length(lens)
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
  nargs <- length(X)
  if (nargs <= 1L){
    return(X)
  }
  out <- vector("list", nargs)
  d <- lengths(X, use.names = FALSE)
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
check_range_sign <- function(x){
  out <- sum(sign(1/x))
  if (abs(out) != length(x)){
    stop("Can't mix negative and positive locations")
  }
  sign(out)
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
# Relative difference
rel_diff <- function(x, y){
  abs(x - y) / pmax(abs(x), abs(y))
}
abs_diff <- function(x, y){
  abs(x - y)
}

# Convenience comparison functions for doubles
# double_equal_rel <- function(x, y, tol = sqrt(.Machine$double.eps)){
#   rel_diff(x, y) < tol
# }

# Convenience comparison functions for doubles
double_equal <- function(x, y, tol = sqrt(.Machine$double.eps)){
  check_is_num(x)
  check_is_num(y)
  set_recycle_args(x = x, y = y, use.names = FALSE)
  if (is.integer(x) && is.integer(y)){
    x == y
  } else {
    cpp_double_equal_vectorised(as.double(x), as.double(y), tolerance = tol)
  }
}
# double_equal <- function(x, y, tol = sqrt(.Machine$double.eps)){
#   abs(x - y) < tol
# }
double_gt <- function(x, y, tol = sqrt(.Machine$double.eps)){
  # (x - y) > tol # Old
  set_recycle_args(x = x, y = y, use.names = FALSE)
  if (is.integer(x) && is.integer(y)){
    x > y
  } else {
    cpp_double_gt_vectorised(as.double(x), as.double(y), tolerance = tol)
  }
}
double_gte <- function(x, y, tol = sqrt(.Machine$double.eps)){
  # (x - y) > -tol # Old
  set_recycle_args(x = x, y = y, use.names = FALSE)
  if (is.integer(x) && is.integer(y)){
    x >= y
  } else {
    cpp_double_gte_vectorised(as.double(x), as.double(y), tolerance = tol)
  }
}
double_lt <- function(x, y, tol = sqrt(.Machine$double.eps)){
  # (x - y) < -tol # Old
  set_recycle_args(x = x, y = y, use.names = FALSE)
  if (is.integer(x) && is.integer(y)){
    x < y
  } else {
    cpp_double_lt_vectorised(as.double(x), as.double(y), tolerance = tol)
  }
}
double_lte <- function(x, y, tol = sqrt(.Machine$double.eps)){
  # (x - y) < tol # Old
  set_recycle_args(x = x, y = y, use.names = FALSE)
  if (is.integer(x) && is.integer(y)){
    x <= y
  } else {
    cpp_double_lte_vectorised(as.double(x), as.double(y), tolerance = tol)
  }
}
# `%~==%` <- double_equal
# `%~>=%` <- double_gte
# `%~<=%` <- double_lte
# `%~>%` <- double_gt
# `%~<%` <- double_lt
# Taken from base R to avoid needing R >= 4
deparse1 <- function(expr, collapse = " ", width.cutoff = 500L, ...){
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}
# Bin x by breaks for each group in g
# Function that takes x (sorted by g) and
# breaks (sorted by g and itself)
fbincode <- function(x, breaks, right = TRUE, include.lowest = FALSE,
                     gx = NULL, gbreaks = NULL){
  x_list <- gsplit2(x, g = gx)
  # gbreaks <- GRP2(gbreaks)
  # if (is.null(gbreaks)){
  #   n_groups <- min(length(breaks), 1L)
  #   group_sizes <- length(breaks)
  #   group_ends <- length(breaks)
  #   group_id <- NULL
  # } else {
  #   n_groups <- GRP_n_groups(gbreaks)
  #   group_sizes <- GRP_group_sizes(gbreaks)
  #   group_ends <- GRP_ends(gbreaks)
  #   group_id <- GRP_group_id(gbreaks)
  # }
  # if (append_inf){
  #   appended_indices <- group_ends + cumsum(seq_ones(n_groups))
  #   new_breaks <- numeric(length(breaks) + n_groups)
  #   new_breaks[-appended_indices] <- breaks
  #   new_breaks[appended_indices] <- Inf
  #   if (!is.null(gbreaks)){
  #     new_group_id <- integer(length(breaks) + n_groups)
  #     new_group_id[-appended_indices] <- group_id
  #     new_group_id[appended_indices] <- group_id[group_ends]
  #     gbreaks <- group_id_to_qg(new_group_id, n_groups = n_groups,
  #                               group_sizes = group_sizes)
  #   }
  #   breaks <- new_breaks
  # }
  breaks_list <- gsplit2(breaks, g = gbreaks)
  out <- vector("list", length(x_list))
  for (i in seq_along(out)){
    out[[i]] <- .bincode(.subset2(x_list, i),
                         .subset2(breaks_list, i),
                         right = right,
                         include.lowest = include.lowest)
  }
  unlist(out, recursive = FALSE, use.names = FALSE)
  # Parallel options
  # out <- foreach(i = seq_along(x_list)) %dopar%
  #   .bincode(.subset2(x_list, i),
  #            .subset2(breaks_list, i),
  #            right = right,
  #            include.lowest = include.lowest)
  # out <- furrr::future_map2(x_list, breaks_list,
  #                           function(x, y) .bincode(x, y,
  #                                                   right = right,
  #                                                   include.lowest = include.lowest))
}
# Is x numeric and not S4?
is_s3_numeric <- function(x){
  typeof(x) %in% c("integer", "double") && !isS4(x)
}

# Much faster and more efficient cut.default
# fast_cut <- function (x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE,
#                   dig.lab = 3L, ordered_result = FALSE, ...){
#   if (!is.numeric(x))
#     stop("'x' must be numeric")
#   if (length(breaks) == 1L) {
#     if (is.na(breaks) || breaks < 2L)
#       stop("invalid number of intervals")
#     nb <- as.integer(breaks + 1)
#     dx <- diff.default(rx <- range(x, na.rm = TRUE))
#     if (dx == 0) {
#       dx <- if (rx[1L] != 0)
#         abs(rx[1L])
#       else 1
#       breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000,
#                         length.out = nb)
#     }
#     else {
#       breaks <- seq.int(rx[1L], rx[2L], length.out = nb)
#       breaks[c(1L, nb)] <- c(rx[1L] - dx/1000, rx[2L] +
#                                dx/1000)
#     }
#   }
#   else nb <- length(breaks <- sort.int(as.double(breaks)))
#   if (anyDuplicated(breaks))
#     stop("'breaks' are not unique")
#   codes.only <- FALSE
#   if (is.null(labels)) {
#     for (dig in dig.lab:max(12L, dig.lab)) {
#       ch.br <- formatC(0 + breaks, digits = dig, width = 1L)
#       if (ok <- all(ch.br[-1L] != ch.br[-nb]))
#         break
#     }
#     labels <- if (ok)
#       paste0(if (right)
#         "("
#         else "[", ch.br[-nb], ",", ch.br[-1L], if (right)
#           "]"
#         else ")")
#     else paste0("Range_", seq_len(nb - 1L))
#     if (ok && include.lowest) {
#       if (right)
#         substr(labels[1L], 1L, 1L) <- "["
#       else substring(labels[nb - 1L], nchar(labels[nb -
#                                                      1L], "c")) <- "]"
#     }
#   }
#   else if (is.logical(labels) && !labels)
#     codes.only <- TRUE
#   else if (length(labels) != nb - 1L)
#     stop("number of intervals and length of 'labels' differ")
#   code <- .bincode(x, breaks, right, include.lowest)
#   if (!codes.only) {
#     levels(code) <- as.character(labels)
#     class(code) <- c(if (ordered_result) "ordered" else character(0), "factor")
#   }
#   code
# }
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
  if (allv2(b, 1)){
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
# flip_names_values <- function(x){
#   x_nms <- names(x)
#   if (is.null(x_nms)){
#     stop("x must be a named vector")
#   }
#   out <- x_nms
#   names(out) <- as.character(unname(x))
#   out
# }
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
check_length_one <- function(x){
  if (length(x) != 1L){
    stop(paste(deparse1(substitute(x)), "must be of length 1"))
  }
}
check_length <- function(x, size){
  if (length(x) != size){
    stop(paste(deparse1(substitute(x)), "must be of length", size))
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

# Similar to collapse::fnobs
# The same can be achieved using
# length(x) - fnobs(x)
# But num_na has support for complex & raw vectors
num_na <- function(x){
  .Call(`_timeplyr_cpp_num_na`, x)
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
  fselect(
    collapse::join(x, y,
                   on = on, sort = sort, how = how,
                   verbose = FALSE,
                   keep.col.order = FALSE,
                   drop.dup.cols = FALSE,
                   overid = 2,
                   ...),
    .cols = c(names(x), setdiff(names(y), names(x)))
  )
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
# near2 <- function(x, y, tol = sqrt(.Machine$double.eps)){
#   adiff <- abs(x - y)
#   ax <- abs(x)
#   ay <- abs(y)
#   any_close_to_zero <- (ax < tol) | (ay < tol)
#   both_same_inf <- (x == Inf & y == Inf) | (x == -Inf & y == -Inf)
#   different_inf <- (x == Inf & y == -Inf) | (x == -Inf & y == Inf)
#   amax <- pmax(ax, ay)
#   rdiff <- adiff / amax
#   out <- dplyr::if_else(any_close_to_zero,
#                         ( adiff < tol ),
#                         ( rdiff < tol ))
#   out[both_same_inf] <- TRUE
#   out[different_inf] <- FALSE
#   out
# }
