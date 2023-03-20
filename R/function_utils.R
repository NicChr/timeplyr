#' @noRd


# Lightning fast monotonic checks
is_monotonic_increasing <- function(x){
  isTRUE(all(x == cummax(x)))
}
is_monotonic_decreasing <- function(x){
  isTRUE(all(x == cummin(x)))
}


frequencies <- function(x){
  collapse::GRPN(x, expand = TRUE)
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
    top_n_categories <- utils::head(ranked_categories, n = n)
    top_n_categories <- names(top_n_categories[radix_order(top_n_categories,
                                                           decreasing = descending)])
  } else {
    top_n_categories <- utils::head(x_unique_sorted, n = n)
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
# N unique with efficient na.rm
n_unique <- function(x, na.rm = FALSE){
  out <- collapse::fnunique(x)
  if (na.rm && length(collapse::whichNA(x)) > 0L){
    out <- out - 1L
  }
  out
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
  names(dplyr::transmute(data, !!!enquos(...)))
}
# Select variables utilising tidyselect notation
tidy_select_names <- function(data, ...){
  names(tidyselect::eval_select(rlang::expr(c(!!!enquos(...))), data = data))
}
# This works like dplyr::summarise but evaluates each expression
# independently, and on the ungrouped data.
# The result is always a list.
summarise_list2 <- function(data, ..., fix.names = TRUE){
  data <- safe_ungroup(data)
  quo_list <- rlang::eval_tidy(enquos(...), data)
  out <- purrr::map(quo_list, function(quo) dplyr_summarise(data, !!quo))
  # Remove NULL entries
  out_sizes <- collapse::vlengths(out)
  if (all(out_sizes == 0)) return(list())
  out <- out[out_sizes > 0]
  # Outer names
  outer_nms <- names(out)
  # Lengths of each list
  out_sizes <- collapse::vlengths(out)
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
  out_lengths <- collapse::vlengths(out)
  if (fix.names){
    final_nms <- character(length(out))
    for (i in seq_along(out)){
      if (outer_nms[i] == ""){
        final_nms[i] <- inner_nms[i]
      } else {
        final_nms[i] <- outer_nms[i]
      }
    }
    names(out) <- final_nms
  }
  out
}
# This is like summarise_list2 but works on grouped data
summarise_list3 <- function(data, ..., fix.names = FALSE){
  quo_list <- rlang::eval_tidy(enquos(...), data)
  out <- purrr::map(quo_list, function(quo) dplyr_summarise(data, !!quo))
  if (fix.names){
    nms_out_nms <- names(out)
    if (is.null(nms_out_nms)) nms_out_nms <- character(length(out))
    out_lengths <- collapse::vlengths(out)
    out_nms <- purrr::flatten_chr(purrr::map2(out, out_lengths, function(x, y) names(x)[y]))
    final_nms <- character(length(out))
      for (i in seq_along(out)){
        if (nms_out_nms[i] == ""){
          final_nms[i] <- out_nms[i]
        } else {
          final_nms[i] <- nms_out_nms[i]
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
  type <- match.arg(type)
  # dplyr_groups <- group_vars(data)
  group_vars <- get_groups(data, {{ .by }})
  if (type == "select"){
   extra_groups <- tidy_select_names(data, !!!enquos(...))
  } else {
    extra_groups <- tidy_transform_names(safe_ungroup(data), !!!enquos(...))
  }
  extra_groups <- setdiff(extra_groups, group_vars)
  all_groups <- c(group_vars, extra_groups)
  list("dplyr_groups" = group_vars,
       "extra_groups" = extra_groups,
       "all_groups" = all_groups)
}
# This function returns the groups of a data frame
get_groups <- function(data, .by = NULL){
  dplyr_groups <- group_vars(data)
  by_groups <- tidy_select_names(data, {{ .by }})
  if (length(by_groups) > 0L){
    if (length(dplyr_groups) > 0L) stop(".by cannot be used on a grouped_df")
    by_groups
  } else {
    dplyr_groups
  }
}

# A collapse version of dplyr_reconstruct,
# fast when lots of groups are involved
df_reconstruct <- function(data, template){
  if (!inherits(data, "data.frame") || !inherits(template, "data.frame")){
    stop("data must be a data.frame")
  }
  if (identical(inherits(template, c("data.table", "data.frame"), which = TRUE),
                c(1L, 2L))){
    attr(data, "groups") <- NULL
    return(data.table::as.data.table(data))
  }
  data_attrs <- attributes(data)
  template_attrs <- attributes(template)
  if (inherits(template, "grouped_df")){
    template_groups <- setdiff(names(template_attrs[["groups"]]), ".rows")
    data_groups <- setdiff(names(attr(data, "groups")), ".rows")
    out_groups <- intersect(template_groups, names(data))
    if (length(out_groups) == 0L){
      template_attrs[["class"]] <- setdiff(template_attrs[["class"]], "grouped_df")
      template_attrs[["groups"]] <- NULL
    # } else {
    } else if (!inherits(data, "grouped_df") || !identical(template_attrs[["groups"]], data_attrs[["groups"]])){
      grps <- collapse::GRP(dplyr::select(safe_ungroup(data), dplyr::all_of(out_groups)),
                            sort = TRUE, decreasing = FALSE, na.last = TRUE,
                            return.groups = TRUE, call = FALSE)
      groups <- dplyr::as_tibble(as.list(grps[["groups"]]))
      groups[[".rows"]] <- collapse::gsplit(x = seq_len(length(grps[["group.id"]])),
                                            g = grps)

      attributes(groups[[".rows"]]) <- attributes(template_attrs[["groups"]][[".rows"]])
      attr(groups, "groups") <- NULL
      template_attrs[["groups"]] <- groups
      attr(template_attrs[["groups"]], ".drop") <- dplyr::group_by_drop_default(template)
    }
    # All other non-group attributes
    template_attrs[["names"]] <- names(data)
    template_attrs[["row.names"]] <- data_attrs[["row.names"]]
    attributes(data) <- template_attrs
    data
  } else {
    template_attrs[["names"]] <- names(data)
    template_attrs[["row.names"]] <- data_attrs[["row.names"]]
    attributes(data) <- template_attrs
    data
  }
}
# Faster version of nrow specifically for data frames
nrow2 <- function(data){
  length(attr(data, "row.names"))
}
# Returns list of named ... expressions
dot_nms <- function(..., fix.names = FALSE){
  dot_nms <- purrr::map_chr(substitute(as.list(...))[-1], rlang::expr_deparse)
  if (fix.names){
    nms_dot_nms <- names(dot_nms)
    out_nms <- character(length(dot_nms))
    if (length(nms_dot_nms) > 0){
      for (i in seq_along(dot_nms)){
        if (nms_dot_nms[i] == ""){
          out_nms[i] <- dot_nms[i]
        } else {
          out_nms[i] <- nms_dot_nms[i]
        }
      }
    } else {
      out_nms <- unname(dot_nms)
    }
    names(dot_nms) <- out_nms
  }
  dot_nms
}
# Faster dot nms
dot_nms2 <- function(...){
  unlist((lapply(substitute(as.list(...))[-1L], deparse)),
         recursive = FALSE, use.names = FALSE)
}
# Default arguments
match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))

  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )


  match.call(sys.function(sys.parent()), call)
}

# Temporary check to see if user has dplyr::reframe
dplyr_reframe_exists <- function(){
  .reframe <- try(dplyr::reframe, silent = TRUE)
  exists(".reframe")
}
# Use reframe if user has it otherwise summarise
dplyr_summarise <- function(...){
  if (dplyr_reframe_exists()){
    dplyr::reframe(...)
  } else {
    dplyr::summarise(...)
  }
}
# Safe ungroup for any data type
safe_ungroup <- function(data){
  if (inherits(data, "grouped_df")){
   dplyr::ungroup(data)
  } else {
    data
  }
}
# Fast factor()
ffactor <- function(x, levels = NULL, ordered = FALSE, na.exclude = TRUE){
  # If no supplied levels, collapse can be used safely
  if (is.null(levels)){
    out <- collapse::qF(data.table::copy(x), sort = TRUE, ordered = ordered,
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
      out <- collapse::qF(data.table::copy(x), sort = TRUE, ordered = ordered,
                          na.exclude = na.exclude)
    } else {
      out <- factor(x, levels = levels, ordered = ordered, exclude = exclude)
    }
  }
  out
}
# Like group_keys but the result is grouped
# Sort controls whether or not the data frame gets sorted by
# the group, not if the group is itself sorted, which in this case
# it always is
unique_groups <- function(data, ...,
                          sort = FALSE, .by = NULL,
                          add_indices = FALSE){
  group_vars <- get_group_info(data, !!!enquos(...),
                               .by = {{ .by }},
                               type = "select")[["all_groups"]]
  # Check if group id colname exists
  grp_nm <- new_var_nm(data, ".group.id")
  data[[grp_nm]] <- group_id(data, !!!enquos(...),
                             sort = TRUE, .overwrite = FALSE,
                                .by = {{ .by }}, as_qg = FALSE)
  # Distinct group rows
  out <- collapse::funique(dplyr::select(data, dplyr::all_of(c(group_vars,
                                                               grp_nm))),
                           cols = grp_nm, sort = sort)
  if (!add_indices){
    out[[grp_nm]] <- NULL
  }
  out
}
# # Add group id to data.table by reference
# # Use template if template is a grouped_df and you
# # want to use the group indices from that
# set_add_group_id <- function(DT, template = NULL, sort = TRUE,
#                              .by = NULL, key = FALSE,
#                              as_qg = FALSE,
#                              .name = ".group.id"){
#   if (!is.null(template)){
#     group_vars <- group_vars(template)
#   } else {
#     group_vars <- NULL
#   }
#   by_vars <- tidy_select_names(DT, {{ .by }})
#   if (length(by_vars) > 0){
#     if (length(group_vars) > 0) stop(".by cannot be used on a grouped_df")
#   }
#   # Method for grouped_df
#   if (length(group_vars) == 0L && length(by_vars) == 0L){
#     DT[, (.name) := rep_len(1L, (.N))]
#     } else if (sort && length(group_vars) > 0){
#       DT[, (.name) := dplyr::group_indices(template)]
#   }
#   else if (length(group_vars) > 0 && !sort){
#     DT[, (.name) := as.integer(collapse::group(DT[, group_vars, with = FALSE],
#                                     starts = FALSE, group.sizes = FALSE))]
#   } else {
#     DT[, (.name) := collapse::GRP(DT[, by_vars, with = FALSE],
#                                   sort = sort,
#                                         decreasing = FALSE,
#                                         na.last = TRUE,
#                                         return.groups = FALSE,
#                                         return.order = FALSE,
#                                         method = "auto",
#                                         call = FALSE)[["group.id"]]]
#   }
#   if (as_qg) DT[, (.name) := collapse::qG(get(.name),
#                                           sort = TRUE,
#                                           ordered = FALSE)]
#   if (key) data.table::setkeyv(DT, cols = .name)
# }
# Slightly safer way of removing DT cols
set_rm_cols <- function(DT, cols = NULL){
 if (length(intersect(cols, names(DT))) > 0L) DT[, (cols) := NULL]
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
  i <- 0L
  grp_nm <- check
  while (check %in% data_nms){
    i <- i + 1L
    check <- paste0(grp_nm, i)
  }
  return(check)
}
# Convenience function
is_df <- function(x){
  inherits(x, "data.frame")
}
# Recycle arguments
recycle_args <- function (..., length, set_names = FALSE){
  dots <- list(...)
  missing_length <- missing(length)
  if (missing_length) {
    recycle_length <- max(lengths(dots))
  }
  else {
    recycle_length <- length
  }
  if (missing_length && base::length(unique(lengths(dots))) == 1L) {
    out <- dots
  }
  else {
    out <- lapply(dots, function(x) rep_len(x, recycle_length))
  }
  if (set_names){
    arg_names <- vapply(match.call(expand.dots = FALSE)[["..."]],
                        FUN = deparse, FUN.VALUE = character(1))
    names(out) <- arg_names
  }
  out
}
# Convenience function to set names without stats namespace call
setnames <- function(object = nm, nm){
  names(object) <- nm
  object
}
rowProds <- function(x, na.rm = FALSE, dims = 1L){
  exp(rowSums(log(x), na.rm = na.rm, dims = dims))
}
# Wrapper around order() to use radix order
radix_order <- function(x, na.last = TRUE, ...){
  order(x, method = "radix", na.last = na.last, ...)
}
# Wrapper around order() to use radix sort
radix_sort <- function(x, na.last = TRUE, ...){
  x[radix_order(x, na.last = na.last, ...)]
}
# Creates a sequence of ones.
seq_ones <- function(length){
  if (length <= .Machine$integer.max){
    rep_len(1L, length)
  } else {
    rep_len(1, length)
  }
}
# Grouped Empirical distribution function
# Like dplyr::cume_dist() but works with groups.
edf <- function(x, g = NULL, na.rm = TRUE){
  # Order x first
  x_order <- radix_order(x)
  x <- x[x_order]
  ones <- seq_ones(length(x))
  if (is.null(g)){
    g <- as.integer(ones)
  } else {
    g <- g[x_order]
  }
  grp <- collapse::GRP(g, sort = FALSE,
                     na.last = TRUE, return.groups = FALSE,
                     return.order = FALSE,
                     call = FALSE)
  grp_n <- collapse::GRPN(grp, expand = TRUE)
  run_sum <- collapse::fcumsum(ones, na.rm = na.rm,
                               check.o = TRUE,
                               g = grp)
  out <- run_sum / grp_n
  out[radix_order(x_order)]
}
format_number <- function(x, digits = NULL,
                          rounding = c("decimal", "signif"), round_half_up = TRUE,
                          scientific = 10, drop_trailing_zeros = TRUE, drop_leading_zeros = FALSE,
                          prefix = "", suffix = "", thousands_sep = "", decimal_sep = ".",
                          format = NULL, flag = NULL, mode = storage.mode(x),
                          width = NULL, big.interval = 3,
                          small.mark = "", small.interval = 5,
                          preserve.width = "individual",
                          zero.print = NULL, replace.zero = TRUE){
  # Handle common special cases
  if (length(x) == 0) return(character(0))
  if (isTRUE(all(is.na(x)))) return(rep_len(NA_character_, length(x)))
  stopifnot(is.numeric(x))
  stopifnot(inherits(scientific, c("integer", "numeric", "logical")))
  if (!isTRUE(digits >= 0 || is.null(digits))) stop("Digits must be >= 0")
  rounding <- match.arg(tolower(rounding), c("decimal", "signif"))
  x_names <- names(x)
  if (inherits(x, "integer64")){
    warning("Numbers of class 'integer64' will be converted to numeric")
    x <- as.numeric(x)
  }
  if (is.logical(scientific)){
    scientific <- if (scientific) -Inf else Inf
  }
  # Auto format
  if (length(format) == 0){
    sci_format <- "e"
    non_sci_format <- if (rounding == "decimal") "f" else "fg"
  }
  # Auto flag
  if (length(flag) == 0){
    sci_flag <- ""
    non_sci_flag <- if (rounding == "decimal") "" else "#"
  }
  # User specified format
  if (length(format) > 0){
    sci_format <- format
    non_sci_format <- format
  }
  # User specified flag
  if (length(flag) > 0){
    sci_flag <- flag
    non_sci_flag <- flag
  }
  # If no digits specified, use auto
  # 0 if format is for integers else <=3
  if (length(digits) == 0){
    digits <- min(getOption("digits"), 3)
  }
  if (rounding == "signif"){
    format_digits_s <- digits
    format_digits_l <- max(digits - 1, 0)
    x <- signif2(x, digits = digits, round_half_up = round_half_up)
  } else if (rounding == "decimal"){
    format_digits_s <- digits
    format_digits_l <- digits
    x <- round2(x, digits = digits, round_half_up = round_half_up)
  }
  # If scientific notation specified, don't use below heuristic
  # Heuristic to use scientific notation when numbers are too large/small
  log10_ranks <- trunc(abs(log10(abs(x))))
  log10_ranks[x == 0] <- 0
  log10_ranks_s <- which(log10_ranks < scientific)
  log10_ranks_l <- which(log10_ranks >= scientific)
  out <- data.table::copy(x)
  out[seq_along(x)] <- character(length(x))
  # collapse::setv(out, seq_along(x), character(length(x)),
  #                vind1 = TRUE)
  collapse::setv(out, log10_ranks_s, formatC(x[log10_ranks_s], drop0trailing = drop_trailing_zeros,
                                             digits = format_digits_s,
                                             format = non_sci_format, flag = non_sci_flag, mode = mode,
                                             big.mark = thousands_sep, decimal.mark = decimal_sep,
                                             width = width, big.interval = big.interval,
                                             small.mark = small.mark, small.interval = small.interval,
                                             preserve.width = preserve.width,
                                             zero.print = replace.zero, replace.zero = replace.zero),
                 vind1 = TRUE)
  collapse::setv(out, log10_ranks_l, formatC(x[log10_ranks_l], drop0trailing = drop_trailing_zeros,
                                             digits = format_digits_l,
                                             format = sci_format, flag = sci_flag,
                                             mode = mode,
                                             big.mark = thousands_sep, decimal.mark = decimal_sep,
                                             width = width, big.interval = big.interval,
                                             small.mark = small.mark, small.interval = small.interval,
                                             preserve.width = preserve.width,
                                             zero.print = zero.print, replace.zero = replace.zero),
                 vind1 = TRUE)
  # Temporary solution to fix formatC bug
  # Remove numbers ending with dot
  if (rounding == "signif"){
    collapse::setv(out,
                   seq_along(out),
                   sub(pattern = paste0("\\", decimal_sep, "{1}([^[:digit:]]{0,})$"), "\\1", out, perl = TRUE),
                   vind1 = TRUE)
    # out <- sub(pattern = paste0("\\", decimal_sep, "{1}([^[:digit:]]{0,})$"), "\\1", out, perl = TRUE)
  }
  # if (drop_leading_zeros) y <- sub("^([-|<|=|>]?)0[.]", "\\1.", y) # Drop leading zeros
  if (drop_leading_zeros){
    # Regex that drops zeros where preceded by non digits at start of string and
    # proceeded by the decimal separator
    reg_pattern <- paste0("^([^[:digit:]]{0,})0{1,}\\", decimal_sep, "{1}")
    collapse::setv(out,
                   seq_along(out),
                   sub(reg_pattern, paste0("\\1", decimal_sep), out, perl = TRUE), # Drop leading zeros
                   vind1 = TRUE)
    # out <- sub(reg_pattern, paste0("\\1", decimal_sep), out, perl = TRUE) # Drop leading zeros
  }
  collapse::setv(out,
                 seq_along(out),
                 stringr::str_c(prefix, out, suffix),
                 vind1 = TRUE)
  # out[seq_len(length(out))] <- stringr::str_c(prefix, out, suffix)
  # y[is.infinite(x)] <- stringr::str_c(prefix, x[is.infinite(x)], suffix)
  # Handle the NAs manually
  collapse::setv(out,
                 which(is.na(x)),
                 NA_character_,
                 vind1 = TRUE)
  # out[is.na(x)] <- NA_character_
  # Retain named vector
  names(out) <- x_names
  out
}
round2 <- function(x, digits = 0, round_half_up = TRUE){
  if (round_half_up){
    janitor::round_half_up(x, digits = digits)
  } else {
    round(x, digits = digits)
  }
}
signif2 <- function(x, digits = 6, round_half_up = TRUE){
  if (round_half_up){
    janitor::signif_half_up(x, digits = digits)
  } else {
    signif(x, digits = digits)
  }
}
