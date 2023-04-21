#' @noRd


# Lightning fast monotonic checks
# is_monotonic_increasing <- function(x){
#   isTRUE(all(x == cummax(x)))
# }
# is_monotonic_decreasing <- function(x){
#   isTRUE(all(x == cummin(x)))
# }


frequencies <- function(x){
  collapse::GRPN(x, expand = TRUE)
}
# Cumulative group sizes
# grp_sizes_cumulative <- function(x){
#   grp <- collapse::group(x, group.sizes = TRUE)
#   grpn <- collapse::GRPN(grp, expand = FALSE)
#   grpn_cum <- collapse::fcumsum(grpn, na.rm = FALSE)
#   out <- grpn_cum[match(as.integer(grp), seq_len(length(grpn)))]
#   out
# }

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
# Memory efficient n unique
n_unique <- function(x, na.rm = FALSE){
  if (lubridate::is.interval(x)){
    dplyr::n_distinct(x, na.rm = na.rm)
  } else {
    collapse::fndistinct(x, na.rm = na.rm)
  }
  # out <- collapse::fnunique(x)
  # if (na.rm && length(collapse::whichNA(x)) > 0L){
  #   out <- out - 1L
  # }
  # out
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
      df_row_slice(safe_ungroup(data), min(nrow2(data), 1L), reconstruct = FALSE), !!!enquos(...),
      fix.names = TRUE
    )
  )
}
tidy_transform_names2 <- function(data, ...){
  names(dplyr::transmute(data, !!!enquos(...)))
}
# Updated version of transmute using mutate
transmute2 <- function(data, ..., .by = NULL){
  group_vars <- get_groups(data, .by = {{ .by }})
  out <- dplyr::mutate(data, !!!enquos(...),
                       .by = {{ .by }}, .keep = "none")
  out_nms <- tidy_transform_names(data, !!!enquos(...))
  dplyr::select(out, all_of(c(group_vars, out_nms)))
}

# Select variables utilising tidyselect notation
tidy_select_names <- function(data, ...){
  names(tidyselect::eval_select(rlang::expr(c(!!!enquos(...))), data = data))
}
# Basic tidyselect information for further manipulation
# Includes output and input names which might be useful
tidy_select_info <- function(data, ...){
  data_nms <- names(data)
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = data)
  out_nms <- names(pos)
  pos <- unname(pos)
  renamed <- is.na(match(out_nms, data_nms) != pos)
  list("pos" = pos,
       "out_nms" = out_nms,
       "in_nms" = data_nms[pos],
       "renamed" = renamed)
}

# This works like dplyr::summarise but evaluates each expression
# independently, and on the ungrouped data.
# The result is always a list.
# Useful way of returning the column names after supplying data-masking variables too
summarise_list <- function(data, ..., fix.names = TRUE){
  if (inherits(data, "grouped_df")) data <- dplyr::ungroup(data)
  quo_list <- rlang::eval_tidy(enquos(...), data)
  out <- lapply(quo_list, function(quo) dplyr_summarise(data, !!quo))
  # Remove NULL entries
  out_sizes <- lengths(out, use.names = FALSE)
  if (all(out_sizes == 0)){
    return(setnames(list(), character(0)))
  }
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
  out_order <- radix_order(c(which_less_than2, rep(which_greater_than1,
                                             out_sizes[which_greater_than1])))
  outer_nms <- c(outer_nms[which_less_than2],
                 rep(outer_nms[which_greater_than1],
                     out_sizes[which_greater_than1]))[out_order]
  out2 <- unlist(out2, recursive = FALSE)
  out1 <- unlist(unname(out1), recursive = FALSE)
  inner_nms <- c(names(out1), names(out2))[out_order]
  out <- c(out1, out2)[out_order]
  out_lengths <- lengths(out)
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

# Slightly faster dplyr::group_vars
group_vars <- function(x){
  if (!inherits(x, "grouped_df") && is_df(x)){
    character(0)
  } else {
    dplyr::group_vars(x)
  }
}
# This function returns the groups of a data frame
get_groups <- function(data, .by = NULL){
  dplyr_groups <- group_vars(data)
  if (rlang::quo_is_null(enquo(.by))){
    by_groups <- NULL
  } else {
    by_groups <- tidy_select_names(data, {{ .by }})
  }
  if (length(by_groups) > 0L){
    if (length(dplyr_groups) > 0L) stop(".by cannot be used on a grouped_df")
    by_groups
  } else {
    dplyr_groups
  }
}
# This function is for functions like count() where extra groups need
# to be created
get_group_info <- function(data, ..., type = c("select", "data-mask"),
                           .by = NULL){
  type <- match.arg(type)
  group_vars <- get_groups(data, {{ .by }})
  if (dots_length(...) == 0L){
    extra_groups <- character(0)
  } else {
    if (type == "select"){
      extra_groups <- tidy_select_names(safe_ungroup(data), !!!enquos(...))
    } else {
      extra_groups <- tidy_transform_names(safe_ungroup(data), !!!enquos(...))
    }
  }

  extra_groups <- setdiff(extra_groups, group_vars)
  all_groups <- c(group_vars, extra_groups)
  list("dplyr_groups" = group_vars,
       "extra_groups" = extra_groups,
       "all_groups" = all_groups)
}

# A collapse version of dplyr_reconstruct,
# fast when lots of groups are involved
df_reconstruct <- function(data, template){
  if (!inherits(data, "data.frame") || !inherits(template, "data.frame")){
    stop("data must be a data.frame")
  }
  data_attrs <- attributes(data)
  template_attrs <- attributes(template)
  if (identical(inherits(template, c("data.table", "data.frame"), which = TRUE),
                c(1L, 2L))){
    attr(data, "groups") <- NULL
    return(collapse::qDT(safe_ungroup(data)[TRUE], keep.attr = FALSE))
    # return(collapse::qDT(data.table::copy(data), keep.attr = FALSE))
  }
  if (inherits(template, "grouped_df")){
    template_groups <- setdiff(names(template_attrs[["groups"]]), ".rows")
    data_groups <- setdiff(names(attr(data, "groups")), ".rows")
    out_groups <- intersect(template_groups, names(data))
    if (length(out_groups) == 0L){
      template_attrs[["class"]] <- setdiff(template_attrs[["class"]], "grouped_df")
      template_attrs[["groups"]] <- NULL
    } else if (!inherits(data, "grouped_df") || !identical(template_attrs[["groups"]], data_attrs[["groups"]])){
      # Sloppy workaround to account for the fact that collapse doesn't
      # correctly group lubridate intervals
      # This is due to the fact that durations don't uniquely identify
      # start and end points.
      if (has_interval(collapse::fselect(safe_ungroup(data), out_groups), quiet = TRUE)){
        grp_nm <- new_var_nm(out_groups, "g")
        groups <- collapse::fselect(safe_ungroup(data), out_groups)
        g <- group_id.default(groups,
                      order = TRUE, as_qg = FALSE)
        groups <- groups %>%
          dplyr::mutate(!!grp_nm := g) %>%
          dplyr::distinct(across(all_of(grp_nm)), .keep_all = TRUE) %>%
          dplyr::arrange(across(all_of(grp_nm)))
        groups[[grp_nm]] <- NULL
      } else {
        g <- collapse::GRP(safe_ungroup(data), by = out_groups,
                              sort = TRUE, decreasing = FALSE, na.last = TRUE,
                           return.order = FALSE,
                              return.groups = TRUE, call = FALSE)
        groups <- dplyr::as_tibble(as.list(g[["groups"]]))
      }

      groups[[".rows"]] <- collapse::gsplit(x = seq_len(nrow2(data)),
                                            g = g)

      attributes(groups[[".rows"]]) <- attributes(template_attrs[["groups"]][[".rows"]])
      for (a in setdiff(names(attributes(groups)),
                        c("row.names", "class", "names"))){
        attr(groups, a) <- NULL
      }
      class(groups) <- c("tbl_df", "tbl", "data.frame")
      template_attrs[["groups"]] <- groups
      attr(template_attrs[["groups"]], ".drop") <- dplyr::group_by_drop_default(template)
    }
  }
  template_attrs[["names"]] <- names(data)
  template_attrs[["row.names"]] <- data_attrs[["row.names"]]
  attributes(data) <- template_attrs
  data
}
# Faster version of nrow specifically for data frames
nrow2 <- function(data){
  length(attr(data, "row.names"))
}
# Returns list of named ... expressions
# dot_nms <- function(..., fix.names = FALSE){
#   dot_nms <- purrr::map_chr(substitute(as.list(...))[-1], rlang::expr_deparse)
#   if (fix.names){
#     nms_dot_nms <- names(dot_nms)
#     out_nms <- character(length(dot_nms))
#     if (length(nms_dot_nms) > 0){
#       for (i in seq_along(dot_nms)){
#         if (nms_dot_nms[i] == ""){
#           out_nms[i] <- dot_nms[i]
#         } else {
#           out_nms[i] <- nms_dot_nms[i]
#         }
#       }
#     } else {
#       out_nms <- unname(dot_nms)
#     }
#     names(dot_nms) <- out_nms
#   }
#   dot_nms
# }
# Faster dot nms
dot_nms <- function(..., use.names = FALSE){
  unlist((lapply(substitute(as.list(...))[-1L], deparse)),
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
# Like group_keys but the result is grouped
# Sort controls whether or not the data frame gets sorted by
# the group, not if the group is itself sorted, which in this case
# it always is
unique_groups <- function(data, ..., order = sort, sort = TRUE,
                          .by = NULL, .group_id = TRUE){
  out <- group_collapse(data, !!!enquos(...),
                        order = order, sort = sort,
                        .by = .by,
                        loc = FALSE,
                        start = FALSE, end = FALSE,
                        size = FALSE)
  if (.group_id){
    names(out)[names(out) == ".group"] <- "group_id"
  } else {
    out[[".group"]] <- NULL
  }
  out

  # group_vars <- get_groups(data, {{ .by }})
  # extra_vars <- tidy_select_names(data, !!!enquos(...))
  # # Check if group id colname exists
  # grp_nm <- new_var_nm(names(data), "group_id")
  # out <- data %>%
  #   dplyr::select(all_of(group_vars), !!!enquos(...)) %>%
  #   add_group_id(all_of(extra_vars),
  #                order = order,
  #                .by = {{ .by }}, as_qg = FALSE,
  #                .name = grp_nm)
  # out <- df_row_slice(out, which(!collapse::fduplicated(out[[grp_nm]], all = FALSE)))
  # if (sort){
  #   out <- df_row_slice(out, radix_order(out[[grp_nm]]),
  #                       reconstruct = FALSE)
  # }
  # if (!.group_id) out[[grp_nm]] <- NULL
  # attr(out, "row.names") <- seq_len(nrow2(out))
  # out
}
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
recycle_args <- function (..., length, use.names = FALSE){
  dots <- list(...)
  missing_length <- missing(length)
  if (missing_length) {
    recycle_length <- max(lengths(dots))
  }
  else {
    recycle_length <- length
  }
  if (missing_length && base::length(unique(lengths(dots, use.names = FALSE))) == 1L) {
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

setv <- utils::getFromNamespace("setv", "collapse")

# Some future utils for counts and weights..
# wt_fun <- function(wt){
#   rlang::expr(sum(!!enquo(wt), na.rm = TRUE))
# }
# data %>% summarise(!!wt_fun(!!enquo(wt)))
# quo_name(enquo(wt))
# quo_is_null()

CJ <- utils::getFromNamespace("CJ", "data.table")

is_whole_number <- function(x){
  if (is.integer(x)) return(TRUE) # If already integer then true
  if (length(x) == 0L) return(FALSE) # If length is 0 then false
  # all.equal(x, as.integer(x), check.attributes = FALSE)
  # isTRUE((sum(x %% 1) == 0))
  all(floor(x) == x, na.rm = FALSE)
}
# Do all list elements have same number of elements?
is_list_df_like <- function(X){
  stopifnot(is.list(X))
  lens <- collapse::vlengths(X, use.names = FALSE)
  isTRUE(n_unique(lens) <= 1)
}
pair_unique <- function(x, y){
  ( ( (x + y + 1) * (x + y) ) / 2 ) + x
}
# Row slice
df_row_slice <- function(data, i, reconstruct = TRUE){
  if (reconstruct){
    df_reconstruct(vctrs::vec_slice(safe_ungroup(data), i), data)
  } else {
    vctrs::vec_slice(data, i)
  }

}
