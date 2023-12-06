##### Data frame helpers #####

check_is_df <- function(x){
  if (!is_df(x)){
    stop(paste(deparse1(substitute(x)), "must be a data.frame"))
  }
}
# Fast nrow/ncol for data frames
df_nrow <- function(x){
  length(attr(x, "row.names", TRUE))
}
df_ncol <- function(x){
  length(attr(x, "names", TRUE))
}
# Slightly faster dplyr::group_vars
group_vars <- function(x){
  if (is_df(x)){
    if (inherits(x, "grouped_df")){
      out <- setdiff2(names(attr(x, "groups")), ".rows")
    } else {
      out <- character()
    }
  } else {
    out <- dplyr::group_vars(x)
  }
  out
}
# Faster group_data() but shows same error msg
group_data <- function(x){
  if (inherits(x, "grouped_df")){
    attr(x, "groups")
  } else {
    dplyr::group_data(x)
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
    if (length(dplyr_groups) > 0L){
      stop(".by cannot be used on a grouped_df")
    }
    by_groups
  } else {
    dplyr_groups
  }
}

# A collapse version of dplyr_reconstruct,
# fast when lots of groups are involved
df_reconstruct <- function(data, template){
  if (!inherits(data, "data.frame")){
    stop("data must be a data.frame")
  }
  if (!inherits(template, "data.frame")){
    stop("template must be a data.frame")
  }
  data_attrs <- attributes(data)
  template_attrs <- attributes(template)

  if (inherits(template, "data.table")){
    if (!is.null(attr(data, "groups"))){
      attr(data, "groups") <- NULL
    }
    out <- as_DT(safe_ungroup(data))
    invisible(data.table::setalloccol(out))
    return(out)
  }
  if (inherits(template, "grouped_df")){
      # &&
      # (!isTRUE(sum(cpp_lengths(attr(data, "groups"))) == df_nrow(data)) ||
      # !groups_equal(data, template))
      # ){
    template_groups <- setdiff2(names(template_attrs[["groups"]]), ".rows")
    data_groups <- setdiff2(names(attr(data, "groups")), ".rows")
    out_groups <- intersect2(template_groups, names(data))
    if (length(out_groups) == 0L){
      template_attrs[["class"]] <- setdiff2(template_attrs[["class"]], "grouped_df")
      template_attrs[["groups"]] <- NULL
    } else {
      sorted <- attr(template_attrs[["groups"]], "sorted")
      groups <- group_collapse(safe_ungroup(data),
                               .cols = out_groups,
                               sort = TRUE,
                               order = if (is.null(sorted)) TRUE else sorted,
                               id = FALSE, start = FALSE,
                               end = FALSE, size = FALSE,
                               loc = TRUE,
                               drop = dplyr::group_by_drop_default(template))
      groups <- frename(groups, .cols = c(".rows" = ".loc"))
      attributes(groups[[".rows"]]) <- attributes(template_attrs[["groups"]][[".rows"]])
      for (a in setdiff2(names(attributes(groups)),
                        c("row.names", "class", "names"))){
        attr(groups, a) <- NULL
      }
      attr(groups, "sorted") <- sorted
      class(groups) <- c("tbl_df", "tbl", "data.frame")
      template_attrs[["groups"]] <- groups
      attr(template_attrs[["groups"]], ".drop") <- dplyr::group_by_drop_default(template)
    }
  }
  template_attrs[["names"]] <- names(data)
  template_attrs[["row.names"]] <- .row_names_info(data, type = 0L)
  attributes(data) <- template_attrs
  data
}
# Row slice
df_row_slice <- function(data, i, reconstruct = TRUE){
  if (is.logical(i)){
    check_length(i, df_nrow(data))
    i <- cpp_which(i)
  }
  if (df_ncol(data) == 0L || list_has_interval(data)){
    .slice <- vctrs::vec_slice
  } else {
    .slice <- collapse::ss
  }
  if (reconstruct){
    df_reconstruct(.slice(safe_ungroup(data), i), data)
  } else {
    .slice(data, i)
  }
}
df_rm_cols <- function(data, .cols){
  cols_to_remove <- col_select_names(data, .cols = .cols)
  dplyr::dplyr_col_modify(data, add_names(vector("list", length(cols_to_remove)),
                                         cols_to_remove))
}
# Seq along df rows/cols
df_seq_along <- function(data, along = "rows"){
  switch(along,
         rows = seq_len(df_nrow(data)),
         seq_len(df_ncol(data)))
  # along <- rlang::arg_match0(along, c("rows", "cols"))
  # if (along == "rows"){
  #   seq_len(df_nrow(data))
  # } else {
  #   seq_len(df_ncol(data))
  # }
}
df_rep <- function(data, times){
  N <- df_nrow(data)
  if (N > 0L && length(times) > N){
    stop("times must not be greater than nrow(data)")
  }
  if (length(times) != N){
    if (length(times) != 1L){
      stop("times must be of length 1 or nrow(data)")
    }
  }
  df_row_slice(data, rep.int(df_seq_along(data, "rows"),
                             times = times))
}
df_rep_each <- function(data, each){
  if (length(each) == 1L){
    each <- rep_len(each, df_nrow(data))
  }
  df_rep(data, each)
  # N <- df_nrow(data)
  # if (N > 0L && length(each) > N){
  #   stop("each must not be greater than nrow(data)")
  # }
  # if (length(each) != N){
  #   if (length(each) != 1L){
  #     stop("each must be of length 1 or nrow(data)")
  #   }
  # }
  # if (length(each) == 0L){
  #   df_row_slice(data, 0L)
  # }
  # if (length(each) == 1L){
  #   df_row_slice(data, rep(df_seq_along(data, "rows"),
  #                          each = each))
  # } else {
  #   df_row_slice(data, seq_id(each))
  # }
}
# Faster version of nrow specifically for data frames
nrow2 <- function(data){
  length(attr(data, "row.names"))
}
# Do all list elements have same number of elements?
is_list_df_like <- function(X){
  check_is_list(X)
  lens <- collapse::vlengths(X, use.names = FALSE)
  collapse::fnunique(lens) <= 1
}
# Convenience function
is_df <- function(x){
  inherits(x, "data.frame")
}
# alternative tibble::enframe
# Turns named vector to 2-column data frame
# or unnamed vector to 1-column data frame
fenframe <- function(x, name = "name", value = "value"){
  if (is_df(x)){
    x <- strip_attr(unclass(x), "row.names")
  }
  if (!vctrs::vec_is(x)){
    stop("x must be a vector")
  }
  x_nms <- names(x)
  x <- unname(x)
  if (is.null(x_nms)){
    out <- list(x)
    names(out) <- value
  } else {
    out <- list(x_nms, x)
    names(out) <- c(name, value)
  }
  attr(out, "class") <- c("tbl_df", "tbl", "data.frame")
  attr(out, "row.names") <- .set_row_names(length(x))
  out
}
# alternative tibble::deframe
fdeframe <- function(x){
  ncol <- df_ncol(x)
  if (!(is_df(x) && ncol %in% (1:2))){
    stop("`x` must be a 1 or 2 col data frame")
  }
  out <- .subset2(x, ncol)
  if (ncol == 2){
    names(out) <- as.character(.subset2(x, 1L))
  }
  out
}
df_n_distinct <- function(data){
  GRP_n_groups(
    df_to_GRP(data, .cols = names(data),
              return.groups = FALSE, return.order = FALSE)
  )
}
# list() that removes NULL elements
list3 <- function(...){
  list_rm_null(list(...))
}
# list to tibble/DT
# No checks are done so use with caution
# Cannot contain duplicate names
# or different length list elements
list_to_tibble <- function(x){
  if (is_df(x)){
    N <- df_nrow(x)
  } else {
    N <- collapse::fnrow(x)
  }
  # Remove NULL items
  x <- list_rm_null(x)
  attributes(x) <- list(class = c("tbl_df", "tbl", "data.frame"),
                        row.names = c(NA_integer_, -N),
                        names = as.character(names(x)))
  x
}
# Very fast conversion of list to data frame
# It must have unique names and list lengths must all be equal
# NULL elements are removed
list_to_data_frame <- function(x){
  if (is_df(x)){
    N <- df_nrow(x)
  } else {
    N <- collapse::fnrow(x)
  }
  # Remove NULL items
  x <- list_rm_null(x)
  attributes(x) <- list(class = "data.frame",
                        row.names = c(NA_integer_, -N),
                        names = as.character(names(x)))
  x
}
# List to data.table (NO COPY!)
# list_to_data_table <- function(x){
#   if (is_df(x)){
#     N <- df_nrow(x)
#   } else {
#     N <- collapse::fnrow(x)
#   }
#   # Remove NULL items
#   x <- list_rm_null(x)
#   attributes(x) <- list(class = c("data.table", "data.frame"),
#                         row.names = c(NA_integer_, -N),
#                         names = as.character(names(x)))
#   data.table::setDT(x)
#   x
# }
# Create new df with no name checks or length checks
# ..N is there purely to create an (n > 0) x 0 data frame
new_df <- function(..., ..N = NULL){
  out <- list3(...)
  if (is.null(..N)){
    if (length(out) == 0L){
      row_names <- integer()
    } else {
      N <- length(.subset2(out, 1L))
      row_names <- c(NA_integer_, -N)
    }
  } else {
    row_names <- .set_row_names(..N)
  }
  attributes(out) <- list(class = "data.frame",
                        row.names = row_names,
                        names = as.character(names(out)))
  out
}
new_tbl <- function(..., ..N = NULL){
  out <- new_df(..., ..N = ..N)
  add_attr(out, "class", c("tbl_df", "tbl", "data.frame"))
}
# This makes a copy
# Also data.tables currently can't have (n > 0) x 0 structure
new_dt <- function(..., .copy = TRUE){
  out <- new_df(...)
  out <- collapse::qDT(out)
  # data.table::setDT(out)
  if (.copy){
    out <- data.table::copy(out)
  }
  out
}
# Pluck data frame row (works for matrices and df-like lists too)
pluck_row <- function(x, i = 1L, j = collapse::seq_col(x),
                      use.names = TRUE){
  if (length(i) != 1L){
    stop("i must be of length 1")
  }
  if (sign(i) < 0L){
    stop("i must be >= 0")
  }
  if (length(j) == 0L){
    stop("length(j) must be >= 1")
  }
  x <- collapse::ss(x, i = i, j = j)
  # out <- fpluck(
  #   collapse::pivot(x, values = names(x), how = "longer"),
  #   2L
  # )
  out <- fpluck(
    data.table::melt(as_DT(x), measure.vars = names(x),
                     value.name = "value"),
    "value"
  )
  if (use.names){
    if (length(out) == 0L){
      names(out) <- character(0)
    } else {
      names(out) <- names(x)
    }
  }
  out
}
# Temporary check to see if user has dplyr::reframe
dplyr_reframe_exists <- function(){
  .reframe <- try(dplyr::reframe, silent = TRUE)
  exists(".reframe", inherits = FALSE)
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
    attr(data, "groups") <- NULL
    attr(data, "class") <- c("tbl_df", "tbl", "data.frame")
  }
  data
}
df_is_sorted <- function(data){
  df_order <- radixorderv2(data)
  isTRUE(attr(df_order, "sorted"))
}

df_paste_names <- function(data,  sep = "_", .cols = names(data)){
  do.call(paste, c(fselect(data, .cols = .cols),
                   list(sep = sep)))
}
# Efficient way to compare the group information
# of 2 grouped_df objects
# Much better than simply using setequal
# group_data_equal <- function(x, y){
#   # groups1 <- group_data(x)
#   # groups2 <- group_data(y)
#   groups1 <- attr(x, "groups")
#   groups2 <- attr(y, "groups")
#   group_vars1 <- group_vars(x)
#   group_vars2 <- group_vars(y)
#   out <- df_nrow(x) == df_nrow(y)
#   if (out){
#     out <- isTRUE(all.equal(names(groups1), names(groups2)))
#   }
#   if (out){
#     out <- df_nrow(groups1) == df_nrow(groups2)
#   }
#   if (out){
#     out <- cpp_group_data_rows_equal(groups1[[".rows"]], groups2[[".rows"]])
#   }
#   if (out){
#     out <- is.null(groups1) && is.null(groups2) || (
#       df_nrow(
#         vctrs::vec_set_difference(
#           fselect(groups1, .cols = group_vars1),
#           fselect(groups2, .cols = group_vars2)
#         )
#       ) == 0L
#     )
#   }
#   # if (out){
#   #  # loc1 <- unlist(fpluck(groups1, ".rows"), use.names = FALSE)
#   #  # loc2 <- unlist(fpluck(groups2, ".rows"), use.names = FALSE)
#   #  # abs_diff <- collapse::fmax(abs(loc1 - loc2))
#   #  # out <- length(abs_diff) == 0 || abs_diff == 0
#   #  # diff_range <- collapse::frange(loc1 - loc2)
#   #  # # out <- sum(abs(diff_range), na.rm = TRUE) == 0
#   #
#   #  # collapse::setop(loc1, op = "-", loc2)
#   #  # out <- sum(abs(collapse::frange(loc1)),
#   #  #            na.rm = TRUE) == 0
#   #  # One method
#   #  # first_diff <- vec_head(loc1) - vec_head(loc2)
#   #  # first_diff_is_zero <- length(first_diff) == 0L || first_diff == 0L
#   #  # first_diff_is_zero && collapse::fnunique(loc1 - loc2) <= 1L
#   #  # Another method
#   #  # out <- isTRUE(all.equal(loc1, loc2))
#   # }
#   out
# }
empty_tbl <- function(){
  `attributes<-`(
    list(),
    list(
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = integer(),
      names = character()
    )
  )
}
# Faster as_tibble
df_as_tibble <- function(x){
  df_reconstruct(x, empty_tbl())
}
# Initialise data frame with NA
df_init <- function(x, size = 1L){
  nrows <- df_nrow(x)
  if (df_ncol(x) == 0){
    df_reconstruct(new_df(..N = size), x)
  } else {
    if (list_has_interval(x)){
      vctrs::vec_init(x, n = size)
    } else {
      collapse::ss(x, i = collapse::alloc(nrows + 1L, size))
    }
  }
}
# Group IDs (same as dplyr::group_indices)
df_group_id <- function(x){
  if (!inherits(x, "grouped_df") && !inherits(x, "data.frame")){
    stop("Can only calculate group indices on data frames")
  }
  N <- df_nrow(x)
  groups <- attr(x, "groups")
  if (is.null(groups)){
    out <- seq_ones(N)
  } else {
    rows <- groups[[".rows"]]
    out <- cpp_df_group_indices(rows, N)
  }
  out
}
# Reorder data frame to original order after having sorted it using a GRP
df_reorder <- function(data, g){
  df_row_slice(data, collapse::greorder(df_seq_along(data, "rows"), g = g))
}
# Drop rows that are all empty
df_drop_empty <- function(data, .cols = names(data)){
  is_empty_row <- collapse::missing_cases(fselect(data, .cols = .cols), prop = 1)
  which_not_empty <- cpp_which(is_empty_row, invert = TRUE)
  df_row_slice(data, which_not_empty)
}
# Alternative dplyr way, just for fun
dplyr_drop_empty <- function(data, .cols = dplyr::everything()){
  dplyr::filter(data, !dplyr::if_all(.cols = {{ .cols }}, .fns = is.na))
}

df_lag <- function(x, n = 1L, g = NULL){
  df_row_slice(x, flag2(df_seq_along(x), n = n, g = g))
}

# A fast base R df select (to be used in fselect and friends)
df_select <- function(x, .cols){
  out <- .subset(x, .cols)
  # out <- list_rm_null(.subset(x, .cols))
  class(out) <- attr(x, "class")
  attr(out, "row.names") <- .set_row_names(df_nrow(x))
  out
}

df_add_cols <- function(data, cols){
  dplyr::dplyr_col_modify(data, cols)
}

# out <- .subset(x, .cols)
# attr(out, "class") <- attr(x, "class")
# attr(out, "row.names") <- attr(x, "row.names")
# out
