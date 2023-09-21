##### Data frame helpers #####

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
      out <- setdiff(names(attr(x, "groups")), ".rows")
    } else {
      out <- character(0)
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
  if (!is_df(data) || !is_df(template)){
    stop("data must be a data.frame")
  }
  data_attrs <- attributes(data)
  template_attrs <- attributes(template)

  if (identical(inherits(template, c("data.table", "data.frame"), which = TRUE),
                c(1L, 2L))){
    if (!is.null(attr(data, "groups"))){
      attr(data, "groups") <- NULL
    }
    out <- as_DT(safe_ungroup(data))
    invisible(data.table::setalloccol(out))
    return(out)
  }
  if (inherits(template, "grouped_df")){
    template_groups <- setdiff(names(template_attrs[["groups"]]), ".rows")
    data_groups <- setdiff(names(attr(data, "groups")), ".rows")
    out_groups <- intersect(template_groups, names(data))
    if (length(out_groups) == 0L){
      template_attrs[["class"]] <- setdiff(template_attrs[["class"]], "grouped_df")
      template_attrs[["groups"]] <- NULL
    } else {
      groups <- group_collapse(safe_ungroup(data),
                               .cols = out_groups, sort = TRUE,
                               id = FALSE, start = FALSE,
                               end = FALSE, size = FALSE,
                               loc = TRUE,
                               drop = dplyr::group_by_drop_default(template))
      groups <- frename(groups, .cols = c(".rows" = ".loc"))
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
  template_attrs[["row.names"]] <- .row_names_info(data, type = 0L)
  attributes(data) <- template_attrs
  data
}
# Row slice
df_row_slice <- function(data, i, reconstruct = TRUE){
  if (collapse::fncol(data) == 0L || has_interval(data)){
    if (is.logical(i)){
      i <- which(i)
    }
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
df_seq_along <- function(data, along = c("rows", "cols")){
  along <- rlang::arg_match0(along, c("rows", "cols"))
  if (along == "rows"){
    seq_len(df_nrow(data))
  } else {
    seq_len(df_ncol(data))
  }
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
  # N <- nrow2(data)
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
    x <- as.list(x)
  }
  if (!vctrs::vec_is(x)){
    stop("x must be a vector")
  }
  x_nms <- names(x)
  if (is.null(x_nms)){
    out <- list(unname(x))
    names(out) <- value
  } else {
    out <- list(x_nms,
                unname(x))
    names(out) <- c(name, value)
  }
  attr(out, "class") <- c("tbl_df", "tbl", "data.frame")
  attr(out, "row.names") <- .set_row_names(length(x))
  out
}
# alternative tibble::deframe
fdeframe <- function(x){
  ncol <- length(attr(x, "names"))
  if (!(is_df(x) || ncol %in% (1:2))){
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
# Create new df with no name checks or length checks
new_df <- function(...){
  out <- list3(...)
  if (length(out) == 0L){
    row_names <- integer()
  } else {
    N <- length(.subset2(out, 1L))
    row_names <- c(NA_integer_, -N)
  }
  attributes(out) <- list(class = "data.frame",
                        row.names = row_names,
                        names = as.character(names(out)))
  out
}
new_tbl <- function(...){
  out <- list3(...)
  if (length(out) == 0L){
    row_names <- integer()
  } else {
    N <- length(.subset2(out, 1L))
    row_names <- c(NA_integer_, -N)
  }
  attributes(out) <- list(class = c("tbl_df", "tbl", "data.frame"),
                          row.names = row_names,
                          names = as.character(names(out)))
  out
}
# This makes a copy
new_dt <- function(...){
  out <- new_df(...)
  out <- data.table::copy(out)
  data.table::setDT(out)
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
group_data_equal <- function(x, y){
  groups1 <- group_data(x)
  groups2 <- group_data(y)
  group_vars1 <- group_vars(x)
  group_vars2 <- group_vars(y)
  out <- df_nrow(x) == df_nrow(y)
  if (out){
    out <- isTRUE(all.equal(names(groups1), names(groups2)))
  }
  if (out){
    out <- df_nrow(groups1) == df_nrow(groups2)
  }
  if (out){
    # out <- dplyr::setequal(
    #   fselect(groups1, .cols = group_vars1),
    #   fselect(groups2, .cols = group_vars2)
    # )
    out <- nrow(
      data.table::fsetdiff(
        as_DT(fselect(groups1, .cols = group_vars1)),
        as_DT(fselect(groups2, .cols = group_vars2))
      )
    ) == 0L
  }
  if (out){
   loc1 <- unlist(fpluck(groups1, ".rows"), use.names = FALSE)
   loc2 <- unlist(fpluck(groups2, ".rows"), use.names = FALSE)
   diff_range <- collapse::frange(loc1 - loc2)
   out <- sum(abs(diff_range), na.rm = TRUE) == 0
   # collapse::setop(loc1, op = "-", loc2)
   # out <- sum(abs(collapse::frange(loc1)),
   #            na.rm = TRUE) == 0
   # One method
   # first_diff <- vec_head(loc1) - vec_head(loc2)
   # first_diff_is_zero <- length(first_diff) == 0L || first_diff == 0L
   # first_diff_is_zero && collapse::fnunique(loc1 - loc2) <= 1L
   # Another method
   # out <- isTRUE(all.equal(loc1, loc2))
  }
  out
}
empty_tbl <- function(){
  structure(list(),
            class = c("tbl_df", "tbl", "data.frame"),
            row.names = integer(),
            names = character())
}
# Faster as_tibble
df_as_tibble <- function(x){
  df_reconstruct(x, empty_tbl())
}
# Initialise data frame with NA
df_init <- function(x, size = 1L){
  nrows <- df_nrow(x)
  if (df_ncol(x) == 0){
    df_reconstruct(structure(list(), class = "data.frame",
                             row.names = .set_row_names(size),
                             names = character()),
                   x)
  } else {
    collapse::ss(x, i = collapse::alloc(nrows + 1L, size))
  }
}
##### data.table specific helpers #####

# Convert to data table
as_DT <- function(x){
  if (inherits(x, "data.table")){
    x[TRUE]
  } else if (inherits(x, "data.frame") &&
             collapse::fncol(x) > 0L){
    collapse::qDT(x[TRUE])
  } else {
    data.table::as.data.table(x)
  }
}
# df_complete_cases <- function(data, .cols = names(data)){
#   df_row_slice(data, vctrs::vec_detect_complete(
#     fselect(data, .cols = .cols)
#   ))
#   # df_row_slice(data, collapse::whichv(rowSums(is.na(
#   #   fselect(data, .cols = .cols)
#   # )), 0))
# }

# key and sort with na.last argument
#
# When cols = character(0) nothing is changed
# When cols = NULL the key is removed
setorderv2 <- function(x, cols, order = 1L, na.last = TRUE){
  if (length(cols) > 0L){
    data.table::setorderv(x, cols = cols, order = order, na.last = na.last)
  }
}
setkeyv2 <- function(x, cols, verbose = getOption("datatable.verbose"),
                     physical = TRUE){
  if (is.null(cols)){
    data.table::setkeyv(x, cols = NULL, verbose = verbose, physical = physical)
  } else {
    stopifnot(is.character(cols))
    if (length(cols) > 0L){
      data.table::setkeyv(x,
                          cols = cols,
                          verbose = verbose,
                          physical = physical)
    }
  }
}
# Slightly safer way of removing DT cols
set_rm_cols <- function(DT, cols = NULL){
  if (is.character(cols)){
    length_check <- length(intersect(cols, names(DT))) > 0L
  } else {
    cols <- as.integer(cols)
    length_check <- length(intersect(cols, seq_along(DT))) > 0L
  }
  if (length_check){
    data.table::set(DT, j = cols, value = NULL)
  }
}
# Data.table version of bind_cols, needs more work
# set_bind_cols <- function(x, y,
#                           suffix = ".y"){
#   if (missing(y)) return(x)
#   x_nms <- names(x)
#   y_nms <- names(y)
#   common_cols <- intersect(x_nms, y_nms)
#   suffix <- rep_len(suffix, length(common_cols))
#   new_col_nms <- paste0(common_cols, suffix)
#   y_nms[y_nms %in% common_cols] <- new_col_nms
#   names(y) <- y_nms
#   data.table::set(x, j = y_nms, value = y)[]
# }
