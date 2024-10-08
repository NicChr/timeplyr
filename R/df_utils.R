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
  }
  # else if (inherits(x, "data.frame")){
  #   N <- df_nrow(x)
  #   out <- list(.rows = vctrs_new_list_of(list(df_seq_along(x)), ptype = integer()))
  #   class(out) <- "data.frame"
  #   attr(out, "row.names") <- .set_row_names(1L)
  #   out
  #   }
  else {
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
    # colnames <- names(data)
    row_names <- .row_names_info(data, type = 0L)
    # out <- data
    # class(out) <- template_attrs[["class"]]
    # data.table::setalloccol(out)
    out <- collapse::qDT(data)
    # data.table::setalloccol(out)
    #####
    # THIS IS TECHNICALLY NOT CORRECT BUT ALIGNS WITH BASE R
    # DATA.TABLE CANNOT HANDLE (n > 0) x 0 data frames
    # This ensures you can add vectors of size n to
    # an (n > 0) x 0 data.table
    # attr(out, "row.names") <- row_names
    data.table::setattr(out, "row.names", row_names)
    #####
    # for (a in setdiff2(names(template_attrs), c("class", "row.names", "names", ".internal.selfref"))){
    for (a in setdiff2(names(template_attrs),
                       c("row.names", "names", ".internal.selfref"))){
      data.table::setattr(out, a, template_attrs[[a]])
    }
    return(out)
  }
  if (inherits(template, "grouped_df")){
    template_groups <- setdiff2(names(template_attrs[["groups"]]), ".rows")

    # If groups in template are all in data AND
    # the data relating to groups in template
    # are identical to those in data, then no need to recalculate

    groups_are_identical <-
      all(template_groups %in% names(data)) &&
      identical(
        strip_attrs(as.list(cheapr::sset(data, j = template_groups))),
        strip_attrs(as.list(cheapr::sset(template, j = template_groups)))
      )

    if (!groups_are_identical){
      out_groups <- intersect2(template_groups, names(data))
      if (length(out_groups) == 0L){
        template_attrs[["class"]] <- setdiff2(template_attrs[["class"]], "grouped_df")
        template_attrs[["groups"]] <- NULL
      } else {
        drop_by_default <- df_group_by_drop_default(template)
        order <- df_group_by_order_default(template)
        sorted <- attr(template_attrs[["groups"]], "sorted")
        groups <- group_collapse(safe_ungroup(data),
                                 .cols = out_groups,
                                 sort = TRUE,
                                 order = order,
                                 id = FALSE, start = FALSE,
                                 end = FALSE, size = FALSE,
                                 loc = TRUE,
                                 .drop = drop_by_default)
        groups <- frename(groups, .cols = c(".rows" = ".loc"))
        attributes(groups[[".rows"]]) <- attributes(template_attrs[["groups"]][[".rows"]])
        for (a in setdiff2(names(attributes(groups)),
                           c("row.names", "class", "names"))){
          attr(groups, a) <- NULL
        }
        attr(groups, "sorted") <- sorted
        class(groups) <- c("tbl_df", "tbl", "data.frame")
        attr(groups, ".drop") <- drop_by_default
        template_attrs[["groups"]] <- groups
      }
    }
  }
  template_attrs[["names"]] <- names(data)
  template_attrs[["row.names"]] <- .row_names_info(data, type = 0L)
  attributes(data) <- template_attrs
  data
}
# Row slice
df_row_slice <- function(data, i, reconstruct = TRUE){
  out <- cheapr::sset(data, i)
  if (reconstruct){
    out <- df_reconstruct(out, data)
  }
  out
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
}
# Repeat data frame rows
# Such that identical(df_rep(data, 3), bind_rows(data, data, data))
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
  df_row_slice(data, rep.int(df_seq_along(data, "rows"), times = times))
}
# Repeat each row
df_rep_each <- function(data, each){
  if (length(each) == 1L){
    each <- rep_len(each, df_nrow(data))
  }
  df_rep(data, each)
}
# Do all list elements have same number of elements?
is_list_df_like <- function(X){
  check_is_list(X)
  lens <- cheapr::lengths_(X)
  collapse::fnunique(lens) <= 1
}
# Convenience function
is_df <- function(x){
  inherits(x, "data.frame")
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
list_as_tbl <- function(x){
  df_as_tbl(list_as_df(x))
}

# Create new df with no name checks or length checks
# ..N is there purely to create an (n > 0) x 0 data frame
new_df <- function(..., ..N = NULL, .recycle = FALSE){
  if (.recycle){
    out <- cheapr::recycle(...)
  } else {
    out <- list3(...)
  }
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
  attr(out, "names") <- as.character(attr(out, "names", TRUE))
  attr(out, "row.names") <- row_names
  class(out) <- "data.frame"
  out
}
new_tbl <- function(..., ..N = NULL, .recycle = FALSE){
  df_as_tbl(new_df(..., ..N = ..N, .recycle = .recycle))
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
    class(data) <- c("tbl_df", "tbl", "data.frame")
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
empty_df <- function(){
  `attributes<-`(
    list(),
    list(
      class = "data.frame",
      row.names = integer(),
      names = character()
    )
  )
}
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

df_as_df <- function(x){
  list_as_df(x)
}
# Faster as_tibble
df_as_tbl <- function(x){
  out <- list_as_df(x)
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}
# Theoretically safe data frame initialisation
# for all objs with a rep() and [ method
df_init <- function(x, size = 1L){
  ncols <- df_ncol(x)
  if (ncols == 0){
    init_df <- new_df(..N = size)
  } else {
    init_df <- list_as_df(
      lapply(x, function(y) na_init(y, size))
    )
  }
  df_reconstruct(init_df, x)
}
# df_init2 <- function(x, size = 1L){
#   # template <- collapse::alloc(NA_integer_, size)
#   template <- rep_len(NA_integer_, size)
#   out <- new_list(length(x), template)
#   for (i in seq_along(out)){
#     if (!is.object(x[[i]]) || inherits(x[[i]], c("Date", "POSIXct"))){
#       out[[i]] <- cast2(out[[i]], x[[i]])
#       attributes(out[[i]]) <- attributes(x[[i]])
#       class(out[[i]]) <- class(x[[i]])
#     } else {
#       out[[i]] <- na_init(x[[i]], size)
#     }
#     # if (isS4(x[[i]]) || !is.atomic(x[[i]])){
#     #   out[[i]] <- na_init(x[[i]], size)
#     # } else {
#     #   attributes(out[[i]]) <- attributes(x[[i]])
#     #   class(out[[i]]) <- class(x[[i]])
#     # }
#   }
#   names(out) <- names(x)
#   out <- list_as_df(out)
#   df_reconstruct(out, x)
#   # if (length(out) == 0){
#   #   attr(out, "row.names") <- .set_row_names(size)
#   # }
#   # out
# }
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
    out <- cpp_df_group_indices(groups[[".rows"]], N)
  }
  out
}
# Reorder data frame to original order after having sorted it using a GRP
df_reorder <- function(data, g){
  df_row_slice(data, greorder2(df_seq_along(data, "rows"), g = g))
}
# Fast/efficient drop empty rows
df_drop_empty <- function(data, .cols = names(data)){
  is_empty_row <- cheapr::row_all_na(cheapr::sset(data, j = .cols))
  which_not_empty <- cheapr::which_(is_empty_row, invert = TRUE)
  if (length(which_not_empty) == df_nrow(data)){
    data
  } else {
    df_row_slice(data, which_not_empty)
  }
}
# Alternative dplyr way, just for fun
dplyr_drop_empty <- function(data, .cols = dplyr::everything()){
  dplyr::filter(data, !dplyr::if_all(.cols = {{ .cols }}, .fns = is.na))
}

# df_select <- function(x, .cols){
#   out <- .subset(x, .cols)
#   class(out) <- attr(x, "class")
#   attr(out, "row.names") <- .set_row_names(df_nrow(x))
#   out
# }

df_add_cols <- function(data, cols){
  dplyr::dplyr_col_modify(data, cols)
}

# Extremely simple count functions for grouped_df

df_count <- function(.data, name = "n", weights = NULL){
  groups <- group_data(.data)
  if (!is.null(weights)){
    if (length(weights) != df_nrow(.data)){
      stop("Weights must satisfy `length(weights) == nrow(.data)`")
    }
    counts <- collapse::fsum(weights, g = df_group_id(.data), use.g.names = FALSE)
  } else {
    counts <- cheapr::lengths_(groups[[".rows"]])
  }
  out <- fselect(groups, .cols = setdiff2(names(groups), ".rows"))
  out[[name]] <- counts
  out
}
df_add_count <- function(.data, name = "n", weights = NULL){
  groups <- group_data(.data)
  group_ids <- df_group_id(.data)
  if (!is.null(weights)){
    if (length(weights) != df_nrow(.data)){
      stop("Weights must satisfy `length(weights) == nrow(.data)`")
    }
    counts <- gsum(weights, g = group_ids)
  } else {
    counts <- cheapr::lengths_(groups[[".rows"]])[group_ids]
  }
  df_add_cols(.data, add_names(list(counts), name))
}

df_group_by_drop_default <- function(x){
  if (inherits(x, "grouped_df")){
    attr(attr(x, "groups", TRUE), ".drop", TRUE)
  } else {
    TRUE
  }
}
df_group_by_order_default <- function(x){
  if (inherits(x, "grouped_df")){
    out <- attr(attr(x, "groups", TRUE), "sorted", TRUE)
  } else {
    out <- TRUE
  }
  if (is.null(out)){
    TRUE
  } else {
    out
  }
}
vctrs_new_list_of <- function(x = list(), ptype){
  structure(x,
            ptype = ptype,
            class = c("vctrs_list_of",
                      "vctrs_vctr",
                      "list"))
}
# Like dplyr::bind_cols() but written in mostly base R
# and simpler..
df_cbind <- function(..., .repair_names = TRUE, .sep = "..."){
  out <- c(...)
  dots <- list3(...)
  nrow_range <- collapse::.range(cpp_nrows(dots), na.rm = TRUE)
  if (isTRUE(nrow_range[1] != nrow_range[2])){
    stop("All data frames must be of equal size")
  }
  out <- list_as_df(out)
  if (.repair_names){
    names(out) <- unique_name_repair(names(out))
  }
  if (length(dots) == 1){
   out <- dots[[1L]]
  } else if (length(dots) > 1){
    N <- nrow_range[1L]
    # Adjustment for 0-column only data frames
    if (df_nrow(out) != N){
      attr(out, "row.names") <- .set_row_names(N)
    }
    template <- dots[[1L]]
    # Special method for grouped_df because
    # we don't need to recalculate groups
    # Since we're not rearranging or renaming variables
    # except in the case of duplicates.
    if (inherits(template, "grouped_df") &&
        all(group_vars(template) %in% names(out))){
      out <- df_reconstruct(out, safe_ungroup(template))
      class(out) <- class(template)
      attr(out, "groups") <- attr(template, "groups")
    } else {
      out <- df_reconstruct(out, template)
    }
  }
  out
}
unique_name_repair <- function(x, .sep = "..."){
  x <- as.character(x)
  col_seq <- seq_along(x)
  which_dup <- which_(collapse::fduplicated(x, all = TRUE))
  x[which_dup] <- paste0(x[which_dup], .sep, col_seq[which_dup])
  x
}
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.time_tbl_df <- function(data, template){
  df_reconstruct(data, template)
}
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.episodes_tbl_df <- function(data, template){
  df_reconstruct(data, template)
}
#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.time_tbl_df <- function(data, i, ..., .preserve = FALSE){
  df_row_slice(data, i)
}
#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.episodes_tbl_df <- function(data, i, ..., .preserve = FALSE){
  df_row_slice(data, i)
}
