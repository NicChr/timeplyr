#' Find duplicate rows
#'
#'
#' @param data A data frame.
#' @param ... Variables used to find duplicate rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
#' @param .both_ways If `TRUE` then duplicates and non-duplicate first instances
#' are retained. The default is `FALSE` which returns only duplicate rows. \cr
#' Setting this to `TRUE` can be particularly useful when examining
#' the differences between duplicate rows.
#' @param .add_count If `TRUE` then a count column is added to denote the
#' number of duplicates (including first non-duplicate instance).
#' The naming convention of this column follows `dplyr::add_count()`.
#' @param .drop_empty If `TRUE` then empty rows with all `NA` values are removed.
#' The default is `FALSE`.
#' @param sort Should result be sorted?
#' If `FALSE` (the default), then rows are returned in the exact same order as
#' they appear in the data.
#' If `TRUE` then the duplicate rows are sorted.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param .keep_na \bold{Deprecated}. Please use `.drop_empty` instead.
#'
#' @returns
#' A `data.frame` of duplicate rows.
#'
#' @details
#' This function works like `dplyr::distinct()` in its handling of
#' arguments and data-masking but returns duplicate rows.
#' In certain situations in can be much faster than `data %>% group_by() %>% filter(n() > 1)`
#' when there are many groups.
#' `fduplicates2()` returns the same output but uses a different
#' method which utilises joins and is written almost entirely using dplyr.
#'
#' @seealso [fcount] [group_collapse] [fdistinct]
#'
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(nycflights13)
#' \dontshow{
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # Duplicates across all columns
#' flights %>%
#'   duplicate_rows()
#' # Duplicate flights with the same tail number and departure time
#' flights %>%
#'   duplicate_rows(tailnum, dep_time)
#' # Can use tidyverse select notation
#' flights %>%
#'   duplicate_rows(across(contains("dep_time")), .keep_all = FALSE)
#' # Similar to janitor::get_dupes()
#' flights %>%
#'   duplicate_rows(tailnum, dep_time, .keep_all = FALSE, .add_count = TRUE)
#' # For every day were there multiple flights that departed at the same time?
#' flights %>%
#'   group_by(year, month, day) %>%
#'   duplicate_rows(dep_time, arr_time, .both_ways = TRUE)
#' @rdname duplicate_rows
#' @export
duplicate_rows <- function(data, ..., .keep_all = FALSE,
                           .both_ways = FALSE, .add_count = FALSE,
                           .drop_empty = FALSE, sort = FALSE,
                           .by = NULL, .cols = NULL,
                           .keep_na = TRUE){
  if (!missing(.keep_na)){
    warning(".keep_na has been deprecated, please use .drop_empty")
    .drop_empty <- !.keep_na
  }
  n_dots <- dots_length(...)
  group_info <- group_info(data, ..., .by = {{ .by }},
                           .cols = .cols,
                           ungroup = TRUE,
                           rename = TRUE)
  all_groups <- group_info[["all_groups"]]
  out <- group_info[["data"]]
  out_nms <- names(out)
  # If no variables selected then all variables used
  if (n_dots == 0 && is.null(.cols)){
    dup_vars <- out_nms
    out_vars <- dup_vars
  } else {
    dup_vars <- all_groups
    if (.keep_all){
      out_vars <- out_nms
    } else {
      out_vars <- dup_vars
    }
  }
  if (length(group_info[["extra_groups"]]) == 0L){
    out <- data
  }
  out <- fselect(out, .cols = out_vars)
  # Groups
  groups <- df_to_GRP(out, .cols = dup_vars,
                      return.order = FALSE,
                      return.groups = FALSE,
                      order = FALSE)
  if (.add_count){
    group_sizes <- GRP_expanded_group_sizes(groups)
    n_var_nm <- new_n_var_nm(out)
    out[[n_var_nm]] <- group_sizes
  }
  which_dup <- GRP_which_duplicated(groups, all = .both_ways)
  out <- df_row_slice(out, which_dup)
  if (sort){
    out <- farrange(out, .cols = dup_vars)
  }
  # Remove empty rows (rows with all NA values)
  if (.drop_empty){
    out <- df_drop_empty(out, .cols = dup_vars)
  }
  df_reconstruct(out, data)
}
#' @rdname duplicate_rows
#' @export
fduplicates <- duplicate_rows
#' @rdname duplicate_rows
#' @export
fduplicates2 <- function(data, ..., .keep_all = FALSE,
                         .both_ways = FALSE, .add_count = FALSE,
                         .drop_empty = FALSE, .by = NULL,
                         .keep_na = TRUE){
  if (!missing(.keep_na)){
    warning(".keep_na has been deprecated, please use .drop_empty")
    .drop_empty <- !.keep_na
  }
  n_dots <- dots_length(...)
  out <- safe_ungroup(data)
  if (n_dots > 0){
    out <- out %>%
      dplyr::mutate(...)
  }
  group_info <- get_group_info(data, ...,
                               type = "data-mask",
                               .by = {{ .by }})
  group_vars <- group_info[["dplyr_groups"]]
  extra_groups <- group_info[["extra_groups"]]
  all_groups <- group_info[["all_groups"]]
  if (n_dots == 0){
    dup_vars <- names(out)
    out_vars <- dup_vars
  } else {
    dup_vars <- all_groups
    if (.keep_all){
      out_vars <- names(out)
    } else {
      out_vars <- dup_vars
    }
  }
  dup_vars <- setdiff(dup_vars, group_vars)
  # Group ID
  grp_nm <- new_var_nm(names(out), ".group.id")
  # Row ID (per group)
  id_nm <- new_var_nm(names(out), ".id")
  out <- out %>%
    dplyr::select(all_of(out_vars)) %>%
    dplyr::group_by(across(all_of(group_vars))) %>%
    dplyr::mutate(!!grp_nm := dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!id_nm := dplyr::row_number(),
                  .by = dplyr::all_of(grp_nm))
  if (.add_count){
    n_var <- new_n_var_nm(names(out))
    out <- dplyr::add_count(out, across(all_of(c(grp_nm, dup_vars))), name = n_var)
    out_vars <- c(out_vars, n_var)
  }
  # data with ID and dup cols
  df_unique <- out %>%
    dplyr::distinct(across(all_of(c(grp_nm, dup_vars))),
                    .keep_all = TRUE)
  # Duplicate rows
  out2 <- out %>%
    dplyr::anti_join(df_unique, by = c(grp_nm, id_nm))
  # Remove empty rows (rows with all NA values)
  if (.drop_empty){
    out2 <- dplyr_drop_empty(out2, .cols = all_of(dup_vars))
  }
  # Keep duplicates including first instance of duplicated rows
  if (.both_ways){
    out2 <- dplyr::semi_join(out,
                             dplyr::select(out2,
                                           all_of(c(grp_nm, dup_vars))),
                             by = c(grp_nm, dup_vars))
  }
  out2[[grp_nm]] <- NULL
  out2[[id_nm]] <- NULL
  df_reconstruct(out2, data)
}
# Tidyverse-only version.
# duplicate_rows <- function(data, ..., .keep_all = FALSE,
#                            .both_ways = FALSE, .keep_na = TRUE,
#                            .by = NULL){
#   # Data groups
#   group_vars <- get_groups(data, .by = {{ .by }})
#   # If no variables selected then all variables used
#   if (dots_length(...) == 0){
#     dup_vars_all <- names(data)
#   } else {
#     if (.keep_all){
#       data <- data %>%
#         safe_ungroup() %>%
#         dplyr::mutate(!!!enquos(...)) %>%
#         df_reconstruct(data)
#       dup_vars_all <- tidy_transform_names(safe_ungroup(data),
#                                            !!!enquos(...))
#     } else {
#       data <- data %>%
#         safe_ungroup() %>%
#         dplyr::mutate(!!!enquos(...)) %>%
#         df_reconstruct(data)
#       keep_vars <- tidy_transform_names(safe_ungroup(data),
#                                         !!!enquos(...))
#       data <- data %>%
#         dplyr::select(all_of(c(group_vars, keep_vars)))
#       dup_vars_all <- names(data)
#     }
#   }
#   dup_vars <- setdiff(dup_vars_all, group_vars)
#   # Add column with row ID to keep track of rows
#   id_var <- new_var_nm(data, ".id")
#   data <- dplyr::mutate(data, .id = dplyr::row_number(),
#                         .by = {{ .by }})
#   # data with ID and dup cols
#   df_unique <- data %>%
#     dplyr::select(all_of(c(id_var, dup_vars_all))) %>%
#     dplyr::distinct(across(all_of(dup_vars_all)), .keep_all = TRUE)
#   # Duplicate rows
#   out <- data %>%
#     dplyr::anti_join(df_unique, by = c(group_vars, id_var))
#   # Remove empty rows (rows with all NA values)
#   if (!.keep_na){
#     out <- out %>%
#       dplyr::filter(!dplyr::if_all(.cols = all_of(dup_vars), is.na))
#   }
#   # Keep duplicates including first instance of duplicated rows
#   if (.both_ways) out <- dplyr::semi_join(data,
#                                             dplyr::select(out,
#                                                           all_of(dup_vars_all)),
#                                             by = dup_vars_all)
#   # Keep all columns or only duplicate search ones?
#   if (.keep_all) {
#     out %>%
#       dplyr::select(-all_of(id_var))
#   } else {
#     out %>%
#       dplyr::select(all_of(dup_vars_all))
#   }
# }
