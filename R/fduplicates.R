#' Find duplicate rows
#'
#' @description This function works like `dplyr::distinct()` in its handling of
#' arguments and data-masking but returns duplicate rows.
#' In certain situations in can be much faster than `data %>% group_by() %>% filter(n() > 1)`
#' when there are many groups.
#' `fduplicates2()` returns the same output but uses a different
#' method which utilises joins and is written almost entirely using dplyr.
#'
#'
#' @param data A data frame.
#' @param ... Variables used to find duplicate rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
#' @param .both_ways If `TRUE` then duplicates and non-duplicate first instances
#' are retained. The default is `FALSE` which returns only duplicated rows.
#' @param .add_count If `TRUE` then a count column is added to denote the
#' number of duplicates (including first non-duplicate instance).
#' The naming convention of this column follows `dplyr::add_count()`.
#' @param .keep_na If `FALSE` then rows with all `NA` values are removed.
#' The default is `TRUE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(nycflights13)
#'
#' # Duplicates across all columns
#' flights %>%
#'   fduplicates()
#' # Duplicate flights with the same tail number and departure time
#' flights %>%
#'   fduplicates(tailnum, dep_time)
#' # Can use tidyverse select notation
#' flights %>%
#'   fduplicates(across(contains("dep_time")), .keep_all = FALSE)
#' # Similar to janitor::get_dupes()
#' flights %>%
#'   fduplicates(tailnum, dep_time, .keep_all = FALSE, .add_count = TRUE)
#' # For every day were there multiple flights that departed at the same time?
#' flights %>%
#'   group_by(year, month, day) %>%
#'   fduplicates(dep_time, arr_time, .both_ways = TRUE)
#' @rdname fduplicates
#' @export
fduplicates <- function(data, ..., .keep_all = FALSE,
                        .both_ways = FALSE, .add_count = FALSE,
                        .keep_na = TRUE,
                        .by = NULL, .cols = NULL){
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
  out <- fselect(out, .cols = out_vars)
  # Groups
  # Add group ID for all groups
  grp_nm <- new_var_nm(names(out), ".group.id")
  out[[grp_nm]] <- group_id(out, .cols = dup_vars, as_qg = TRUE)
  n_var_nm <- new_n_var_nm(names(out))
  out[[n_var_nm]] <- collapse::GRPN(out[[grp_nm]], expand = TRUE)
  out <- df_row_slice(out, which(out[[n_var_nm]] > 1))
  if (!.add_count) out[[n_var_nm]] <- NULL
  if (.add_count) out_vars <- c(out_vars, n_var_nm)
  if (!.both_ways){
    out <- df_row_slice(out, which(collapse::fduplicated(out[[grp_nm]], all = FALSE)),
                        reconstruct = FALSE)
  }
  # Remove empty rows (rows with all NA values)
  if (!.keep_na){
    out <- collapse::na_omit(out, cols = dup_vars, prop = 1)
  }
  # Remove added group id cols
  out[[grp_nm]] <- NULL
  df_reconstruct(out, data)
}
#' @rdname fduplicates
#' @export
fduplicates2 <- function(data, ..., .keep_all = FALSE,
                           .both_ways = FALSE, .add_count = FALSE,
                           .keep_na = TRUE, .by = NULL){
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
    dplyr::mutate(!!grp_nm := group_id(data, .by = {{ .by }}),
                  !!id_nm := gseq_len(nrow2(data), g = .data[[grp_nm]]))
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
  if (!.keep_na){
    out2 <- out2 %>%
      dplyr::filter(!dplyr::if_all(.cols = all_of(dup_vars), is.na))
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
