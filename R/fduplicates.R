#' Find duplicate rows
#'
#' @description This functions works like `dplyr::distinct()` in its handling of
#' arguments and data-masking but returns duplicate rows.
#' It is exceptionally faster than `dplyr::distinct()` and `janitor::get_dupes()`
#' when there are many groups.
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
#' @export
fduplicates <- function(data, ..., .keep_all = FALSE,
                        .both_ways = FALSE, .add_count = FALSE,
                        .keep_na = TRUE, .by = NULL){
  out <- data %>%
    safe_ungroup() %>%
    dplyr::mutate(!!!enquos(...))
  group_info <- get_group_info(data, !!!enquos(...), type = "data-mask",
                               .by = {{ .by }})
  group_vars <- group_info[["dplyr_groups"]]
  extra_groups <- group_info[["extra_groups"]]
  all_groups <- group_info[["all_groups"]]
  # Coerce to DT
  out_nms <- names(out)
  out <- data.table::copy(out)
  data.table::setDT(out)
  # If no variables selected then all variables used
  if (dots_length(...) == 0L){
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
  # Remove uneccessary cols
  set_rm_cols(out, setdiff(names(out), out_vars))
  data.table::setcolorder(out, out_vars)
  # Groups
  grp_nm <- new_var_nm(names(out), ".group.id")
  out[, (grp_nm) := group_id(data, .by = {{ .by }},
                             sort = TRUE)]
  # Add group ID for all groups
  grp_nm2 <- new_var_nm(c(names(out), grp_nm), ".group.id")
  out[, (grp_nm2) := group_id(out, all_of(dup_vars), sort = FALSE)]

  n_var_nm <- new_n_var_nm(names(out))
  out[, (n_var_nm) := collapse::GRPN(get(grp_nm2), expand = TRUE)]
  out <- out[get(n_var_nm) > 1, ]
  if (!.add_count) out[, (n_var_nm) := NULL]
  if (.add_count) out_vars <- c(out_vars, n_var_nm)
  if (!.both_ways){
    out <- out[collapse::fduplicated(get(grp_nm2), all = FALSE), ]
  }
  # Remove empty rows (rows with all NA values)
  if (!.keep_na){
    out <- out %>%
      dplyr::filter(!dplyr::if_all(.cols = all_of(dup_vars), is.na))
  }
  # Remove added group id cols
  set_rm_cols(out, c(grp_nm, grp_nm2))
  df_reconstruct(out, data)
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
