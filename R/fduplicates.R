#' Find duplicate rows
#'
#' @description This functions works like `dplyr::distinct()` in its handling of
#' arguments and data-masking but returns duplicate rows.
#' It is exceptionally faster than `dplyr::distinct()` and `janitor::get_dupes()`
#' when there are many groups.
#'
#' @param data A data frame.
#' @param ... Variables used to find duplicate rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept, default is `FALSE`.
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

  # Coerce to DT
  out <- data.table::copy(out)
  data.table::setDT(out)
  # Groups
  grp_nm <- new_var_nm(names(data), ".group.id")
  out[, (grp_nm) := group_id(data, .by = {{ .by }},
                             sort = TRUE)]
  # If no variables selected then all variables used
  if (dots_length(...) == 0L){
    out_vars <- c(group_vars, setdiff(names(out), c(grp_nm, group_vars)))
    dup_vars <- setdiff(names(out), c(grp_nm, group_vars))
  } else {
    out_vars <- c(group_vars, extra_groups)
    dup_vars <- extra_groups
  }
  # Add group ID for all groups
  grp_nm2 <- new_var_nm(c(names(out), grp_nm), ".group.id")
  if (isTRUE(all.equal(group_vars, out_vars))){
    out[, (grp_nm2) := get(grp_nm)]
  } else {
    out[, (grp_nm2) := group_id(out, all_of(out_vars), sort = TRUE)]
  }

  n_var_nm <- new_n_var_nm(names(out))
  out <- out %>%
    fadd_count(across(all_of(grp_nm2)),
               name = n_var_nm,
               keep_class = FALSE)
  out <- out[get(n_var_nm) > 1, ]
  if (!.add_count) out[, (n_var_nm) := NULL]
  if (.add_count) out_vars <- c(out_vars, n_var_nm)
  if (!.both_ways){
    # Add column with row ID to keep track of rows
    id_var <- new_var_nm(c(names(out), grp_nm, grp_nm2), ".id")
    out[, (id_var) := gseq_len(nrow2(out), g = get(grp_nm2))]
    out <- out[get(id_var) > 1, ]
    out[, (id_var) := NULL]
  }
  # Remove empty rows (rows with all NA values)
  if (!.keep_na){
    out <- out %>%
      dplyr::filter(!dplyr::if_all(.cols = all_of(dup_vars), is.na))
  }
  # Keep all columns or only duplicate search ones?
  if (.keep_all){
    out %>%
      dplyr::select(-all_of(c(grp_nm, grp_nm2))) %>%
      df_reconstruct(data)
  } else {
    out %>%
      dplyr::select(all_of(out_vars)) %>%
      df_reconstruct(data)
  }
}
# duplicate_rows <- function(data, ..., .keep_all = FALSE,
#                            .both_ways = FALSE, .keep_na = TRUE,
#                            .by = NULL){
#   # Data groups
#   grp_nm <- new_var_nm(names(data), ".group.id")
#   out <- data %>%
#     add_group_id(.by = {{ .by }},
#                  sort = TRUE,
#                  .name = grp_nm) %>%
#     safe_ungroup() %>%
#     dplyr::mutate(!!!enquos(...))
#   group_info <- get_group_info(data, !!!enquos(...), type = "data-mask",
#                                .by = {{ .by }})
#   group_vars <- group_info[["dplyr_groups"]]
#   extra_groups <- group_info[["extra_groups"]]
#   # If no variables selected then all variables used
#   if (dots_length(...) == 0L){
#     out_vars <- c(group_vars, setdiff(names(out), c(grp_nm, group_vars)))
#     dup_vars <- setdiff(names(out), c(grp_nm, group_vars))
#   } else {
#     out_vars <- c(group_vars, extra_groups)
#     dup_vars <- extra_groups
#   }
#   # Add group ID for all groups
#   grp_nm2 <- new_var_nm(c(names(out), grp_nm), ".group.id")
#   out[[grp_nm2]] <- group_id(out, all_of(out_vars),
#                              sort = TRUE)
#   # If include first instance of each duplicate, use the count method
#   # Otherwise anti-join method
#   if (.both_ways){
#     n_var_nm <- new_n_var_nm(names(out))
#     out <- out %>%
#       fadd_count(across(all_of(grp_nm2)),
#                  name = n_var_nm) %>%
#       dplyr::filter(.data[[n_var_nm]] > 1) %>%
#       dplyr::select(-all_of(n_var_nm))
#   } else {
#     # Add column with row ID to keep track of rows
#     id_var <- new_var_nm(c(names(out), grp_nm, grp_nm2), ".id")
#     out[[id_var]] <- gseq_len(nrow2(out), g = out[[grp_nm]])
#     # data with ID and dup cols
#     df_unique <- collapse::funique(out,
#                                    cols = grp_nm2)
#     # Duplicate rows
#     out <- out %>%
#       dplyr::anti_join(df_unique, by = c(grp_nm, id_var))
#     out[[id_var]] <- NULL
#   }
#   # Remove empty rows (rows with all NA values)
#   if (!.keep_na){
#     out <- out %>%
#       dplyr::filter(!dplyr::if_all(.cols = all_of(dup_vars), is.na))
#   }
#   # Keep all columns or only duplicate search ones?
#   if (.keep_all){
#     out %>%
#       dplyr::select(-all_of(c(grp_nm, grp_nm2))) %>%
#       df_reconstruct(data)
#   } else {
#     out %>%
#       dplyr::select(all_of(out_vars)) %>%
#       df_reconstruct(data)
#   }
# }
# duplicate_rows <- function(data, ..., .keep_all = FALSE,
#                            .both_ways = FALSE, .keep_na = TRUE,
#                            .by = NULL){
#   # Data groups
#   grp_nm <- new_var_nm(names(data), ".group.id")
#   out <- data %>%
#     add_group_id(.by = {{ .by }},
#                  sort = TRUE,
#                  .name = grp_nm) %>%
#     safe_ungroup() %>%
#     dplyr::mutate(!!!enquos(...))
#   group_info <- get_group_info(data, !!!enquos(...), type = "data-mask",
#                                .by = {{ .by }})
#   group_vars <- group_info[["dplyr_groups"]]
#   extra_groups <- group_info[["extra_groups"]]
#   # If no variables selected then all variables used
#   if (dots_length(...) == 0L){
#     out_vars <- c(group_vars, setdiff(names(out), c(grp_nm, group_vars)))
#     dup_vars <- setdiff(names(out), c(grp_nm, group_vars))
#   } else {
#     out_vars <- c(group_vars, extra_groups)
#     dup_vars <- extra_groups
#   }
#   # Add group ID for extra groups
#   grp_nm2 <- new_var_nm(c(names(out), grp_nm), ".group.id")
#   out[[grp_nm2]] <- group_id(out, all_of(dup_vars),
#                              sort = TRUE)
#   # If include first instance of each duplicate, use the count method
#   # Otherwise anti-join method
#   if (.both_ways){
#     n_var_nm <- new_n_var_nm(names(out))
#     out <- out %>%
#       fadd_count(across(all_of(c(grp_nm, grp_nm2))),
#                  name = n_var_nm) %>%
#       dplyr::filter(.data[[n_var_nm]] > 1) %>%
#       dplyr::select(-all_of(n_var_nm))
#   } else {
#     # Add column with row ID to keep track of rows
#     id_var <- new_var_nm(c(names(out), grp_nm, grp_nm2), ".id")
#     out[[id_var]] <- gseq_len(nrow2(out), g = out[[grp_nm]])
#     # data with ID and dup cols
#     df_unique <- collapse::funique(out,
#                                    cols = c(grp_nm, grp_nm2))
#     # Duplicate rows
#     out <- out %>%
#       dplyr::anti_join(df_unique, by = c(grp_nm, id_var))
#     out[[id_var]] <- NULL
#   }
#   # Remove empty rows (rows with all NA values)
#   if (!.keep_na){
#     out <- out %>%
#       dplyr::filter(!dplyr::if_all(.cols = all_of(dup_vars), is.na))
#   }
#   # Keep all columns or only duplicate search ones?
#   if (.keep_all){
#     out %>%
#       dplyr::select(-all_of(c(grp_nm, grp_nm2))) %>%
#       df_reconstruct(data)
#   } else {
#     out %>%
#       dplyr::select(all_of(out_vars)) %>%
#       df_reconstruct(data)
#   }
# }
# duplicate_rows <- function(data, ..., .keep_all = FALSE,
#                            .both_ways = FALSE, .keep_na = TRUE,
#                            .by = NULL){
#   # Data groups
#   grp_nm <- new_var_nm(names(data), ".group.id")
#   out <- data %>%
#     add_group_id(.by = {{ .by }},
#                  sort = TRUE,
#                  .name = grp_nm) %>%
#     safe_ungroup() %>%
#     dplyr::mutate(!!!enquos(...))
#   group_info <- get_group_info(data, !!!enquos(...), type = "data-mask",
#                                .by = {{ .by }})
#   group_vars <- group_info[["dplyr_groups"]]
#   extra_groups <- group_info[["extra_groups"]]
#   # If no variables selected then all variables used
#   if (dots_length(...) == 0L){
#     out_vars <- c(group_vars, setdiff(names(out), c(grp_nm, group_vars)))
#     dup_vars <- setdiff(names(out), c(grp_nm, group_vars))
#   } else {
#     out_vars <- c(group_vars, extra_groups)
#     dup_vars <- extra_groups
#   }
#   # Add group ID for extra groups
#   grp_nm2 <- new_var_nm(c(names(out), grp_nm), ".group.id")
#   out[[grp_nm2]] <- group_id(out, all_of(dup_vars),
#                              sort = TRUE)
#   # Add column with row ID to keep track of rows
#   id_var <- new_var_nm(c(names(out), grp_nm, grp_nm2), ".id")
#   out[[id_var]] <- gseq_len(nrow2(out), g = out[[grp_nm]])
#   # data with ID and dup cols
#   df_unique <- out %>%
#     dplyr::distinct(across(all_of(c(grp_nm, grp_nm2))),
#                     .keep_all = TRUE)
#   # Duplicate rows
#   out2 <- out %>%
#     dplyr::anti_join(df_unique, by = c(grp_nm, id_var))
#   # Remove empty rows (rows with all NA values)
#   if (!.keep_na){
#     out2 <- out2 %>%
#       dplyr::filter(!dplyr::if_all(.cols = all_of(dup_vars), is.na))
#   }
#   # Keep duplicates including first instance of duplicated rows
#   if (.both_ways) out2 <- dplyr::semi_join(out,
#                                            dplyr::select(out2,
#                                                          all_of(c(grp_nm, grp_nm2))),
#                                            by = c(grp_nm, grp_nm2))
#   # Keep all columns or only duplicate search ones?
#   if (.keep_all){
#     out2 %>%
#       dplyr::select(-all_of(c(id_var, grp_nm, grp_nm2))) %>%
#       df_reconstruct(data)
#   } else {
#     out2 %>%
#       dplyr::select(all_of(out_vars)) %>%
#       df_reconstruct(data)
#   }
# }
# Tidyverse-only version.
duplicate_rows <- function(data, ..., .keep_all = FALSE,
                           .both_ways = FALSE, .keep_na = TRUE,
                           .by = NULL){
  # Data groups
  group_vars <- get_groups(data, .by = {{ .by }})
  # If no variables selected then all variables used
  if (dots_length(...) == 0){
    dup_vars_all <- names(data)
  } else {
    if (.keep_all){
      data <- data %>%
        safe_ungroup() %>%
        dplyr::mutate(!!!enquos(...)) %>%
        df_reconstruct(data)
      dup_vars_all <- tidy_transform_names(safe_ungroup(data),
                                           !!!enquos(...))
    } else {
      data <- data %>%
        safe_ungroup() %>%
        dplyr::mutate(!!!enquos(...)) %>%
        df_reconstruct(data)
      keep_vars <- tidy_transform_names(safe_ungroup(data),
                                        !!!enquos(...))
      data <- data %>%
        dplyr::select(all_of(c(group_vars, keep_vars)))
      dup_vars_all <- names(data)
    }
  }
  dup_vars <- setdiff(dup_vars_all, group_vars)
  # Add column with row ID to keep track of rows
  id_var <- new_var_nm(data, ".id")
  data <- dplyr::mutate(data, .id = dplyr::row_number(),
                        .by = {{ .by }})
  # data with ID and dup cols
  df_unique <- data %>%
    dplyr::select(all_of(c(id_var, dup_vars_all))) %>%
    dplyr::distinct(across(all_of(dup_vars_all)), .keep_all = TRUE)
  # Duplicate rows
  out <- data %>%
    dplyr::anti_join(df_unique, by = c(group_vars, id_var))
  # Remove empty rows (rows with all NA values)
  if (!.keep_na){
    out <- out %>%
      dplyr::filter(!dplyr::if_all(.cols = all_of(dup_vars), is.na))
  }
  # Keep duplicates including first instance of duplicated rows
  if (.both_ways) out <- dplyr::semi_join(data,
                                            dplyr::select(out,
                                                          all_of(dup_vars_all)),
                                            by = dup_vars_all)
  # Keep all columns or only duplicate search ones?
  if (.keep_all) {
    out %>%
      dplyr::select(-all_of(id_var))
  } else {
    out %>%
      dplyr::select(all_of(dup_vars_all))
  }
}
