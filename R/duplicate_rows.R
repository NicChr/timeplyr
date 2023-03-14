#' Find duplicate rows
#'
#' @description This is a simple wrapper around `distinct`
#' and `anti_join` to return duplicate rows.
#' It basically returns all the records that `distinct()`
#' doesn't return and works very similarly to `distinct()`.
#' It is considerably faster than janitor's `get_dupes()`,
#' particularly for large data frames. It is also group-aware
#' and leaves `dplyr` groups intact.
#' To replicate janitor's `get_dupes()` functionality,
#' set `.both_ways = TRUE` and use `dplyr::add_count(...)`.
#'
#' @param data A data frame.
#' @param ... Variables used to find duplicate rows.
#' @param .keep_all If `TRUE` then all columns of data frame are kept, default is `FALSE`.
#' @param .both_ways If `TRUE` then duplication is kept from both sides, default is `FALSE`.
#' If set to `FALSE` this is equivalent to `anti_join(df, distinct(df))`.
#' @param .keep_na If `FALSE` then rows with all `NA` values are removed before
#' finding duplicates. The default is `TRUE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(nycflights13)
#'
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
#'   duplicate_rows(tailnum, dep_time, .keep_all = FALSE) %>%
#'   fadd_count(tailnum, dep_time)
#' # For every day were there multiple flights that departed at the same time?
#' flights %>%
#'   group_by(year, month, day) %>%
#'   duplicate_rows(dep_time, arr_time, .both_ways = TRUE)
#' @export
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
