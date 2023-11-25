#' A fast replacement to dplyr::count()
#'
#' @description
#' Near-identical alternative to `dplyr::count()`.
#'
#' @param data A data frame.
#' @param ... Variables to group by.
#' @param wt Frequency weights.
#'   Can be `NULL` or a variable:
#'
#'   * If `NULL` (the default), counts the number of rows in each group.
#'   * If a variable, computes `sum(wt)` for each group.
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param order Should the groups be calculated as ordered groups?
#' If `FALSE`, this will return the groups in order of first appearance,
#' and in many cases is faster.
#' If `TRUE` (the default), the groups are returned in sorted order,
#' exactly the same way as `dplyr::count`.
#' @param name The name of the new column in the output.
#'  If there's already a column called `n`,
#'  it will use `nn`.
#'  If there's a column called `n` and `n`n,
#'  it'll use `nnn`, and so on, adding `n`s until it gets a new name.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#'
#' @details
#' This is a fast and near-identical alternative to dplyr::count() using the `collapse` package.
#' Unlike `collapse::fcount()`, this works very similarly to `dplyr::count()`.
#' The only main difference is that anything supplied to `wt`
#' is recycled and added as a data variable.
#' Other than that everything works exactly as the dplyr equivalent.
#'
#' `fcount()` and `fadd_count()` can be up to >100x faster than the dplyr equivalents.
#'
#' @returns
#' A `data.frame` of frequency counts by group.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' iris %>%
#'   fcount()
#' iris %>%
#'   fadd_count(name = "count") %>%
#'   fslice_head(n = 10)
#' iris %>%
#'   group_by(Species) %>%
#'   fcount()
#' iris %>%
#'   fcount(Species)
#' iris %>%
#'   fcount(across(where(is.numeric), mean))
#'
#' ### Sorting behaviour
#'
#' # Sorted by group
#' starwars %>%
#'   fcount(hair_color)
#' # Sorted by frequency
#' starwars %>%
#'   fcount(hair_color, sort = TRUE)
#' # Groups sorted by order of first appearance (faster)
#' starwars %>%
#'   fcount(hair_color, order = FALSE)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
fcount <- function(data, ..., wt = NULL, sort = FALSE, order = TRUE,
                   name = NULL, .by = NULL, .cols = NULL){
  group_vars <- group_vars(data)
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = TRUE)
  out <- group_info[["data"]]
  all_vars <- group_info[["all_groups"]]
  N <- df_nrow(out)
  # Weights
  if (!rlang::quo_is_null(enquo(wt))){
    out_info <- mutate_summary_grouped(out, !!enquo(wt))
    wt_var <- out_info[["cols"]]
    out <- out_info[["data"]]
  } else {
    wt_var <- character()
  }
  if (length(wt_var) > 0L){
    wtv <- out[[wt_var]]
  }
  use_only_grouped_df_groups <- length(all_vars) == 0L ||
    (order && length(group_vars) > 0L && length(group_vars) == length(all_vars))
  if (use_only_grouped_df_groups){
    g <- df_to_GRP(data, return.order = FALSE, order = order)
  } else {
    g <- df_to_GRP(out, .cols = all_vars, return.order = FALSE,
                   order = order)
  }
  group_data <- GRP_groups(g)
  if (is.null(group_data)){
      out <- fselect(out, .cols = all_vars)
      gstarts <- GRP_starts(g)
      out <- df_row_slice(out, gstarts, reconstruct = FALSE)
  } else {
    out <- group_data
  }
  group_sizes <- GRP_group_sizes(g)
  if (length(all_vars) == 0L){
   g <- NULL
  }
  N <- df_nrow(out)
  if (is.null(name)){
    name <- new_n_var_nm(out)
  }
  # Edge-case, not sure how to fix this
  if (N == 0L && length(all_vars) == 0L){
    out <- df_init(out, 1L)
  }
  if (length(wt_var) == 0){
    nobs <- group_sizes
  } else {
    nobs <- collapse::fsum(as.double(wtv),
                           g = g,
                           na.rm = TRUE,
                           use.g.names = FALSE,
                           fill = FALSE)
    if (isTRUE(all_integerable(nobs))){
      nobs <- as.integer(nobs)
    }
    # Replace NA with 0
    nobs[cpp_which(is.na(nobs))] <- 0L
  }
  out[[name]] <- nobs
  if (sort){
    row_order <- radix_order(-nobs)
    out <- df_row_slice(out, row_order)
  }
  df_reconstruct(out, data)
}
#' @rdname fcount
#' @export
fadd_count <- function(data, ..., wt = NULL, sort = FALSE, order = TRUE,
                       name = NULL, .by = NULL, .cols = NULL){
  group_vars <- group_vars(data)
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = TRUE)
  out <- group_info[["data"]]
  all_vars <- group_info[["all_groups"]]
  if (rlang::quo_is_null(enquo(wt))){
    wt_var <- character()
  } else {
    ncol1 <- df_ncol(out)
    out_info <- mutate_summary_grouped(out, !!enquo(wt))
    out <- out_info[["data"]]
    ncol2 <- df_ncol(out)
    has_wt <- (ncol2 == ncol1)
    wt_var <- out_info[["cols"]]
    if (length(wt_var) > 0L){
      wtv <- out[[wt_var]]
      if (!has_wt){
        out <- df_rm_cols(out, wt_var)
      }
    }
  }
  use_only_grouped_df_groups <- length(all_vars) == 0L ||
    (order && length(group_vars) > 0L && length(group_vars) == length(all_vars))
  if (use_only_grouped_df_groups){
    g <- df_to_GRP(data, return.order = FALSE, order = order)
  } else {
    g <- df_to_GRP(out, .cols = all_vars, return.order = FALSE, order = order)
  }
  if (is.null(name)){
    name <- new_n_var_nm(out)
  }
  if (length(wt_var) > 0L){
    if (length(all_vars) == 0L){
      g <- NULL
    }
    nobs <- gsum(as.double(wtv),
                   g = g,
                   na.rm = TRUE)
    # Replace NA with 0
    nobs[cpp_which(is.na(nobs))] <- 0
    if (isTRUE(all_integerable(nobs))){
      nobs <- as.integer(nobs)
    }
  } else {
    nobs <- GRP_expanded_group_sizes(g)
  }
  out <- dplyr::dplyr_col_modify(out, cols = add_names(list(nobs),
                                                      name))
  if (sort){
    row_order <- radix_order(-out[[name]])
    out <- df_row_slice(out, row_order)
  }
  df_reconstruct(out, data)
}
