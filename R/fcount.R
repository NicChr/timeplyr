#' A fast replacement to dplyr::count()
#'
#' @description
#' This is a fast and near-identical alternative to dplyr::count() using the `collapse` package.
#' Unlike `collapse::fcount()`, this works very similarly to `dplyr::count()`.
#' The only main difference is that anything supplied to `wt`
#' is recycled and added as a data variable.
#' Other than that everything works exactly as the dplyr equivalent.
#'
#' `fcount()` and `fadd_count()` can be up to >100x faster than the dplyr equivalents.
#' `collapse` and `data.table` are used for computing, adding the count column
#' and optionally sorting.
#'
#' @param data A data frame.
#' @param ... Variables to group by.
#' @param wt Frequency weights.
#'   Can be `NULL` or a variable:
#'
#'   * If `NULL` (the default), counts the number of rows in each group.
#'   * If a variable, computes `sum(wt)` for each group.
#' @param sort If `TRUE`, will s how the largest groups at the top.
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
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' iris %>%
#'   fcount()
#' iris %>%
#'   fadd_count()
#' iris %>%
#'   group_by(Species) %>%
#'   fcount()
#' iris %>%
#'   fcount(across(where(is.numeric), mean))
#' iris %>%
#'   fadd_count(across(where(is.numeric), mean))
#' @importFrom dplyr %>%
#' @importFrom data.table :=
#' @importFrom data.table .N
#' @importFrom data.table .SD
#' @importFrom data.table .GRP
#' @importFrom dplyr .data
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom timechange time_add
#' @importFrom pillar tbl_sum
#' @export
fcount <- function(data, ..., wt = NULL, sort = FALSE, name = NULL,
                   .by = NULL, .cols = NULL){
  group_vars <- group_vars(data)
  group_info <- group_info(data, ..., .by = {{ .by }},
                           .cols = .cols,
                           ungroup = TRUE,
                           rename = TRUE)
  out <- group_info[["data"]]
  all_vars <- group_info[["all_groups"]]
  N <- nrow2(out)
  # Weights
  if (!rlang::quo_is_null(enquo(wt))){
    out <- mutate2(out, !!enquo(wt))
    wt_var <- tidy_transform_names(data, !!enquo(wt))
  } else {
    wt_var <- character(0)
  }
  if (length(wt_var) > 0L){
    wtv <- out[[wt_var]]
  }
  grp_nm <- new_var_nm(all_vars, ".group.id")
  use_only_grouped_df_groups <- length(all_vars) == 0L ||
    length(group_vars) > 0L && (length(group_vars) == length(all_vars))
  if (use_only_grouped_df_groups){
    g <- df_to_GRP(data, return.order = FALSE)
  } else {
    g <- df_to_GRP(out, .cols = all_vars, return.order = FALSE)
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
  N <- nrow2(out)
  if (is.null(name)) name <- new_n_var_nm(out)
  # Edge-case, not sure how to fix this
  if (N == 0L && length(all_vars) == 0L){
    N <- 1L
    nobs <- 0L
    out <- df_reconstruct(structure(list(),
                                    .Names = character(0),
                                    class = "data.frame",
                                    row.names = c(NA, -1L)),
                          data)
  } else if (length(wt_var) == 0){
    nobs <- group_sizes
  } else {
    nobs <- collapse::fsum(as.double(wtv),
                           g = g,
                           na.rm = TRUE,
                           use.g.names = FALSE,
                           fill = FALSE)
    if (length(nobs) == 0L || isTRUE((
      collapse::fmax(nobs, na.rm = TRUE) <= .Machine$integer.max
    ))){
      nobs <- as.integer(nobs)
    }
    # Replace NA with 0
    nobs[is.na(nobs)] <- 0L
  }
  out[[name]] <- nobs
  if (sort){
    out <- farrange(out, desc(.data[[name]]))
  }
  df_reconstruct(out, data)
}
#' @rdname fcount
#' @export
fadd_count <- function(data, ..., wt = NULL, sort = FALSE, name = NULL,
                       .by = NULL, .cols = NULL){
  group_info <- group_info(data, ..., .by = {{ .by }},
                           .cols = .cols,
                           ungroup = TRUE,
                           rename = TRUE)
  out <- group_info[["data"]]
  all_vars <- group_info[["all_groups"]]
  if (rlang::quo_is_null(enquo(wt))){
    wt_var <- character(0)
  } else {
    ncol1 <- ncol(out)
    out <- mutate2(out, !!enquo(wt))
    ncol2 <- ncol(out)
    if (ncol2 == ncol1){
      has_wt <- TRUE
    } else {
      has_wt <- FALSE
    }
    wt_var <- tidy_transform_names(data, !!enquo(wt))
    if (length(wt_var) > 0L){
      wtv <- out[[wt_var]]
      if (!has_wt){
        out <- df_rm_cols(out, wt_var)
      }
    }
  }
  group_id <- group_id(out, .cols = all_vars,
                       order = TRUE,
                       as_qg = TRUE)
  if (is.null(name)) name <- new_n_var_nm(out)
  if (length(wt_var) > 0L){
    # nobs <- nobs * wtv
    nobs <- gsum(as.double(wtv),
                   g = group_id,
                   na.rm = TRUE)
    # Replace NA with 0
    nobs <- data.table::nafill(nobs, type = "const", fill = 0, nan = NaN)
    if (all(nobs <= .Machine$integer.max, na.rm = TRUE)){
      nobs <- as.integer(nobs)
    }
  } else {
    nobs <- collapse::GRPN(group_id, expand = TRUE)
  }
  out <- dplyr::dplyr_col_modify(out, cols = setnames(list(nobs),
                                                      name))
  if (sort){
    out <- farrange(out, desc(.data[[name]]))
  }
  df_reconstruct(out, data)
}
