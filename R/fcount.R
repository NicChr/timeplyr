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
#' @param keep_class (Deprecated). This argument is no longer used.
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
#' @importFrom dplyr .data
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom timechange time_add
#' @export
fcount <- function(data, ..., wt = NULL, sort = FALSE, name = NULL,
                   .by = NULL){
  # Ungrouped mutate
  out <- data %>%
    safe_ungroup() %>%
    dplyr::mutate(!!!enquos(...), !!enquo(wt))
  # Weights
  if (!rlang::quo_is_null(enquo(wt))){
    wt_var <- tidy_transform_names(safe_ungroup(data), !!enquo(wt))
  } else {
    wt_var <- character(0)
  }
  if (length(wt_var) > 0L){
    wtv <- out[[wt_var]]
  }
  group_info <- get_group_info(data, !!!enquos(...),
                               type = "data-mask",
                               .by = {{ .by }})
  group_vars <- group_info[["dplyr_groups"]]
  data_vars <- group_info[["extra_groups"]]
  all_vars <- group_info[["all_groups"]]
  grp_nm <- new_var_nm(c(group_vars, data_vars), ".group.id")
  out <- out %>%
    add_group_id(all_of(all_vars),
                 order = TRUE,
                 as_qg = TRUE,
                 .name = grp_nm) %>%
    collapse::fselect(c(grp_nm, group_vars, data_vars))

  if (is.null(name)) name <- new_n_var_nm(out)
  group_id <- out[[grp_nm]]
  # Keep unique groups and sort
  if (nrow2(out) >= 2L){
    out <- collapse::funique(out, cols = grp_nm, sort = TRUE)
  }
  N <- nrow2(out)
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
    nobs <- collapse::GRPN(group_id, expand = FALSE)
  } else {
    nobs <- collapse::fsum(as.double(wtv),
                           g = group_id,
                           na.rm = TRUE,
                           use.g.names = FALSE,
                           fill = FALSE)
    if (all(collapse::na_rm(nobs) <= .Machine$integer.max)){
      nobs <- as.integer(nobs)
    }
    # Replace NA with 0
    nobs <- data.table::nafill(nobs, type = "const", fill = 0, nan = NaN)
  }
  out[[name]] <- nobs
  if (sort){
    out <- df_row_slice(out, radix_order(out[[name]], decreasing = TRUE),
                        reconstruct = FALSE)
    # out <- out[radix_order(out[[name]], decreasing = TRUE), , drop = FALSE]
  }
  # Set row.names attr
  out[[grp_nm]] <- NULL
  attr(out, "row.names") <- seq_len(N)
  df_reconstruct(out, data)
}
#' @rdname fcount
#' @export
fadd_count <- function(data, ..., wt = NULL, sort = FALSE, name = NULL,
                       .by = NULL,
                       keep_class = TRUE){
  out <- data %>%
    safe_ungroup() %>%
    dplyr::mutate(!!!enquos(...))

  ncol1 <- ncol(out)
  out <- dplyr::mutate(out, !!enquo(wt))
  ncol2 <- ncol(out)
  if (ncol2 == ncol1){
    has_wt <- TRUE
  } else {
    has_wt <- FALSE
  }
  if (rlang::quo_is_null(enquo(wt))){
    wt_var <- character(0)
  } else {
    wt_var <- tidy_transform_names(safe_ungroup(data), !!enquo(wt))
  }
  if (length(wt_var) > 0L){
    wtv <- out[[wt_var]]
    if (!has_wt) out[[wt_var]] <- NULL
  }
  group_info <- get_group_info(data, !!!enquos(...),
                               type = "data-mask",
                               .by = {{ .by }})
  group_vars <- group_info[["dplyr_groups"]]
  all_vars <- group_info[["all_groups"]]

  group_id <- group_id(out, all_of(all_vars), order = TRUE,
                       as_qg = TRUE)
  if (is.null(name)) name <- new_n_var_nm(out)
  if (length(wt_var) > 0L){
    # nobs <- nobs * wtv
    nobs <- gsum(as.double(wtv),
                   g = group_id,
                   na.rm = TRUE)
    # Replace NA with 0
    nobs[is.na(nobs)] <- 0
    if (all(nobs <= .Machine$integer.max, na.rm = TRUE)){
      nobs <- as.integer(nobs)
    }
  } else {
    nobs <- collapse::GRPN(group_id, expand = TRUE)
  }
  out[[name]] <- nobs
  if (sort){
    out <- df_row_slice(out, radix_order(out[[name]],
                                         decreasing = TRUE),
                        reconstruct = FALSE)
  }
  df_reconstruct(out, data)
}
