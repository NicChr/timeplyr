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
#' @param keep_class Logical. If `TRUE` then the class of the input data is retained.
#' If `FALSE`, which is sometimes faster, a `data.table` is returned.
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
#' iris %>%
#'   fadd_count(Species,
#'              keep_class = FALSE) # Data.table result
#' @importFrom dplyr %>%
#' @importFrom data.table %between%
#' @importFrom dplyr .data
#' @importFrom dplyr group_vars
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom timechange time_add
#' @export
fcount <- function(data, ..., wt = NULL, sort = FALSE, name = NULL,
                   .by = NULL){
  # Temporary fix as collapse grouping doesn't work on lubridate intervals
  if (any(purrr::map_lgl(data, lubridate::is.interval))){
    message("A variable of class 'interval' exists.
    The grouping will be done using 'dplyr' until the issue is fixed.
            You can report the issue at
            https://github.com/SebKrantz/collapse/issues")
    group_vars <- get_groups(data, {{ .by }})
    return(dplyr::count(data,
                        dplyr::across(all_of(group_vars) ),
                        !!!enquos(...),
                        wt = !!enquo(wt),
                        sort = sort, name = name))
  }
  # If all dot args are data variables, don't transform
  out <- safe_ungroup(data)
  out <- out %>%
    # Ungrouped mutate
    dplyr::mutate(!!!enquos(...))
  # if (length(setdiff(dot_nms2(...), names(data))) > 0L){
  #   out <- out %>%
  #     # Ungrouped mutate
  #     dplyr::mutate(!!!enquos(...))
  # }
  out <- out %>%
    dplyr::mutate(!!enquo(wt))
  wt_var <- tidy_transform_names(safe_ungroup(data),
                                 !!enquo(wt))
  if (length(wt_var) > 0L) wtv <- out[[wt_var]]
  group_info <- get_group_info(data, !!!enquos(...),
                               type = "data-mask",
                               .by = {{ .by }})
  group_vars <- group_info[["dplyr_groups"]]
  data_vars <- group_info[["extra_groups"]]
  all_vars <- group_info[["all_groups"]]
  grp_nm <- new_var_nm(names(out), ".group.id")
  out <- out %>%
    add_group_id(sort = TRUE,
             .by = all_of(all_vars),
             .overwrite = TRUE,
             as_qg = TRUE,
             .name = grp_nm) %>%
    dplyr::select(all_of(c(grp_nm, group_vars, data_vars))) %>%
    df_reconstruct(data)

  if (is.null(name)) name <- new_n_var_nm(out)
  group_id <- out[[grp_nm]]
  # Keep unique groups and sort
  if (nrow2(out) >= 2L) out <- collapse::funique.data.frame(out, cols = grp_nm, sort = TRUE)
  N <- nrow2(out)
  if (length(wt_var) == 0){
    nobs <- collapse::GRPN(group_id, expand = FALSE)
    out[[name]] <- nobs
    # Edge-case, not sure how to fix this
    if (N == 0L && length(all_vars) == 0L){
      N <- 1L
      out <- df_reconstruct(dplyr::tibble(!!name := 0L), data)
    }
  } else {
    # wtv <- dplyr::mutate(safe_ungroup(data), !!enquo(wt))[[wt_var]]
    nobs <- collapse::fsum(as.double(wtv),
                           g = group_id,
                           na.rm = TRUE,
                           use.g.names = FALSE,
                           fill = FALSE)
    if (all(collapse::na_rm(nobs) <= .Machine$integer.max)){
      nobs <- as.integer(nobs)
    }
    # Replace NA with 0
    out[[name]] <- data.table::nafill(nobs, type = "const",
                                      fill = 0, nan = NaN)
  }
  if (sort){
    out <- out[radix_order(out[[name]], decreasing = TRUE), , drop = FALSE]
  }
  # Set row.names attr
  out[[grp_nm]] <- NULL
  attr(out, "row.names") <- seq_len(N)
  out
}
#' @rdname fcount
#' @export
fadd_count <- function(data, ..., wt = NULL, sort = FALSE, name = NULL,
                       .by = NULL,
                       keep_class = TRUE){
  # Temporary fix as collapse grouping doesn't work on lubridate intervals
  if (any(purrr::map_lgl(data, lubridate::is.interval))){
    message("A variable of class 'interval' exists.
    The grouping will be done using 'dplyr' until the issue is fixed.
            You can report the issue at
            https://github.com/SebKrantz/collapse/issues")
    group_vars <- get_groups(data, {{ .by }})
    return(dplyr::add_count(data,
                        dplyr::across( all_of(group_vars) ),
                        !!!enquos(...),
                        wt = !!enquo(wt),
                        sort = sort, name = name))
  }
  template <- data[0, , drop = FALSE]
  out <- data %>%
    safe_ungroup() %>%
    dplyr::mutate(!!!enquos(...),
                  !!enquo(wt)) %>%
    data.table::copy()
  wt_var <- tidy_transform_names(safe_ungroup(data),
                                 !!enquo(wt))
  group_info <- get_group_info(data, !!!enquos(...),
                               type = "data-mask",
                               .by = {{ .by }})
  group_vars <- group_info[["dplyr_groups"]]
  data_vars <- group_info[["extra_groups"]]
  all_vars <- group_info[["all_groups"]]
  grp_nm <- new_var_nm(out, ".group.id")

  data.table::setDT(out)
  group_id <- group_id(out, .by = all_of(all_vars), sort = TRUE,
                   as_qg = TRUE)
  if (is.null(name)) name <- new_n_var_nm(out)

  nobs <- collapse::GRPN(group_id, expand = TRUE)
  if (length(wt_var) > 0L){
    out[, (name) := as.double(nobs) * get(wt_var)]
    if (all(collapse::na_rm(out[[name]]) <= .Machine$integer.max)){
      out[, (name) := as.integer(get(name))]
    }
    # Replace NA with 0
    data.table::setnafill(out, cols = name, type = "const", fill = 0, nan = NaN)
  } else {
   out[, (name) := nobs]
  }
  if (sort){
    data.table::setorderv(out, cols = name, na.last = TRUE, order = -1L)
  }
  if (keep_class){
    df_reconstruct(out, template)
    }
  else {
   out[]
  }
}
