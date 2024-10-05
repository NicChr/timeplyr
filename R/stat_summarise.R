#' These functions have been superseded by fastplyr functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `r lifecycle::badge("superseded")`
#'
#' These functions can now be found in fastplyr. \cr
#' They are no longer recommended in this package and thus have been both
#' deprecated and superseded.
#'
#' @param data A data frame.
#' @param ... Variables to apply the statistical functions to.
#' Tidy data-masking applies.
#' @param stat A character vector of statistical summaries to apply.
#' This can be one or more of the following: \cr
#' "n", "nmiss", "ndistinct", "min", "max", "mean", "first", "last", "sd",
#' "var", "mode", "median", "sum", "prop_complete".
#' @param q_probs (Optional) Quantile probabilities.
#' If supplied, `q_summarise()` is called and added to the result.
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
#' @param sort Should groups be sorted? Default is `TRUE`.
#' @param .count_name Name of count column, default is "n".
#' @param .names An optional glue specification passed to `stringr::glue()`.
#' If `.names = NULL`, then when there is 1 variable, the function name
#' is used, i.e `.names = "{.fn}"`, when there are multiple variables and
#' 1 function, the variable names are used, i.e, `.names = "{.col}"`
#' and in the case of multiple variables and functions.
#' `"{.col}_{.fn}"` is used.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param as_tbl Should the result be a `tibble`? Default is `FALSE`.
#' @param inform_stats Should available stat functions be displayed
#' at the start of each session? Default is `TRUE`.
#'
#' @returns
#' A summary `data.table` containing the summary values for each group.
#'
#' @rdname stat_summarise
#' @export
stat_summarise <- function(data, ...,
                           stat =.stat_fns[1:3],
                           q_probs = NULL,
                           na.rm = TRUE, sort = df_group_by_order_default(data),
                           .count_name = NULL,
                           .names = NULL, .by = NULL, .cols = NULL,
                           inform_stats = TRUE,
                           as_tbl = FALSE){
  lifecycle::deprecate_warn(
    when = "0.9.0",
    what = "stat_summarise()",
    with = "fastplyr::f_summarise()"
  )
  # if (inform_stats){
  #   inform_available_stats()
  # }
  funs <- .stat_fns
  if (!is.character(stat)){
    stop("stat must be a character vector")
  }
  if (length(setdiff2(stat, funs)) > 0L){
    stop(paste0("stat must be one or more of the following:\n",
                paste(funs, collapse = ", ")))
  }
  stat_to_collapse_fun <- function(stat){
    get(paste0("f", stat))
    # get(paste0("f", stat),
    #     asNamespace("collapse"))
  }
  group_info <- tidy_group_info(data, ..., .by = {{ .by }},
                                .cols = .cols,
                                ungroup = TRUE,
                                rename = TRUE,
                                unique_groups = FALSE)
  group_vars <- group_info[["dplyr_groups"]]
  dot_vars <- group_info[["extra_groups"]]
  non_group_dot_vars <- setdiff(dot_vars, group_vars)
  staging <- group_info[["data"]]
  g <- df_to_GRP(staging, .cols = group_vars, order = sort)
  gstarts <- GRP_starts(g)
  # Distinct groups
  out <- df_row_slice(
    fastplyr::f_select(
      staging, .cols = c(group_vars, non_group_dot_vars)
    ), gstarts
  )
  if (length(group_vars) == 0){
    g <- NULL
  }
  if (df_nrow(out) == 0L && length(group_vars) == 0L){
    out <- df_init(out, 1L)
  }
  out <- df_as_dt(out, .copy = FALSE)
  n_nm <- character()
  if ("n" %in% stat){
    if (is.null(.count_name)){
      n_nm <- unique_count_col(names(out))
    } else {
      n_nm <- .count_name
    }
    set_add_cols(out, add_names(list(fn(staging, g = g)), n_nm))
    data.table::setcolorder(out, c(group_vars, n_nm, non_group_dot_vars))
  }
  stat <- setdiff2(stat, "n")
  # New names
  var_nms <- across_col_names(.cols = dot_vars,
                              .fns = stat,
                              .names = NULL)
  # Output names
  out_nms <- across_col_names(.cols = dot_vars,
                              .fns = stat,
                              .names = .names)
  # Try to make sure group variables are kept
  # which_are_groups <- which(dot_vars %in% group_vars)
  # var_nms[which_are_groups] <- out_nms[which_are_groups]

  data.table::setalloccol(out,
                          n = getOption("datatable.alloccol", default = 1000L) +
                            (length(dot_vars) * length(stat)) +
                            (length(dot_vars) * length(q_probs)))
  k <- 0L
  for (.col in dot_vars){
    for (s in stat){
      k <- k + 1L
      data.table::set(out, j = var_nms[k],
                      value = stat_to_collapse_fun(s)(fpluck(staging, .col),
                                                      g = g,
                                                      na.rm = na.rm,
                                                      use.g.names = FALSE))
    }
  }
  set_rm_cols(out, setdiff(non_group_dot_vars, var_nms))
  not_groups <- match(var_nms, group_vars, 0L) == 0L
  output_nms <- c(group_vars, n_nm, add_names(var_nms[not_groups], out_nms[not_groups]))
  out <- fastplyr::f_select(out, .cols = output_nms)
  # Add quantiles if requested
  if (!is.null(q_probs)){
    q_summary <- q_summarise(staging, across(all_of(dot_vars)),
                             probs = q_probs,
                             pivot = "wide",
                             sort = sort,
                             .by = all_of(group_vars),
                             na.rm = na.rm)
    if (df_nrow(q_summary) == 0 && df_nrow(out) > 0){
      q_summary <- df_init(q_summary, 1L)
    }
    add_cols <- setdiff(names(q_summary),
                        group_vars)
    if (length(add_cols) > 0){
      set_add_cols(out, fastplyr::f_select(q_summary, .cols = add_cols))
    }
  }
  if (as_tbl){
    out <- df_as_tbl(out)
  }
  out
}

# Recreate column names from the .names arg of dplyr::across()
across_col_names <- function(.cols = NULL, .fns = NULL,
                             .names = NULL){
  nms_null <- is.null(.names)
  if (nms_null && length(.cols) == 1L){
    out <- .fns
  } else if (nms_null && length(.fns) == 1L){
    out <- .cols
    } else {
    out <- character(length(.cols) * length(.fns))
    init <- 0L
    .fn <- .fns
    if (nms_null){
      for (.col in .cols){
        out[seq_along(.fns) + init] <- stringr::str_c(.col, "_", .fn)
        init <- init + length(.fns)
      }
    } else {
      for (.col in .cols){
        out[seq_along(.fns) + init] <- stringr::str_glue(.names)
        init <- init + length(.fns)
      }
    }
  }
  if (anyDuplicated(out) > 0){
    stop("Column names must be unique")
  }
  out
}
#' @rdname stat_summarise
#' @export
.stat_fns <- c("n", "nmiss", "ndistinct", "min", "max", "mean", "median",
               "sd", "var", "mode", "first", "last", "sum",
               "prop_complete")

inform_available_stats <- function(){
  rlang::inform(message = c("The below stat functions are available for use in stat_summarise",
                            .stat_fns),
                use_cli_format = TRUE,
                .frequency = "once",
                .frequency_id = "rlang_stat_inform")
}
