#' Fast grouped statistical summary for data frames.
#'
#' @description
#' `collapse` and `data.table` are used for the calculations.
#'
#' @param data A data frame.
#' @param ... Variables to apply the statistical functions to.
#' Tidy data-masking applies.
#' @param stat A character vector of statistical summaries to apply.
#' This can be one or more of the following: \cr
#' "n", "nmiss", "min", "max", "mean", "first", "last", "sd",
#' "var", "mode", "median", "sum", "prop_complete".
#' @param q_probs (Optional) Quantile probabilities.
#' If supplied, `q_summarise()` is called and added to the result.
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
#' @param sort Should groups be sorted? Default is `TRUE`.
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
#'
#' @returns
#' A summary `data.table` containing the summary values for each group.
#'
#' @details
#' `stat_summarise()` can apply multiple functions to multiple variables.
#'
#' `stat_summarise()` is equivalent to \cr
#' `data %>% group_by(...) %>% summarise(across(..., list(...)))` \cr
#' but is faster and more efficient and accepts limited statistical functions.
#'
#' @seealso [q_summarise]
#'
#' @format `.stat_fns`
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' \dontshow{
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' stat_df <- iris %>%
#'   stat_summarise(Sepal.Length, .by = Species)
#' # Join quantile info too
#' q_df <- iris %>%
#'   q_summarise(Sepal.Length, .by = Species)
#' summary_df <- left_join(stat_df, q_df, by = "Species")
#' summary_df
#'
#' # Multiple cols
#' iris %>%
#'   group_by(Species) %>%
#'   stat_summarise(across(contains("Width")),
#'             stat = c("min", "max", "mean", "sd"))
#' @rdname stat_summarise
#' @export
stat_summarise <- function(data, ...,
                           stat = .stat_fns[1:5],
                           q_probs = NULL,
                           na.rm = TRUE, sort = TRUE,
                           .names = NULL, .by = NULL, .cols = NULL,
                           as_tbl = FALSE){
  inform_available_stats()
  funs <- .stat_fns
  if (!is.character(stat)){
    stop("stat must be a character vector")
  }
  if (length(setdiff(stat, funs)) > 0L){
    stop(paste0("stat must be one or more of the following:\n",
                paste(funs, collapse = ", ")))
  }
  stat_to_collapse_fun <- function(stat){
    get(paste0("f", stat))
    # get(paste0("f", stat),
    #     asNamespace("collapse"))
  }
  group_info <- group_info(data, ..., .by = {{ .by }},
                           .cols = .cols,
                           ungroup = TRUE,
                           rename = TRUE,
                           unique_groups = FALSE)
  group_vars <- group_info[["dplyr_groups"]]
  dot_vars <- group_info[["extra_groups"]]
  non_group_dot_vars <- setdiff(dot_vars, group_vars)
  data <- group_info[["data"]]
  g <- df_to_GRP(data, .cols = group_vars, order = sort)
  gstarts <- GRP_starts(g)
  # Distinct groups
  out <- df_row_slice(
    fselect(
      data, .cols = c(group_vars, non_group_dot_vars)
    ), gstarts
  )
  if (length(group_vars) == 0){
    g <- NULL
  }
  if (df_nrow(out) == 0L && length(group_vars) == 0L){
    out <- df_init(out, 1L)
  }
  out <- as_DT(out)
  if ("n" %in% stat){
    n_nm <- new_n_var_nm(names(out))
    data.table::set(out, j = n_nm,
                    value = fn(data, g = g))
    stat <- setdiff(stat, "n")
    data.table::setcolorder(out, c(group_vars, n_nm, non_group_dot_vars))
  }
  # New names
  var_nms <- across_col_names(.cols = dot_vars,
                              .fns = stat,
                              .names = NULL)
  # Output names
  out_nms <- across_col_names(.cols = dot_vars,
                              .fns = stat,
                              .names = .names)

  data.table::setalloccol(out,
                          n = getOption("datatable.alloccol", default = 1000L) +
                            (length(dot_vars) * length(stat)) +
                            (length(dot_vars) * length(q_probs)))
  k <- 0L
  for (.col in dot_vars){
    for (s in stat){
      k <- k + 1L
      data.table::set(out, j = var_nms[k],
                      value = stat_to_collapse_fun(s)(fpluck(data, .col),
                                                      g = g,
                                                      na.rm = na.rm,
                                                      use.g.names = FALSE))
    }
  }
  set_rm_cols(out, setdiff(non_group_dot_vars, var_nms))
  data.table::setnames(out,
                       old = var_nms,
                       new = out_nms)
  # Add quantiles if requested
  if (!is.null(q_probs)){
    q_summary <- q_summarise(data, across(all_of(dot_vars)),
                             probs = q_probs,
                             pivot = "wide",
                             sort = sort,
                             .by = all_of(group_vars),
                             na.rm = na.rm)
    add_cols <- setdiff(names(q_summary),
                        group_vars)
    data.table::set(out,
                    j = add_cols,
                    value = fselect(q_summary, .cols = add_cols))
  }
  if (as_tbl){
    out <- df_as_tibble(out)
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
.stat_fns <- c("n", "nmiss", "min", "max", "mean", "median",
               "sd", "var", "mode", "first", "last", "sum",
               "prop_complete")

inform_available_stats <- function(){
  rlang::inform(message = c("The below stat functions are available for use in stat_summarise",
                            .stat_fns),
                use_cli_format = TRUE,
                .frequency = "once",
                .frequency_id = "rlang_stat_inform")
}
