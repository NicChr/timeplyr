#' Fast grouped statistical summary for data frames.
#'
#' @description
#' `collapse` and `data.table` are used for the calculations.
#' @param data A data frame.
#' @param ... Variables to apply the statistical functions to.
#' Tidy data-masking applies.
#' @param stat A character vector of statistical summaries to apply.
#' This can be one or more of the following: \cr
#' "n", "nmiss", "min", "max", "mean", "first", "last", "sd",
#' "var", "mode", "median".
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
#' @return A summary `data.table` containing the summary values for each group.
#' @details
#'
#' `stat_summarise()` can apply multiple functions to multiple variables.
#'
#' `stat_summarise()` is equivalent to \cr
#' `data %>% group_by(...) %>% summarise(across(..., list(...)))` \cr
#' but is faster and more efficient and accepts limited statistical functions.
#' @seealso \link[timeplyr]{q_summary}
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' stat_df <- iris %>%
#'   stat_summarise(Sepal.Length, .by = Species)
#' # Join quantile info too
#' q_df <- iris %>%
#'   q_summary(Sepal.Length, .by = Species)
#' summary_df <- stat_df[q_df, on = "Species"]
#'
#' # Multiple cols
#' iris %>%
#'   group_by(Species) %>%
#'   stat_summarise(across(contains("Width")),
#'             stat = c("min", "max", "mean", "sd"))
#' @rdname stat_summarise
#' @export
stat_summarise <- function(data, ...,
                           stat = c("n", "nmiss", "min", "max", "mean"),
                           na.rm = TRUE, sort = TRUE,
                           .names = NULL, .by = NULL, .cols = NULL){
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
                           rename = TRUE)
  group_vars <- group_info[["dplyr_groups"]]
  dot_vars <- group_info[["extra_groups"]]
  data <- group_info[["data"]]
  if (length(group_vars) == 0L){
    g <- NULL
    gstarts <- min(1L, nrow2(data))
  } else {
    g <- GRP2(data, sort = sort, by = group_vars)
    gstarts <- GRP_starts(g)
  }
  # Distinct groups
  out <- df_row_slice(
    fselect(
      data, .cols = c(group_vars, dot_vars)
    ), gstarts
  )
  if (collapse::fnrow(out) == 0L && length(group_vars) == 0L){
    out[1L, ] <- NA
  }
  out <- as_DT(out)
  if ("n" %in% stat){
    n_nm <- new_n_var_nm(names(out))
    data.table::set(out, j = n_nm,
                    value = fn(data, g = g))
    stat <- setdiff(stat, "n")
    data.table::setcolorder(out, c(group_vars, n_nm, dot_vars))
  }
  # New names
  var_nms <- across_col_names(.cols = dot_vars,
                              .fns = stat,
                              .names = NULL)
  # Output names
  out_nms <- across_col_names(.cols = dot_vars,
                              .fns = stat,
                              .names = .names)

  k <- 0L
  for (.col in dot_vars){
    for (s in stat){
      k <- k + 1L
      data.table::set(out, j = var_nms[k],
                      value = stat_to_collapse_fun(s)(data[[.col]],
                                                      g = g,
                                                      na.rm = na.rm,
                                                      use.g.names = FALSE))
    }
  }
  set_rm_cols(out, setdiff(dot_vars, var_nms))
  data.table::setnames(out,
                       old = var_nms,
                       new = out_nms)
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
        setv(out, seq_along(.fns) + init,
             stringr::str_c(.col, "_", .fn),
             vind1 = TRUE)
        init <- init + length(.fns)
      }
    } else {
      for (.col in .cols){
        setv(out, seq_along(.fns) + init,
             stringr::str_glue(.names),
             vind1 = TRUE)
        init <- init + length(.fns)
      }
    }
  }
  if (anyDuplicated(out) > 0){
    stop("Column names must be unique")
  }
  out
}
#' @export
.stat_fns <- c("n", "nmiss", "min", "max", "mean", "median",
               "sd", "var", "mode", "first", "last")
