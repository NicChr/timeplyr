#' Fast statistical summary for data frames.
#'
#' @description
#' `collapse` and `data.table` are used for the calculations.
#'
#' @param data A data frame.
#' @param ... Variables used to calculate the min/max values.
#' Tidy data-masking applies.
#' @param stat A character vector of statistical summaries to apply.
#' This can be one or more of the following: \cr
#' "min", "max", "mean", "first", "last", "sd", "var", "mode", "median", "nobs".
#' @param .names See `?dplyr::across` for more details.
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param sort Should groups be sorted? Default is `TRUE`.
#' @param keep_class Logical. If `TRUE` then the class of
#' the input data is retained.
#' If `FALSE`, which is sometimes faster, a `data.table` is returned.
#' @return A summary data frame containing the summary values for each group.
#' @details
#'
#' `stat_summarise()` can apply multiple functions to multiple variables.
#'
#' `stat_summarise()` is equivalent to \cr
#' `data %>% group_by(...) %>% summarise(across(..., list(...)))` \cr
#' but is faster and more efficient and accepts limited statistical functions.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' iris %>%
#'   stat_summarise(Sepal.Length, Sepal.Width,
#'                  stat = c("min", "max", "mean"),
#'                  .by = Species)
#' @rdname stat_summarise
#' @export
stat_summarise <- function(data, ..., stat = "mean",
                           .names = NULL,
                           na.rm = TRUE, .by = NULL, .cols = NULL,
                           sort = TRUE,
                           keep_class = TRUE){
  funs <- c("min", "max", "mean", "first", "last", "sd", "var", "mode",
            "median", "nobs")
  if (!is.character(stat)){
    stop("stat must be a character vector")
  }
  if (length(setdiff(stat, funs)) > 0L){
    stop(paste0("stat must be one or more of the following:\n",
                paste(funs, collapse = ", ")))
  }
  stat_to_collapse_fun <- function(stat){
    get(paste0("f", stat),
        asNamespace("collapse"))
  }
  group_info <- group_info(data, ..., .by = {{ .by }},
                           .cols = .cols,
                           ungroup = TRUE,
                           rename = TRUE)
  group_vars <- group_info[["dplyr_groups"]]
  dot_vars <- group_info[["extra_groups"]]
  template <- vec_head(data)
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
  out <- list_to_DT(out)
  var_nms <- across_col_names(.cols = dot_vars,
                              .fns = stat,
                              .names = NULL)
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
  data.table::setnames(out,
                       old = var_nms,
                       new = out_nms)
  set_rm_cols(out, setdiff(dot_vars, out_nms))
  if (keep_class){
    df_reconstruct(out, template)
  } else {
    out
  }
}
# Recreate column names from the .names arg of dplyr::across()
across_col_names <- function(.cols = NULL, .fns = NULL,
                             .names = NULL){
  nms_null <- is.null(.names)
  if (length(.fns) == 1L && nms_null){
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
