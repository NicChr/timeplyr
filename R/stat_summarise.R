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
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @param sort Should groups be sorted? Default is `TRUE`.
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
                           na.rm = TRUE, .by = NULL, .cols = NULL,
                           sort = TRUE){
  funs <- c("min", "max", "mean", "first", "last", "sd", "var", "mode",
            "median", "nobs")
  if (!is.character(stat)){
    stop("stat must be a character vector")
  }
  if (length(setdiff(stat, funs)) > 0L){
    stop(paste0("stat be one or more of the following:\n",
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
  for (i in seq_along(dot_vars)){
    for (s in stat){
      data.table::set(out, j = paste0(dot_vars[i], "_", s),
                      value = stat_to_collapse_fun(s)(data[[dot_vars[i]]],
                                                      g = g,
                                                      na.rm = na.rm,
                                                      use.g.names = FALSE))
    }
  }
  set_rm_cols(out, dot_vars)
  df_reconstruct(out, template)
}
# min_max_summary <- function(data, ..., na.rm = TRUE,
#                     .by = NULL, .cols = NULL){
#   group_info <- group_info(data, ..., .by = {{ .by }},
#                            .cols = .cols,
#                            ungroup = TRUE,
#                            rename = TRUE)
#   group_vars <- group_info[["dplyr_groups"]]
#   dot_vars <- group_info[["extra_groups"]]
#   template <- vec_head(data)
#   data <- group_info[["data"]]
#   if (length(group_vars) == 0L){
#     g <- NULL
#     gstarts <- min(1L, nrow2(data))
#   } else {
#     g <- GRP2(data, sort = TRUE, by = group_vars)
#     gstarts <- GRP_starts(g)
#   }
#   # Distinct groups
#   out <- df_row_slice(
#     fselect(
#       data, .cols = c(group_vars, dot_vars)
#     ), gstarts
#   )
#   out <- list_to_DT(out)
#   min_vars <- rep_len("_min", length(dot_vars))
#   max_vars <- rep_len("_max", length(dot_vars))
#   min_vars <- paste0(dot_vars, min_vars)
#   max_vars <- paste0(dot_vars, max_vars)
#   for (i in seq_along(dot_vars)){
#     data.table::set(out, j = min_vars[i],
#                     value = collapse::fmin(data[[dot_vars[i]]],
#                                            g = g,
#                                            na.rm = na.rm,
#                                            use.g.names = FALSE))
#     data.table::set(out, j = max_vars[i],
#                     value = collapse::fmax(data[[dot_vars[i]]],
#                                            g = g,
#                                            na.rm = na.rm,
#                                            use.g.names = FALSE))
#   }
#   set_rm_cols(out, dot_vars)
#   df_reconstruct(out, template)
# }
# add_min_max <- function(data, ..., na.rm = TRUE,
#                     .by = NULL, .cols = NULL){
#   group_info <- group_info(data, ..., .by = {{ .by }},
#                            .cols = .cols,
#                            ungroup = TRUE,
#                            rename = TRUE)
#   group_vars <- group_info[["dplyr_groups"]]
#   dot_vars <- group_info[["extra_groups"]]
#   template <- vec_head(data)
#   data <- group_info[["data"]]
#   if (length(group_vars) == 0L){
#     g <- NULL
#   } else {
#     g <- GRP2(data, sort = TRUE, by = group_vars)
#   }
#   # Distinct groups
#   out <- list_to_DT(data)
#   min_vars <- rep_len("_min", length(dot_vars))
#   max_vars <- rep_len("_max", length(dot_vars))
#   min_vars <- paste0(dot_vars, min_vars)
#   max_vars <- paste0(dot_vars, max_vars)
#   for (i in seq_along(dot_vars)){
#     data.table::set(out, j = min_vars[i],
#                     value = gmin(data[[dot_vars[i]]],
#                                  g = g,
#                                  na.rm = na.rm))
#     data.table::set(out, j = max_vars[i],
#                     value = gmax(data[[dot_vars[i]]],
#                                  g = g,
#                                  na.rm = na.rm))
#   }
#   df_reconstruct(out, template)
# }
