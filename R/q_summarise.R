#' Fast grouped quantile summary
#'
#' @description
#' `collapse` and `data.table` are used for the calculations.
#' @param data A data frame.
#' @param ... Variables used to calculate quantiles for.
#' Tidy data-masking applies.
#' @param probs Quantile probabilities.
#' @param type An integer from 5-9 specifying which algorithm to use.
#' See \code{\link{quantile}} for more details.
#' @param pivot Should data be pivoted wide or long? Default is `wide`.
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
#' @param sort Should groups be sorted? Default is `TRUE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .cols (Optional) alternative to `...` that accepts
#' a named character vector or numeric vector.
#' If speed is an expensive resource, it is recommended to use this.
#' @return A `data.table` containing the quantile values for each group.
#' @seealso \link[timeplyr]{stat_summarise}
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' # Standard quantiles
#' iris %>%
#'   q_summarise(Sepal.Length)
#' # Quantiles by species
#' iris %>%
#'   q_summarise(Sepal.Length, .by = Species)
#' # Quantiles by species across multiple columns
#' iris %>%
#'   q_summarise(Sepal.Length, Sepal.Width,
#'             probs = c(0, 1),
#'             .by = Species)
#' # Long format if one desires, useful for ggplot2
#' iris %>%
#'   q_summarise(Sepal.Length, pivot = "long",
#'             .by = Species)
#' # Example with lots of groups
#' set.seed(20230606)
#' df <- data.frame(x = rnorm(10^5),
#'                  g = sample.int(10^5, replace = TRUE))
#' q_summarise(df, x, .by = g, sort = FALSE)
#' @export
q_summarise <- function(data, ...,
                        probs = seq(0, 1, 0.25),
                        type = 7,
                        pivot = c("wide", "long"),
                        na.rm = TRUE, sort = TRUE,
                        .by = NULL, .cols = NULL){
  pivot <- rlang::arg_match0(pivot, c("wide", "long"))
  wide <- pivot == "wide"
  group_info <- group_info(data, ..., .by = {{ .by }},
                           .cols = .cols,
                           ungroup = TRUE,
                           rename = TRUE,
                           unique_groups = FALSE)
  group_vars <- group_info[["dplyr_groups"]]
  dot_vars <- group_info[["extra_groups"]]
  non_group_dot_vars <- setdiff(dot_vars, group_vars)
  if (length(dot_vars) == 0L){
    stop("Please supply at least 1 non-group variable to ...")
  }
  out <- group_info[["data"]]
  # Dealing with quantile names
  quantile_probs <- probs
  quantile_ties <- paste0("q", type)
  q_prcnts <- quantile_probs * 100
  quantile_nms <- paste0(rep_len("p", length(quantile_probs)), q_prcnts)
  quantile_out_nms <- factor(quantile_nms, levels = quantile_nms)
  # Turn data frame into GRP object
  groups <- df_to_GRP(out, .cols = group_vars, order = sort)
  n_groups <- GRP_n_groups(groups)
  group_starts <- GRP_starts(groups)
  # Select necessary columns
  out <- fselect(
    out, .cols = c(group_vars, non_group_dot_vars)
  )
  out <- as_DT(out)
  group_id_nm <- new_var_nm(out, "group_id")
  out[, (group_id_nm) := GRP_group_id(groups)]
  # When there's no groups, collapse likes a NULL g object (usually)
  if (length(group_vars) == 0){
    groups <- NULL
  }
  # Distinct groups
  q_df <- df_row_slice(out, group_starts)
  # If nrow is 0 or no probs
  if (nrow2(data) == 0L || length(probs) == 0L){
    if (wide){
      q_df <- matrix(integer(0), ncol = length(quantile_out_nms), nrow = 0)
      colnames(q_df) <- quantile_nms
      q_df <- data.table::as.data.table(q_df)
    } else {
      q_df <- data.table::data.table(.quantile = quantile_out_nms)
      data.table::set(q_df, j = dot_vars, value = NA_real_)
    }
  } else {
    # Lookup table using group ID as key
    group_lookup <- fselect(q_df, .cols = c(group_id_nm, group_vars))
    # Faster to just use this when there are small numbers of groups
    if (n_groups < 1e05){
      q_df <- out[, lapply(.SD, function(x) collapse::fquantile(x,
                                                                probs = probs,
                                                                type = type,
                                                                names = FALSE,
                                                                na.rm = na.rm,
                                                                check.o = FALSE
      )),
      keyby = group_id_nm,
      .SDcols = dot_vars]
      data.table::set(q_df,
                      j = ".quantile",
                      value = rep(quantile_out_nms, times = nrow2(q_df) / length(probs)))
      if (length(group_vars) > 0L){
        data.table::set(q_df,
                        j = group_vars,
                        value = df_rep_each(fselect(group_lookup,
                                                    .cols = group_vars),
                                            length(probs)))
      }
    } else {
      # Allocate sufficient column space
      # Have to do this because of a data.table bug with set()
      q_df <- df_rep(q_df, length(probs))
      num_cols_to_allocate <- getOption("datatable.alloccol", default = 1000L) +
        (length(dot_vars) * length(probs))
      data.table::setalloccol(q_df, n = num_cols_to_allocate)
      # Add quantile names
      data.table::set(q_df,
                      j = (".quantile"),
                      value = rep(quantile_out_nms,
                                  each = nrow2(q_df) / length(probs)))
      # Coerce variables to numeric for safety
      q_df[, (dot_vars) := lapply(.SD, as.double),
           .SDcols = dot_vars]
      # Use collapse::nth() instead if there are many groups
      for (.col in dot_vars){
        k <- 0L
        probi <- 0L
        for (p in probs){
          p_seq <- seq_len(n_groups) + probi
          probi <- probi + n_groups
          k <- k + 1L
          if (p == 0){
            data.table::set(q_df,
                            i = p_seq,
                            j = .col,
                            value = collapse::fmin(out[[.col]],
                                                   g = groups,
                                                   na.rm = na.rm,
                                                   use.g.names = FALSE))
          }
          if (p == 1){
            data.table::set(q_df,
                            i = p_seq,
                            j = .col,
                            value = collapse::fmax(out[[.col]],
                                                   g = groups,
                                                   na.rm = na.rm,
                                                   use.g.names = FALSE))
          }
          if (p > 0 & p < 1){
            data.table::set(q_df,
                            i = p_seq,
                            j = .col,
                            value = collapse::fnth(out[[.col]],
                                                   n = p,
                                                   g = groups,
                                                   na.rm = na.rm,
                                                   use.g.names = FALSE,
                                                   ties = quantile_ties))
          }
        }
      }
    }
    # if wide is true then pivot wider
    if (wide){
      cast_formula <- stats::as.formula(paste0(group_id_nm, " ~ .quantile"))
      q_df <- data.table::dcast(q_df,
                                formula = cast_formula,
                                value.var = dot_vars)
      if (length(group_vars) > 0L){
        q_df[, (group_vars) := fselect(group_lookup, .cols =
                                         setdiff(names(group_lookup),
                                                 group_id_nm))]
      }
      out_nms <- c(group_vars,
                   setdiff(names(q_df),
                           c(group_vars, ".", group_id_nm)))
    } else {
      if (isTRUE(is.null(data.table::key(q_df)) ||
                  any(data.table::key(q_df) != group_id_nm))){
        q_df <- farrange(q_df, .cols = group_id_nm)
      }
      out_nms <- c(group_vars,
                   setdiff(names(q_df),
                           c(group_vars,
                             dot_vars, group_id_nm)),
                   dot_vars)
    }
    # Remove temporary group ID
    data.table::set(q_df,
                    j = group_id_nm,
                    value = NULL)
    q_df <- fselect(q_df, .cols = out_nms)
  }
  q_df[]
}
# q_summarise_old <- function(data, ...,
#                             probs = seq(0, 1, 0.25),
#                             type = 7L,
#                             pivot = c("wide", "long"),
#                             na.rm = TRUE, sort = TRUE,
#                             .by = NULL, .cols = NULL){
#   pivot <- rlang::arg_match0(pivot, c("wide", "long"))
#   wide <- pivot == "wide"
#   group_info <- group_info(data, ..., .by = {{ .by }},
#                            .cols = .cols,
#                            ungroup = TRUE,
#                            rename = TRUE,
#                            unique_groups = FALSE)
#   group_vars <- group_info[["dplyr_groups"]]
#   dot_vars <- group_info[["extra_groups"]]
#   non_group_dot_vars <- setdiff(dot_vars, group_vars)
#   if (length(dot_vars) == 0L){
#     stop("Please supply at least 1 non-group variable to ...")
#   }
#   out <- group_info[["data"]]
#   q_prcnts <- probs * 100
#   quantile_nms <- paste0(rep_len("p", length(probs)), q_prcnts)
#   quantile_nms <- factor(quantile_nms, levels = quantile_nms)
#   # Group ID
#   g <- group_id(out, .cols = group_vars, order = sort)
#   grp_nm <- new_var_nm(out, ".group.id")
#   # Add group ID to df
#   out[[grp_nm]] <- g
#   out <- as_DT(out)
#   # Select necessary cols
#   out <- fselect(out, .cols = c(grp_nm, group_vars, dot_vars))
#   if (nrow2(out) == 0L || length(probs) == 0L){
#     if (wide){
#       q_df <- matrix(integer(0), ncol = length(quantile_nms), nrow = 0)
#       colnames(q_df) <- quantile_nms
#       q_df <- data.table::as.data.table(q_df)
#     } else {
#       q_df <- data.table::data.table(.quantile = quantile_nms)
#       data.table::set(q_df, j = dot_vars, value = NA_real_)
#     }
#   } else {
#     # out <- farrange(out, .cols = c(grp_nm, dot_vars))
#     # Feed (by-group) order vector
#     # out[[".qorder"]] <- data.table::rowidv(out[[grp_nm]])
#     # Lookup table using group ID as key
#     grp_df <- collapse::funique(fselect(out, .cols = c(grp_nm, group_vars)),
#                                 cols = grp_nm, sort = sort)
#     # Quantile calculation
#     # data.table method
#     q_df <- out[, lapply(.SD, function(x) collapse::fquantile(x,
#                                                               probs = probs,
#                                                               names = FALSE,
#                                                               na.rm = na.rm,
#                                                               check.o = FALSE
#                                                               # o = get(".qorder"),
#                                                               # check.o = TRUE
#                                                               )),
#                 keyby = grp_nm,
#                 .SDcols = dot_vars]
#     # collapse method
#     # out <- collapse::fgroup_by(out, grp_nm, sort = sort)
#     # q_df <- collapse::fsummarise(
#     #   out,
#     #   across(.cols = dot_vars,
#     #          list(function(x)
#     #            collapse::.quantile(x, probs = probs,
#     #                                names = FALSE,
#     #                                type = type,
#     #                                na.rm = na.rm)))
#     # )
#     # Add quantile names as variable
#     data.table::set(q_df,
#                     j = ".quantile",
#                     value = rep(quantile_nms, times = nrow2(q_df) / length(probs)))
#     cast_formula <- stats::as.formula(paste0(grp_nm, " ~ .quantile"))
#     # if wide is true then pivot wider
#     if (wide){
#       q_df <- data.table::dcast(q_df,
#                                 formula = cast_formula,
#                                 value.var = dot_vars)
#       # q_df <- q_df[grp_df]
#       if (length(group_vars) > 0L){
#         # Bind group variables
#         q_df[, (group_vars) := fselect(grp_df, .cols = setdiff(names(grp_df), grp_nm))]
#       }
#       out_nms <- c(group_vars,
#                    setdiff(names(q_df),
#                            group_vars))
#     } else {
#       # Join group variables
#       if (length(group_vars) > 0L){
#         q_df[, (group_vars) := fselect(grp_df, .cols = setdiff(names(grp_df), grp_nm))[
#                                         rep(seq_len(.N),
#                                             each = length(quantile_nms))
#                                       ]]
#       }
#       out_nms <- c(group_vars,
#                    setdiff(names(q_df),
#                            c(group_vars,
#                              dot_vars)),
#                    dot_vars)
#     }
#     data.table::setcolorder(q_df, neworder = out_nms)
#     # Remove group ID variable
#     data.table::set(q_df, j = grp_nm, value = NULL)
#   }
#   q_df[]
# }

