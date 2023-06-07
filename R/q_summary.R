#' Fast grouped quantiles.
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
#'   q_summary(Sepal.Length)
#' # Quantiles by species
#' iris %>%
#'   q_summary(Sepal.Length, .by = Species)
#' # Quantiles by species across multiple columns
#' iris %>%
#'   q_summary(Sepal.Length, Sepal.Width,
#'             probs = c(0, 1),
#'             .by = Species)
#' # Long format if one desires, useful for ggplot2
#' iris %>%
#'   q_summary(Sepal.Length, pivot = "long",
#'             .by = Species)
#' # Example with lots of groups
#' set.seed(20230606)
#' df <- data.frame(x = rnorm(10^5),
#'                  g = sample.int(10^5, replace = TRUE))
#' q_summary(df, x, .by = g, sort = FALSE)
#' @export
q_summary <- function(data, ...,
                      probs = seq(0, 1, 0.25),
                      type = 7L,
                      pivot = c("wide", "long"),
                      na.rm = TRUE, sort = TRUE,
                      .by = NULL, .cols = NULL){
  pivot <- rlang::arg_match0(pivot, c("wide", "long"))
  wide <- pivot == "wide"
  group_info <- group_info(data, ..., .by = {{ .by }},
                           .cols = .cols,
                           ungroup = TRUE,
                           rename = TRUE)
  group_vars <- group_info[["dplyr_groups"]]
  dot_vars <- group_info[["extra_groups"]]
  if (length(dot_vars) == 0L){
    stop("Please supply at least 1 non-group variable to ...")
  }
  out <- group_info[["data"]]
  q_prcnts <- probs * 100
  quantile_nms <- paste0(rep_len("p", length(probs)), q_prcnts)
  quantile_nms <- factor(quantile_nms, levels = quantile_nms)
  # Group ID
  g <- group_id(out, .cols = group_vars, order = sort)
  grp_nm <- new_var_nm(out, ".group.id")
  # Add group ID to df
  out[[grp_nm]] <- g
  out <- as_DT(out)
  # Select necessary cols
  out <- fselect(out, .cols = c(grp_nm, group_vars, dot_vars))
  if (nrow2(out) == 0L || length(probs) == 0L){
    if (wide){
      q_df <- matrix(integer(0), ncol = length(quantile_nms), nrow = 0)
      colnames(q_df) <- quantile_nms
      q_df <- data.table::as.data.table(q_df)
    } else {
      q_df <- data.table::data.table(.quantile = quantile_nms)
    }
  } else {
    # out <- farrange(out, .cols = c(grp_nm, dot_vars))
    # Feed (by-group) order vector
    # out[[".qorder"]] <- data.table::rowidv(out[[grp_nm]])
    # Lookup table using group ID as key
    grp_df <- collapse::funique(fselect(out, .cols = c(grp_nm, group_vars)),
                                cols = grp_nm, sort = sort)
    # Quantile calculation
    # data.table method
    q_df <- out[, lapply(.SD, function(x) collapse::fquantile(x,
                                                              probs = probs,
                                                              names = FALSE,
                                                              na.rm = na.rm
                                                              # o = get(".qorder"),
                                                              # check.o = TRUE
                                                              )),
                keyby = grp_nm,
                .SDcols = dot_vars]
    # collapse method
    # out <- collapse::fgroup_by(out, grp_nm, sort = sort)
    # q_df <- collapse::fsummarise(
    #   out,
    #   across(.cols = dot_vars,
    #          list(function(x)
    #            collapse::.quantile(x, probs = probs,
    #                                names = FALSE,
    #                                type = type,
    #                                na.rm = na.rm)))
    # )
    # Add quantile names as variable
    data.table::set(q_df,
                    j = ".quantile",
                    value = rep(quantile_nms, times = nrow2(q_df) / length(probs)))
    cast_formula <- stats::as.formula(paste0(grp_nm, " ~ .quantile"))
    # if wide is true then pivot wider
    if (wide){
      q_df <- data.table::dcast(q_df,
                                formula = cast_formula,
                                value.var = dot_vars)
      # q_df <- q_df[grp_df]
      if (length(group_vars) > 0L){
        # Bind group variables
        q_df[, (group_vars) := fselect(grp_df, .cols = setdiff(names(grp_df), grp_nm))]
      }
      out_nms <- c(group_vars,
                   setdiff(names(q_df),
                           group_vars))
    } else {
      # Join group variables
      if (length(group_vars) > 0L){
        q_df[, (group_vars) := fselect(grp_df, .cols = setdiff(names(grp_df), grp_nm))[
                                        rep(seq_len(.N),
                                            each = length(quantile_nms))
                                      ]]
      }
      out_nms <- c(group_vars,
                   setdiff(names(q_df),
                           c(group_vars,
                             dot_vars)),
                   dot_vars)
    }
    data.table::setcolorder(q_df, neworder = out_nms)
    # Remove group ID variable
    data.table::set(q_df, j = grp_nm, value = NULL)
  }
  q_df[]
}
