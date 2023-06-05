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
#' iris %>%
#'   q_summary(Sepal.Length, .by = Species)
#' @export
q_summary <- function(data, ...,
                      probs = seq(0, 1, 0.25),
                      type = 7L,
                      pivot = c("wide", "long"),
                      na.rm = TRUE, sort = FALSE,
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
  q_prcnts <- round(probs * 100)
  quantile_nms <- paste0(rep_len("p", length(probs)), q_prcnts)
  # Group ID
  g <- group_id(out, .cols = group_vars, order = sort)
  grp_nm <- new_var_nm(out, ".group.id")
  # Add group ID to df
  out[[grp_nm]] <- g
  out <- as_DT(out)
  if (nrow2(out) == 0L || length(probs) == 0L){
    if (wide){
      q_df <- matrix(integer(0), ncol = length(quantile_nms), nrow = 0)
      colnames(q_df) <- quantile_nms
      q_df <- data.table::as.data.table(q_df)
    } else {
      q_df <- data.table::data.table(q_nms = quantile_nms)
    }
  } else {
    # Lookup table using group ID as key
    grp_df <- collapse::funique(fselect(out, .cols = c(grp_nm, group_vars)),
                                cols = grp_nm, sort = sort)
    # Quantile calculation
    # Alternate method using data.table
    q_df <- out[, lapply(.SD, function(x) collapse::.quantile(x,
                                                              probs = probs)),
                by = grp_nm,
                .SDcols = dot_vars]
    if (sort){
      data.table::setkeyv(q_df, grp_nm)
    }
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
                    j = "q_nms",
                    value = rep(quantile_nms, times = nrow2(q_df) / length(probs)))
    cast_formula <- as.formula(paste0(grp_nm, " ~ q_nms"))
    # if wide is true then pivot wider
    if (wide){
      q_df[, ("q_nms") := factor(get("q_nms"),
                                 levels = quantile_nms)]
      q_df <- data.table::dcast(q_df,
                                formula = cast_formula,
                                value.var = dot_vars)
      # q_df <- q_df[grp_df]
      if (length(group_vars) > 0L){
        # Bind group variables
        q_df[, (group_vars) := grp_df[, setdiff(names(grp_df), grp_nm),
                                      with = FALSE]]
      }
      data.table::setcolorder(q_df, neworder = c(group_vars,
                                                 setdiff(names(q_df),
                                                         group_vars)))
    } else {
      # Join group variables
      if (length(group_vars) > 0L){
        q_df[grp_df, (group_vars) := mget(group_vars),
             on = grp_nm, allow.cartesian = FALSE]
      }
      data.table::setcolorder(q_df, neworder = c(group_vars,
                                                 setdiff(names(q_df),
                                                         c(group_vars,
                                                           dot_vars)),
                                                 dot_vars))
    }
    # Remove group ID variable
    set_rm_cols(q_df, grp_nm)
  }
  q_df[]
}
