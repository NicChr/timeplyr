#' Fast alternative to `skimr::skim()`
#'
#' @description Inspired by the brilliant `skimr` package, this is a fast
#' alternative that provides an un-grouped data frame summary.
#'
#' @param data A data frame.
#' @param hist Logical. If `TRUE`, histogram spark graphs are produced
#' in the numeric summary.
#' @return A list of length 7 with the elements:
#' * `nrow` - Number of rows
#' * `ncol` - Number of columns
#' * `logical` - A `tibble` summary of the logical columns.
#' * `numeric` - A `tibble` summary of the numeric columns.
#' * `date` - A `tibble` summary of the date columns.
#' * `datetime` - A `tibble` summary of the datetime columns.
#' * `categorical` - A `tibble` summary of the categorical columns.
#'
#' @details `collapse` is used to compute the summary statistics and
#' `data.table` is used to wrangle the data frames. \cr
#' Character vectors are internally converted to factors using
#' `collapse::qF()`.
#'
#' @examples
#' library(timeplyr)
#' library(nycflights13)
#' fskim(flights)
#' @export
fskim <- function(data, hist = FALSE){
  N <- nrow2(data)
  data <- as_DT(safe_ungroup(data))
  data_nms <- names(data)
  col_classes <- vapply(data, function(x) vec_tail(class(x)), character(1))
  out <- as_DT(fenframe(col_classes,
                        name = "col",
                        value = "class"))
  out <- data.table::copy(out)
  data.table::setalloccol(out, n = 1000)
  chr_vars <- data_nms[vapply(data, is.character, logical(1), USE.NAMES = FALSE)]
  # Convert character to factor
  if (length(chr_vars) > 0L){
    data[, (chr_vars) :=
           lapply(.SD, function(x) collapse::qF(x, sort = TRUE, ordered = TRUE)),
         .SDcols = chr_vars]
  }
  # Separate vars
  lgl_vars <- data_nms[vapply(data, is.logical, logical(1))]
  num_vars <- data_nms[vapply(data, function(x) inherits(x, c("integer", "numeric")), logical(1))]
  # exotic_num_vars <- data_nms[vapply(data, function(x) !inherits(x, c("integer", "numeric")) &&
  #                                      is.numeric(x), logical(1))]
  date_vars <- data_nms[vapply(data, is_date, logical(1))]
  datetime_vars <- data_nms[vapply(data, is_datetime, logical(1))]
  cat_vars <- data_nms[vapply(data, is.factor, logical(1))]
  other_vars <- setdiff(data_nms,
                        c(lgl_vars, num_vars,
                          # exotic_num_vars,
                          date_vars, datetime_vars, cat_vars))
  if (length(other_vars) > 0){
    warning(paste0("Unsure how to calculate summaries for these variables: \n",
                   paste(other_vars, collapse = "\n")),
            "\n\nFalling back to character")
    data[, (other_vars) := lapply(.SD, function(x) collapse::qF(as.character(x),
                                                                ordered = TRUE, sort = TRUE)),
         .SDcols = other_vars]
    cat_vars <- c(cat_vars, other_vars)
    # Sort cat + other vars in order of first appearance
    cat_vars <- data_nms[sort(match(cat_vars, data_nms))]
  }

  ### Logical variables ###

  lgl_data <- fselect(data, .cols = lgl_vars)
  which_lgl <- which(out[["col"]] %in% lgl_vars)
  lgl_out <- out[which_lgl]
  # Pre-allocate columns
  data.table::set(lgl_out,
                  j = c("n_missing"),
                  value = NA_integer_)
  data.table::set(lgl_out,
                  j = "p_complete",
                  value = NA_real_)
  data.table::set(lgl_out,
                  j = c("n_true", "n_false"),
                  value = NA_integer_)
  data.table::set(lgl_out,
                  j = "p_true",
                  value = NA_real_)
  # data.table::set(lgl_out,
  #                 j = c("first", "last"),
  #                 value = NA)
  data.table::set(lgl_out,
                  j = c("head", "tail"),
                  value = NA_character_)
  if (N > 0L && length(which_lgl) > 0){
    data.table::set(lgl_out, j = "n_missing",
                    value = pluck_row(lgl_data[, lapply(.SD, fnmiss)]))
    data.table::set(lgl_out, j = "p_complete",
                    value = pluck_row(lgl_data[, lapply(.SD, prop_complete)]))
    data.table::set(lgl_out, j = "n_true",
                    value = pluck_row(lgl_data[, lapply(.SD, collapse::fsum)]))
    data.table::set(lgl_out, j = "n_false",
                    value = N - lgl_out[["n_missing"]] - lgl_out[["n_true"]])
    data.table::set(lgl_out, j = "p_true",
                    value = lgl_out[["n_true"]] / (N - lgl_out[["n_missing"]]))
    # data.table::set(lgl_out, j = "first", value = collapse::ffirst(lgl_data))
    # data.table::set(lgl_out, j = "last", value = collapse::flast(lgl_data))
    data.table::set(lgl_out, j = "head",
                    value = pluck_row(lgl_data[, lapply(.SD,
                                                           function(x){
                                                             paste(vec_head(x, n = 3),
                                                                   collapse = ", ")
                                                           })]))
    data.table::set(lgl_out, j = "tail",
                    value = pluck_row(lgl_data[, lapply(.SD,
                                                           function(x){
                                                             paste(vec_tail(x, n = 3),
                                                                   collapse = ", ")
                                                           })]))
  }
  lgl_out <- list_to_tibble(as.list(lgl_out))

  ### Numeric variables ###

  num_data <- fselect(data, .cols = num_vars)
  which_num <- which(out[["col"]] %in% num_vars)
  num_out <- out[which_num]
  # Pre-allocate columns
  data.table::set(num_out,
                  j = c("n_missing"),
                  value = NA_integer_)
  data.table::set(num_out,
                  j = "p_complete",
                  value = NA_real_)
  data.table::set(num_out,
                  j = "n_unique",
                  value = NA_integer_)
  data.table::set(num_out,
                  j = c("mean", "p0", "p25", "p50", "p75", "p100",
                        "iqr"),
                  value = NA_real_)
  data.table::set(num_out,
                  j = "sd",
                  value = NA_real_)
  data.table::set(num_out,
                  j = c("head", "tail"),
                  value = NA_character_)
  if (hist){
    data.table::set(num_out,
                    j = "hist",
                    value = NA_character_)
  }
  if (N > 0L && length(which_num) > 0){
    data.table::set(num_out, j = "n_missing",
                    value = pluck_row(num_data[, lapply(.SD, fnmiss)]))
    data.table::set(num_out, j = "p_complete",
                    value = pluck_row(num_data[, lapply(.SD, prop_complete)]))
    data.table::set(num_out, j = "n_unique",
                    value = pluck_row(num_data[,
                                               lapply(.SD,
                                                      function(x){
                                                        n_unique(x, na.rm = TRUE)
                                                      })]))
    data.table::set(num_out, j = "mean", value = collapse::fmean(num_data))
    # Add quantiles
    q_summary <- q_summarise(num_data, .cols = names(num_data),
                             probs = seq(0, 1, 0.25),
                             pivot = "long")
    for (i in df_seq_along(q_summary, along = "rows")){
      data.table::set(num_out,
                      j = as.character(q_summary[[".quantile"]][i]),
                      value = pluck_row(fselect(q_summary,
                                                   .cols = setdiff(names(q_summary),
                                                                   ".quantile")),
                                           i))
    }
    data.table::set(num_out, j = "iqr", value = num_out[["p75"]] - num_out[["p25"]])
    # data.table::set(num_out, j = "var", value = collapse::fvar(num_data))
    # data.table::set(num_out, j = "sd", value = sqrt(num_out[["var"]]))
    data.table::set(num_out, j = "sd", value = collapse::fsd(num_data))
    # data.table::set(num_out, j = "first", value = collapse::ffirst(num_data))
    # data.table::set(num_out, j = "last", value = collapse::flast(num_data))
    data.table::set(num_out, j = "head",
                    value = pluck_row(num_data[, lapply(.SD,
                                                           function(x){
                                                             paste(vec_head(x, n = 3),
                                                                   collapse = ", ")
                                                             })]))
    data.table::set(num_out, j = "tail",
                    value = pluck_row(num_data[, lapply(.SD,
                                                           function(x){
                                                             paste(vec_tail(x, n = 3),
                                                                   collapse = ", ")
                                                           })]))
    if (hist){
      data.table::set(num_out,
                      j = "hist",
                      value = pluck_row(num_data[, lapply(.SD, finline_hist)]))
    }
  }
  num_out <- list_to_tibble(as.list(num_out))

  ### Exotic numeric variables ###

  # num_data <- fselect(data, .cols = num_vars)
  # which_num <- which(out[["col"]] %in% num_vars)
  # num_out <- out[which_num]
  # # Pre-allocate columns
  # data.table::set(num_out,
  #                 j = c("n_missing"),
  #                 value = NA_integer_)
  # data.table::set(num_out,
  #                 j = "p_complete",
  #                 value = NA_real_)
  # data.table::set(num_out,
  #                 j = "n_unique",
  #                 value = NA_integer_)
  # data.table::set(num_out,
  #                 j = c("mean", "p0", "p25", "p50", "p75", "p100",
  #                       "iqr"),
  #                 value = NA_real_)
  # data.table::set(num_out,
  #                 j = c("sd", "var"),
  #                 value = NA_real_)
  # data.table::set(num_out,
  #                 j = c("head", "tail"),
  #                 value = NA_character_)
  # if (hist){
  #   data.table::set(num_out,
  #                   j = "hist",
  #                   value = NA_character_)
  # }
  # if (N > 0L && length(which_num) > 0){
  #   data.table::set(num_out, j = "n_missing",
  #                   value = pluck_row(num_data[, lapply(.SD, fnmiss)]))
  #   data.table::set(num_out, j = "p_complete",
  #                   value = pluck_row(num_data[, lapply(.SD, prop_complete)]))
  #   data.table::set(num_out, j = "n_unique",
  #                   value = pluck_row(num_data[,
  #                                              lapply(.SD,
  #                                                     function(x){
  #                                                       n_unique(x, na.rm = TRUE)
  #                                                     })]))
  #   data.table::set(num_out, j = "mean", value = collapse::fmean(num_data))
  #   # Add quantiles
  #   q_summary <- q_summarise(num_data, .cols = names(num_data),
  #                            probs = seq(0, 1, 0.25),
  #                            pivot = "long")
  #   for (i in df_seq_along(q_summary, along = "rows")){
  #     data.table::set(num_out,
  #                     j = as.character(q_summary[[".quantile"]][i]),
  #                     value = pluck_row(fselect(q_summary,
  #                                               .cols = setdiff(names(q_summary),
  #                                                               ".quantile")),
  #                                       i))
  #   }
  #   data.table::set(num_out, j = "iqr", value = num_out[["p75"]] - num_out[["p25"]])
  #   data.table::set(num_out, j = "var", value = collapse::fvar(num_data))
  #   data.table::set(num_out, j = "sd", value = sqrt(num_out[["var"]]))
  #   # data.table::set(num_out, j = "first", value = collapse::ffirst(num_data))
  #   # data.table::set(num_out, j = "last", value = collapse::flast(num_data))
  #   data.table::set(num_out, j = "head",
  #                   value = pluck_row(num_data[, lapply(.SD,
  #                                                       function(x){
  #                                                         paste(vec_head(x, n = 3),
  #                                                               collapse = ", ")
  #                                                       })]))
  #   data.table::set(num_out, j = "tail",
  #                   value = pluck_row(num_data[, lapply(.SD,
  #                                                       function(x){
  #                                                         paste(vec_tail(x, n = 3),
  #                                                               collapse = ", ")
  #                                                       })]))
  #   if (hist){
  #     data.table::set(num_out,
  #                     j = "hist",
  #                     value = pluck_row(num_data[, lapply(.SD, finline_hist)]))
  #   }
  # }
  # num_out <- list_to_tibble(as.list(num_out))

  ### Date variables ###

  date_data <- fselect(data, .cols = date_vars)
  which_date <- which(out[["col"]] %in% date_vars)
  date_out <- out[which_date]
  # Pre-allocate columns
  data.table::set(date_out,
                  j = c("n_missing"),
                  value = NA_integer_)
  data.table::set(date_out,
                  j = "p_complete",
                  value = NA_real_)
  data.table::set(date_out,
                  j = "n_unique",
                  value = NA_integer_)
  data.table::set(date_out,
                  j = c("min", "max"),
                  value = lubridate::NA_Date_)
  data.table::set(date_out,
                  j = c("head", "tail"),
                  value = NA_character_)
  if (N > 0L && length(which_date) > 0){
    data.table::set(date_out, j = "n_missing",
                    value = pluck_row(date_data[, lapply(.SD, fnmiss)]))
    data.table::set(date_out, j = "p_complete",
                    value = pluck_row(date_data[, lapply(.SD, prop_complete)]))
    data.table::set(date_out, j = "n_unique",
                    value = pluck_row(date_data[,
                                                lapply(.SD,
                                                       function(x){
                                                         n_unique(x, na.rm = TRUE)
                                                       })]))
    data.table::set(date_out, j = "min",
                    value = pluck_row(date_data[, lapply(.SD, collapse::fmin)]))
    data.table::set(date_out, j = "max",
                    value = pluck_row(date_data[, lapply(.SD, collapse::fmax)]))
    # data.table::set(date_out, j = "first",
    #                 value = pluck_row(date_data[, lapply(.SD, collapse::ffirst)]))
    # data.table::set(date_out, j = "last",
    #                 value = pluck_row(date_data[, lapply(.SD, collapse::flast)]))
    data.table::set(date_out, j = "head",
                    value = pluck_row(date_data[, lapply(.SD,
                                                           function(x){
                                                             paste(vec_head(x, n = 3),
                                                                   collapse = ", ")
                                                           })]))
    data.table::set(date_out, j = "tail",
                    value = pluck_row(date_data[, lapply(.SD,
                                                           function(x){
                                                             paste(vec_tail(x, n = 3),
                                                                   collapse = ", ")
                                                           })]))
  }
  date_out <- list_to_tibble(as.list(date_out))

  ### Datetime variables ###

  datetime_data <- fselect(data, .cols = datetime_vars)
  which_datetime <- which(out[["col"]] %in% datetime_vars)
  datetime_out <- out[which_datetime]
  # Pre-allocate columns
  data.table::set(datetime_out,
                  j = c("n_missing"),
                  value = NA_integer_)
  data.table::set(datetime_out,
                  j = "p_complete",
                  value = NA_real_)
  data.table::set(datetime_out,
                  j = "n_unique",
                  value = NA_integer_)
  data.table::set(datetime_out,
                  j = c("min", "max"),
                  value = lubridate::NA_POSIXct_)
  data.table::set(datetime_out,
                  j = c("head", "tail"),
                  value = NA_character_)
  if (N > 0L && length(which_datetime) > 0){
    data.table::set(datetime_out, j = "n_missing",
                    value = pluck_row(datetime_data[, lapply(.SD, fnmiss)]))
  data.table::set(datetime_out, j = "p_complete",
                  value = pluck_row(datetime_data[, lapply(.SD, prop_complete)]))
  data.table::set(datetime_out, j = "n_unique",
                  value = pluck_row(datetime_data[,
                                              lapply(.SD,
                                                     function(x){
                                                       n_unique(x, na.rm = TRUE)
                                                     })]))
  data.table::set(datetime_out, j = "min",
                  value = pluck_row(datetime_data[, lapply(.SD, collapse::fmin)]))
  data.table::set(datetime_out, j = "max",
                  value = pluck_row(datetime_data[, lapply(.SD, collapse::fmax)]))
  # data.table::set(datetime_out, j = "first",
  #                 value = pluck_row(datetime_data[, lapply(.SD, collapse::ffirst)]))
  # data.table::set(datetime_out, j = "last",
  #                 value = pluck_row(datetime_data[, lapply(.SD, collapse::flast)]))
  data.table::set(datetime_out, j = "head",
                  value = pluck_row(datetime_data[, lapply(.SD,
                                                         function(x){
                                                           paste(vec_head(x, n = 3),
                                                                 collapse = ", ")
                                                         })]))
  data.table::set(datetime_out, j = "tail",
                  value = pluck_row(datetime_data[, lapply(.SD,
                                                         function(x){
                                                           paste(vec_tail(x, n = 3),
                                                                 collapse = ", ")
                                                         })]))
  }
  datetime_out <- list_to_tibble(as.list(datetime_out))

  ### Categorical variables ###

  cat_data <- fselect(data, .cols = cat_vars)
  which_cat <- which(out[["col"]] %in% cat_vars)
  cat_out <- out[which_cat]
  # Pre-allocate columns
  data.table::set(cat_out,
                  j = c("n_missing"),
                  value = NA_integer_)
  data.table::set(cat_out,
                  j = "p_complete",
                  value = NA_real_)
  data.table::set(cat_out,
                  j = "n_unique",
                  value = NA_integer_)
  data.table::set(cat_out,
                  j = c("min", "max", "head", "tail"),
                  value = NA_character_)
  if (N > 0L && length(which_cat) > 0){
    data.table::set(cat_out, j = "n_missing",
                    value = pluck_row(cat_data[, lapply(.SD, fnmiss)]))
    data.table::set(cat_out, j = "p_complete",
                    value = pluck_row(cat_data[, lapply(.SD, prop_complete)]))
    data.table::set(cat_out, j = "n_unique",
                    value = pluck_row(cat_data[,
                                               lapply(.SD,
                                                      function(x){
                                                        n_unique(x, na.rm = TRUE)
                                                      })]))
    data.table::set(cat_out, j = "min",
                    value = pluck_row(cat_data[, lapply(.SD, collapse::fmin)]))
    data.table::set(cat_out, j = "max",
                    value = pluck_row(cat_data[, lapply(.SD, collapse::fmax)]))
    # data.table::set(cat_out, j = "first",
    #                 value = pluck_row(cat_data[, lapply(.SD, collapse::ffirst)]))
    # data.table::set(cat_out, j = "last",
    #                 value = pluck_row(cat_data[, lapply(.SD, collapse::flast)]))
    data.table::set(cat_out, j = "head",
                    value = pluck_row(cat_data[, lapply(.SD,
                                                           function(x){
                                                             paste(vec_head(x, n = 3),
                                                                   collapse = ", ")
                                                           })]))
    data.table::set(cat_out, j = "tail",
                    value = pluck_row(cat_data[, lapply(.SD,
                                                           function(x){
                                                             paste(vec_tail(x, n = 3),
                                                                   collapse = ", ")
                                                           })]))
  }
  cat_out <- list_to_tibble(as.list(cat_out))

  list("nrow" = N,
       "ncol" = ncol(data),
       "logical" = lgl_out,
       "numeric" = num_out,
       "date" = date_out,
       "datetime" = datetime_out,
       "categorical" = cat_out)
}
# Fast skimr:::spark_bar
fspark_bar <- function(x){
  bars <- intToUtf8(c(9601L, 9602L, 9603L, 9605L, 9606L, 9607L), multiple = TRUE)
  out <- bars[
    fcut_ind(x, breaks = seq(0, 1, length.out = length(bars) + 1),
             rightmost.closed = TRUE, left.open = FALSE, all.inside = FALSE)
  ]
  paste0(out, collapse = "")
}
# Fast skimr::inline_hist
finline_hist <- function(x, n_bins = 5L){
  if (length(x) < 1L){
   return(" ")
  }
  if (is.infinite(collapse::fmax(abs(x)))){
    x[is.infinite(x)] <- NA
  }
  if (all(is.na(x))) {
    return(" ")
  }
  if (collapse::allv(collapse::na_rm(x), 0)){
    x <- x + 1
  }
  hist_dt <- tabulate(cut(x, n_bins, labels = FALSE),
                      nbins = n_bins)
  hist_dt <- hist_dt / max(hist_dt)
  fspark_bar(hist_dt)
}
