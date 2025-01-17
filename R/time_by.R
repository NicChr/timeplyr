#' Group by a time variable at a higher time unit
#'
#' @description
#' `time_by` groups a time variable by a specified time unit like
#' for example "days" or "weeks". \cr
#' It can be used exactly like `dplyr::group_by`.
#'
#' @param data A data frame.
#' @param time Time variable (\bold{data-masking}). \cr
#' E.g., a `Date`, `POSIXt`, `numeric` or any time variable.
#' @param width A [timespan].
#' @param .name An optional glue specification passed to `stringr::glue()`
#' which can be used to concatenate
#' strings to the time column name or replace it.
#' @param .add Should the time groups be added to existing groups?
#' Default is `TRUE`.
#' @param x A `time_tbl_df`.
#'
#' @returns
#' A `time_tbl_df` which for practical purposes can be treated the
#' same way as a dplyr `grouped_df`.
#'
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(fastplyr)
#' library(nycflights13)
#' library(lubridate)
#'
#' # Basic usage
#' hourly_flights <- flights %>%
#'   time_by(time_hour) # Detects time granularity
#'
#' hourly_flights
#'
#' monthly_flights <- flights %>%
#'   time_by(time_hour, "month")
#' weekly_flights <- flights %>%
#'   time_by(time_hour, "week")
#'
#' monthly_flights %>%
#'   f_count()
#'
#' weekly_flights %>%
#'   f_summarise(n = n(), arr_delay = mean(arr_delay, na.rm = TRUE))
#'
#' # To aggregate multiple variables, use time_aggregate
#'
#' flights %>%
#'   f_count(week = time_cut_width(time_hour, months(3)))
#' @rdname time_by
#' @export
time_by <- function(data, time, width = NULL, .name = NULL, .add = TRUE){
  check_is_df(data)
  data_nms <- names(data)
  group_vars <- group_vars(data)
  out <- df_ungroup(data)
  time_info <- mutate_summary_ungrouped(out, !!enquo(time))
  time_var <- time_info[["cols"]]
  check_length_lte(time_var, 1)

  # Remove duplicate cols..
  out <- time_info[["data"]]
  col_seq <- seq_along(names(out))

  if (length(time_var) > 0L){
    check_is_time_or_num(out[[time_var]])
    width <- get_granularity(out[[time_var]], width)
    if (!.add || length(group_vars) == 0L){
      g <- NULL
      time_span_groups <- character(0)
    } else {
      g <- fastplyr::f_select(out, .cols = group_vars)
      time_span_groups <- group_vars
    }
    time_span_GRP <- df_to_GRP(out, .cols = time_span_groups, return.groups = FALSE)
    from <- gmin(out[[time_var]], g = time_span_GRP)
    # Aggregate time data
    time_agg <- time_cut_width(out[[time_var]], width, from = from)
    time_var <- across_col_names(time_var, .fns = "", .names = .name)
    out <- df_add_cols(out, add_names(list(time_agg), time_var))
  }
  groups <- time_var
  if (.add){
    groups <- c(group_vars, time_var)
  }
  out <- fastplyr::f_group_by(out, .cols = groups)
  if (length(time_var) > 0L && length(groups) > 0L){
    out <- dplyr::new_grouped_df(out,
                                 groups = group_data(out),
                                 time = time_var,
                                 class = "time_tbl_df")
  }
  out
}

#' @rdname time_by
#' @export
time_tbl_time_col <- function(x){
  UseMethod("time_tbl_time_col")
}
#' @export
time_tbl_time_col.time_tbl_df <- function(x){
  attr(x, "time")
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.time_tbl_df <- function(x, ...){
  n_groups <- df_nrow(group_data(x))
  group_vars <- group_vars(x)
  time_var <- attr(x, "time")
  non_time_group_vars <- setdiff(group_vars, time_var)
  time <- group_data(x)[[time_var]]
  if (length(non_time_group_vars) > 0L){
    n_non_time_groups <- df_n_distinct(
      fastplyr::f_select(group_data(x),
              .cols = non_time_group_vars)
    )
    n_time_groups <- collapse::fnunique(time)
    groups_header <- c("Groups" =
                         paste0(paste(non_time_group_vars, collapse = ", "),
                                " [",
                                prettyNum(n_non_time_groups, big.mark = ","),
                                "]"))

  } else {
    n_time_groups <- n_groups
    groups_header <- character(0)
  }
  if (!is.null(time)){
    width <- timespan_abbr(interval_width(time))
    time_range <- interval_range(time)
    time_header <- c("Time" = paste0(time_var,
                                     " [",
                                     prettyNum(n_time_groups, big.mark = ","),
                                     "]"))
    time_by_header <- c("Width" = width)
    time_range_header <- c("Range" = paste0(time_range[1L], " -- ", time_range[2L]))
  } else {
    time_header <- character()
    time_by_header <- character()
    time_range_header <- character()
  }
  num_row <- prettyNum(nrow(x), big.mark = ",")
  num_col <- prettyNum(ncol(x), big.mark = ",")
  tbl_header <- c("A tibble" = paste0(num_row, " x ", num_col))
  c(tbl_header,
    groups_header,
    time_header,
    time_by_header,
    time_range_header)
}

#' @exportS3Method fastplyr::reconstruct
reconstruct.time_tbl_df <- function(template, data, copy_extra_attributes = TRUE){
  out <- NextMethod("reconstruct")

  time_var <- time_tbl_time_col(template)
  time <- dplyr::group_data(out)[[time_var]]

  if (is.null(time)){
    attr(out, "time") <- NULL
    class(out) <- cheapr::val_rm(class(out), "time_tbl_df")
  }
  out
}
