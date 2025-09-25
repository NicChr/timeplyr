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
#' hourly_flights <- flights |>
#'   time_by(time_hour) # Detects time granularity
#'
#' hourly_flights
#'
#' monthly_flights <- flights |>
#'   time_by(time_hour, "month")
#' weekly_flights <- flights |>
#'   time_by(time_hour, "week")
#'
#' monthly_flights |>
#'   f_count()
#'
#' weekly_flights |>
#'   f_summarise(n = n(), arr_delay = mean(arr_delay, na.rm = TRUE))
#'
#' # To aggregate multiple variables, use `time_cut_width`
#'
#' flights |>
#'   f_count(week = time_cut_width(time_hour, months(3)))
#' @rdname time_by
#' @export
time_by <- function(data, time, width = NULL, .name = NULL, .add = TRUE){
  check_is_df(data)
  data_nms <- names(data)
  group_vars <- fastplyr::f_group_vars(data)
  out <- fastplyr::f_ungroup(data)
  time_info <- mutate_one(out, !!enquo(time))
  time_var <- names(time_info)
  check_length_lte(time_var, 1)
  out <- cheapr::df_modify(data, time_info)
  col_seq <- seq_along(names(out))
  from <- NULL # Initialise

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
    if (length(time_span_groups) != 0){
      from <- out |>
        fastplyr::f_ungroup() |>
        fastplyr::f_mutate(
        dplyr::across(
          dplyr::all_of(time_var),
          collapse::fmin, .names = ".min."
        ),
        .by = all_of(time_span_groups)
      ) |>
        fastplyr::f_pull(.cols = ".min.")
    }
    # Aggregate time data
    time_agg <- time_cut_width(out[[time_var]], width, from = from)
    time_var <- across_col_names(time_var, .fns = "", .names = .name)
    out <- cheapr::df_modify(out, add_names(list(time_agg), time_var))
  }
  groups <- time_var
  if (.add){
    groups <- c(group_vars, time_var)
  }
  out <- fastplyr::f_group_by(out, .cols = groups)
  if (length(time_var) > 0L && length(groups) > 0L){
    out <- dplyr::new_grouped_df(out,
                                 groups = fastplyr::f_group_data(out),
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
  n_groups <- df_nrow(fastplyr::f_group_data(x))
  group_vars <- fastplyr::f_group_vars(x)
  time_var <- attr(x, "time")
  non_time_group_vars <- setdiff(group_vars, time_var)
  time <- fastplyr::f_group_data(x)[[time_var]]
  if (length(non_time_group_vars) > 0L){
    n_non_time_groups <- df_n_distinct(
      fastplyr::f_select(fastplyr::f_group_data(x),
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

#' @exportS3Method cheapr::rebuild
rebuild.time_tbl_df <- function(x, template, ...){

  time_var <- time_tbl_time_col(template)
  time <- fastplyr::f_group_data(x)[[time_var]]

  class(template) <- setdiff(class(template), "time_tbl_df")
  out <- cheapr::rebuild(x, template, ...)

  if (is.null(time) || !is_time_interval(time)){
    attr(out, "time") <- NULL
    class(out) <- cheapr::val_rm(class(out), "time_tbl_df")
  }
  out
}
