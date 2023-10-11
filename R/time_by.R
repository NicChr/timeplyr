#' Group by a time variable at a higher time unit
#'
#' @description
#' `time_by` groups a time variable by a specified time unit like
#' for example "days" or "weeks". \cr
#' It can be used exactly like `dplyr::group_by`.
#'
#' @param data A data frame.
#' @param time Time variable (\bold{data-masking}). \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`, `yearmon`, or `yearqtr`.
#' @param time_by Time unit. \cr
#' Must be one of the following:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * Lubridate duration or period object, e.g. `days(1)` or `ddays(1)`.
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param from (Optional) Start time.
#' @param to (Optional) end time.
#' @param .name An optional glue specification passed to `stringr::glue()`
#' which can be used to concatenate
#' strings to the time column name or replace it.
#' @param .add Should the time groups be added to existing groups?
#' Default is `FALSE`.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks,
#' months or years are specified, and `durations`
#' are used otherwise. If `durations`
#' are used the output is always of class `POSIXt`.
#' @param time_floor Should the start of each time sequence
#' be floored to
#' the nearest unit specified through the `time_by`
#' argument? This is particularly useful for
#' starting sequences at the beginning of a week
#' or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `time_floor = TRUE`.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param .time_by_group Should the time aggregations be built on a
#' group-by-group basis (the default), or should the time variable be aggregated
#' using the full data? If done by group, different groups may contain
#' different time sequences. This only applies when `.add = TRUE`.
#' @param x A `time_tbl_df`.
#'
#' @returns
#' A `time_tbl_df` which for practical purposes can be treated the
#' same way as a dplyr `grouped_df`.
#'
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(nycflights13)
#' \dontshow{
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' monthly_flights <- flights %>%
#'   time_by(time_hour, "month")
#'
#' monthly_flights
#'
#' time_by_span(monthly_flights)
#'
#' monthly_flights <- flights %>%
#'   time_by(time_hour, "month", time_floor = TRUE) %>%
#'   count()
#' weekly_flights <- flights %>%
#'   time_by(time_hour, "week", time_floor = TRUE) %>%
#'   count()
#'
#' rolling_weekly_flights <- flights %>%
#'   time_by(time_hour, "week") %>%
#'   summarise(n = n(),
#'             arr_delay = mean(arr_delay, na.rm = TRUE))
#'
#' monthly_flights
#' weekly_flights
#' rolling_weekly_flights
#' @rdname time_by
#' @export
time_by <- function(data, time, time_by = NULL,
                    from = NULL, to = NULL,
                    .name = "{.col}",
                    .add = FALSE,
                    time_type = c("auto", "duration", "period"),
                    time_floor = FALSE,
                    week_start = getOption("lubridate.week.start", 1),
                    roll_month = "preday", roll_dst = "pre",
                    .time_by_group = TRUE){
  check_is_df(data)
  data_nms <- names(data)
  group_vars <- group_vars(data)
  rlang::check_required(time)
  data <- mutate2(safe_ungroup(data),
                  !!enquo(time),
                  !!enquo(from),
                  !!enquo(to))
  time_var <- tidy_transform_names(data, !!enquo(time))
  from_var <- tidy_transform_names(data, !!enquo(from))
  to_var <- tidy_transform_names(data, !!enquo(to))
  if (length(time_var) > 0L){
    if (length(time_var) > 1L){
      stop("Please choose one time variable.")
    }
  time_by <- time_by_get(data[[time_var]], time_by = time_by,
                         quiet = TRUE)
  if (time_by_length(time_by) > 1){
    stop("Please supply only one numeric value in time_by")
  }
  if (!.add || !.time_by_group || length(group_vars) == 0L){
    g <- NULL
    time_span_groups <- character(0)
  } else {
    g <- fselect(data, .cols = group_vars)
    time_span_groups <- group_vars
  }
  time_span_GRP <- df_to_GRP(data, .cols = time_span_groups,
                             return.groups = TRUE)
  from_to_list <- get_from_to(data, time = time_var,
                              from = from_var, to = to_var,
                              .by = all_of(time_span_groups))
  # Aggregate time data
  time_agg <- time_aggregate_switch(data[[time_var]],
                                    time_by = time_by,
                                    start = fpluck(from_to_list, 1L),
                                    end = fpluck(from_to_list, 2L),
                                    g = time_span_GRP,
                                    time_type = time_type,
                                    roll_month = roll_month,
                                    roll_dst = roll_dst,
                                    time_floor = time_floor,
                                    week_start = week_start,
                                    as_int = TRUE)
  time_span_start <- collapse::fmin(time_agg, g = time_span_GRP,
                                    use.g.names = FALSE)
  time_span_end <- collapse::fmax(time_int_end(time_agg), g = time_span_GRP,
                                    use.g.names = FALSE)
  time_agg <- time_int_rm_attrs(time_agg)
  time_var <- across_col_names(time_var, .fns = "", .names = .name)
  data <- dplyr::mutate(data, "{time_var}" := time_agg)
  time_span <- GRP_group_data(time_span_GRP)
  if (df_nrow(time_span) == 0L){
    time_span <- df_init(time_span, 1L)
  }
  time_span$start <- time_span_start
  time_span$end <- time_span_end
  num_gaps <- time_num_gaps(data[[time_var]],
                            time_by = time_by,
                            time_type = time_type,
                            g = g, use.g.names = FALSE,
                            check_time_regular = FALSE)
  time_span[["num_gaps"]] <- num_gaps
  }
  groups <- time_var
  if (.add){
    groups <- c(group_vars, time_var)
  }
  out <- fgroup_by(data, .cols = groups)
  if (isTRUE(is.na(match(from_var, data_nms)))){
    out <- df_rm_cols(out, from_var)
  }
  if (isTRUE(is.na(match(to_var, data_nms)))){
    out <- df_rm_cols(out, to_var)
  }
  if (length(groups) > 0L){
    out <- dplyr::new_grouped_df(out,
                                 groups = group_data(out),
                                 time = time_var,
                                 time_by = time_by,
                                 time_span = time_span,
                                 class = "time_tbl_df")
  }
  out
}
#' @rdname time_by
#' @export
time_by_span <- function(x){
  UseMethod("time_by_span")
}
#' @export
time_by_span.time_tbl_df <- function(x){
  attr(x, "time_span")
}
#' @rdname time_by
#' @export
time_by_var <- function(x){
  UseMethod("time_by_var")
}
#' @export
time_by_var.time_tbl_df <- function(x){
  attr(x, "time")
}
#' @rdname time_by
#' @export
time_by_units <- function(x){
  UseMethod("time_by_units")
}
#' @export
time_by_units.time_tbl_df <- function(x){
  attr(x, "time_by")
}
tbl_sum.time_tbl_df <- function(x, ...){
  n_groups <- df_nrow(group_data(x))
  group_vars <- group_vars(x)
  time_var <- time_by_var(x)
  non_time_group_vars <- setdiff(group_vars, time_var)
  time_by <- time_by_pretty(attr(x, "time_by"))
  time <- group_data(x)[[time_var]]
  time_span <- time_by_span(x)
  time_range <- c(min(time_span[["start"]], na.rm = TRUE),
                  max(time_span[["end"]], na.rm = TRUE))
  if (length(non_time_group_vars) > 0L){
    n_non_time_groups <- df_n_distinct(
      fselect(group_data(x),
              .cols = non_time_group_vars)
    )
    n_time_groups <- n_unique(time)
    groups_header <- c("Groups" =
                         paste0(paste(non_time_group_vars, collapse = ", "),
                                " [",
                                prettyNum(n_non_time_groups, big.mark = ","),
                                "]"))

  } else {
    n_time_groups <- n_groups
    groups_header <- character(0)
  }
  time_header <- c("Time" = paste0(time_var,
                                   " [",
                                   prettyNum(n_time_groups, big.mark = ","),
                                   "]"))
  time_by_header <- c("By" = time_by)
  time_range_header <- c("Span" = paste0(time_range[1L], " - ", time_range[2L]))
  num_row <- prettyNum(nrow(x), big.mark = ",")
  num_col <- prettyNum(ncol(x), big.mark = ",")
  tbl_header <- c("A tibble" = paste0(num_row, " x ", num_col))
  c(tbl_header,
    groups_header,
    time_header,
    time_by_header,
    time_range_header)
}
