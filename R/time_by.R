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
#' * lubridate duration or period object, e.g. `days(1)` or `ddays(1)`.
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
#' the time aggregation when days, weeks,
#' months or years are specified, and `durations`
#' are used otherwise. If `durations`
#' are used the output is always of class `POSIXct`.
#' @param .time_by_group Should the time aggregations be built on a
#' group-by-group basis (the default), or should the time variable be aggregated
#' using the full data? If done by group, different groups may contain
#' different time sequences. This only applies when `.add = TRUE`.
#' @param as_interval Should time variable be a `time_interval`?
#' Default is `FALSE`. \cr
#' This can be controlled globally through `options(timeplyr.use_intervals)`.
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
#' library(lubridate)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#'
#' # Basic usage
#' hourly_flights <- flights %>%
#'   time_by(time_hour) # Detects time granularity
#'
#' hourly_flights
#' time_by_span(hourly_flights)
#'
#' monthly_flights <- flights %>%
#'   time_by(time_hour, "month")
#' weekly_flights <- flights %>%
#'   time_by(time_hour, "week", from = floor_date(min(time_hour), "week"))
#'
#' monthly_flights %>%
#'   count()
#'
#' weekly_flights %>%
#'   summarise(n = n(), arr_delay = mean(arr_delay, na.rm = TRUE))
#'
#' # To aggregate multiple variables, use time_aggregate
#'
#' flights %>%
#'   select(time_hour) %>%
#'   mutate(across(everything(), \(x) time_aggregate(x, time_by = "weeks"))) %>%
#'   count(time_hour)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname time_by
#' @export
time_by <- function(data, time, time_by = NULL,
                    from = NULL, to = NULL,
                    .name = paste0("time_intv_", time_by_pretty(time_by, "_")),
                    .add = FALSE,
                    time_type = getOption("timeplyr.time_type", "auto"),
                    as_interval = getOption("timeplyr.use_intervals", TRUE),
                    .time_by_group = TRUE){
  check_is_df(data)
  data_nms <- names(data)
  group_vars <- group_vars(data)
  out <- safe_ungroup(data)
  time_info <- mutate_summary_ungrouped(out, !!enquo(time))
  from_info <- mutate_summary_ungrouped(out, !!enquo(from), .keep = "none")
  to_info <- mutate_summary_ungrouped(out, !!enquo(to), .keep = "none")
  time_var <- time_info[["cols"]]
  from_var <- from_info[["cols"]]
  to_var <- to_info[["cols"]]
  check_length_lte(time_var, 1)
  check_length_lte(from_var, 1)
  check_length_lte(to_var, 1)

  # Remove duplicate cols..
  time_data <- time_info[["data"]]
  col_seq <- seq_along(names(time_data))
  from_data <- from_info[["data"]]
  to_data <- to_info[["data"]]
  from_data <- fselect(from_data, .cols = which_(names(from_data) %in% names(time_data), invert = TRUE))
  to_data <- fselect(to_data, .cols = which_(names(to_data) %in% names(time_data), invert = TRUE))
  out <- df_cbind(time_data, from_data, to_data)
  if (length(time_var) > 0L){
    check_is_time_or_num(out[[time_var]])
    time_by <- time_by_get(out[[time_var]], time_by = time_by,
                           quiet = TRUE)
    if (time_by_length(time_by) > 1){
      stop("Please supply only one numeric value in time_by")
    }
    if (!.add || !.time_by_group || length(group_vars) == 0L){
      g <- NULL
      time_span_groups <- character(0)
    } else {
      g <- fselect(out, .cols = group_vars)
      time_span_groups <- group_vars
    }
    time_span_GRP <- df_to_GRP(out, .cols = time_span_groups,
                               return.groups = TRUE)
    from_to_list <- get_from_to(out, time = time_var,
                                from = from_var, to = to_var,
                                .by = all_of(time_span_groups))
    # Aggregate time data
    time_agg <- time_aggregate(out[[time_var]],
                                    time_by = time_by,
                                    from = fpluck(from_to_list, 1L),
                                    to = fpluck(from_to_list, 2L),
                                    time_type = time_type,
                                    as_interval = as_interval)
    if (as_interval){
      time_start <- interval_start(time_agg)
    } else {
      time_start <- time_agg
    }
    time_span_start <- collapse::fmin(time_start,
                                      g = time_span_GRP,
                                      use.g.names = FALSE)
    time_span_end <- collapse::fmax(out[[time_var]], g = time_span_GRP,
                                    use.g.names = FALSE)
    # time_span_end <- collapse::fmax(time_int_end(time_agg), g = time_span_GRP,
    #                                 use.g.names = FALSE)
    # time_agg <- time_int_rm_attrs(time_agg)
    time_var <- across_col_names(time_var, .fns = "", .names = .name)
    out <- df_add_cols(out, add_names(list(time_agg), time_var))
    time_span <- GRP_group_data(time_span_GRP)
    if (df_nrow(time_span) == 0L && df_nrow(data) > 0L){
      time_span <- df_init(time_span, 1L)
    }
    time_span$start <- time_span_start
    time_span$end <- time_span_end
    num_gaps <- time_num_gaps(time_start,
                              time_by = time_by,
                              time_type = time_type,
                              g = time_span_GRP, use.g.names = FALSE,
                              check_time_regular = FALSE)
    time_span[["num_gaps"]] <- num_gaps
  }
  groups <- time_var
  if (.add){
    groups <- c(group_vars, time_var)
  }
  out <- fgroup_by(out, .cols = groups)
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
#' @exportS3Method pillar::tbl_sum
tbl_sum.time_tbl_df <- function(x, ...){
  n_groups <- df_nrow(group_data(x))
  group_vars <- group_vars(x)
  time_var <- time_by_var(x)
  non_time_group_vars <- setdiff(group_vars, time_var)
  time_by <- time_by_pretty(attr(x, "time_by"))
  time <- group_data(x)[[time_var]]
  time_span <- time_by_span(x)
  time_range <- c(collapse::fmin(time_span[["start"]], na.rm = TRUE),
                  collapse::fmax(time_span[["end"]], na.rm = TRUE))
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
