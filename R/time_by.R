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
#' @param width A [timespan].
#' @param .name An optional glue specification passed to `stringr::glue()`
#' which can be used to concatenate
#' strings to the time column name or replace it.
#' @param .add Should the time groups be added to existing groups?
#' Default is `FALSE`.
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
time_by <- function(data, time, width = NULL,
                    .name = NULL,
                    .add = FALSE){
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
    width <- get_time_granularity(out[[time_var]], width)
    if (!.add || !.time_by_group || length(group_vars) == 0L){
      g <- NULL
      time_span_groups <- character(0)
    } else {
      g <- fastplyr::f_select(out, .cols = group_vars)
      time_span_groups <- group_vars
    }
    time_span_GRP <- df_to_GRP(out, .cols = time_span_groups,
                               return.groups = TRUE)
    from <- gmin(out[[time_var]], g = time_span_GRP)
    # Aggregate time data
    time_agg <- time_cut_width(out[[time_var]], width, from = from)
    time_var <- across_col_names(time_var, .fns = "", .names = .name)
    out <- df_add_cols(out, add_names(list(time_agg), time_var))
    # time_span <- GRP_group_data(time_span_GRP)
    # if (df_nrow(time_span) == 0L && df_nrow(data) > 0L){
    #   time_span <- df_init(time_span, 1L)
    # }
    # time_span$start <- time_span_start
    # time_span$end <- time_span_end
    # num_gaps <- time_num_gaps(time_start,
    #                           time_by = time_by,
    #                           time_type = time_type,
    #                           g = time_span_GRP, use.g.names = FALSE,
    #                           check_time_regular = FALSE)
    # time_span[["num_gaps"]] <- num_gaps
  }
  groups <- time_var
  if (.add){
    groups <- c(group_vars, time_var)
  }
  out <- fastplyr::f_group_by(out, .cols = groups)
  # if (length(groups) > 0L){
  #   out <- dplyr::new_grouped_df(out,
  #                                groups = group_data(out),
  #                                time = time_var,
  #                                time_by = time_by,
  #                                time_span = time_span,
  #                                class = "time_tbl_df")
  # }
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
