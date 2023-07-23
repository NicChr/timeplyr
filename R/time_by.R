#' Group by a time variable at a higher time unit
#'
#' @description `time_by` groups a time variable by a specified time unit like
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
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(nycflights13)
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
                    .add = FALSE,
                    time_type = c("auto", "duration", "period"),
                    time_floor = FALSE,
                    week_start = getOption("lubridate.week.start", 1),
                    roll_month = "preday", roll_dst = "pre",
                    .time_by_group = TRUE){
  time_quo <- enquo(time)
  group_vars <- group_vars(data)
  data <- mutate2(safe_ungroup(data), !!time_quo)
  time_var <- tidy_transform_names(data, !!time_quo)
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
  time_span_end <- collapse::fmax(data[[time_var]], g = time_span_GRP,
                                  use.g.names = FALSE)
  # Aggregate time data
  time_agg <- time_aggregate_left(data[[time_var]],
                                  time_by = time_by,
                                  g = time_span_GRP,
                                  time_type = time_type,
                                  roll_month = roll_month,
                                  roll_dst = roll_dst,
                                  time_floor = time_floor,
                                  week_start = week_start)
  time_agg <- time_int_rm_attrs(time_agg)
  data <- dplyr::mutate(data, !!time_var := time_agg)
  time_span_start <- collapse::fmin(time_agg, g = time_span_GRP,
                                    use.g.names = FALSE)
  time_span <- GRP_group_data(time_span_GRP)
  if (nrow2(time_span) == 0L){
    time_span <- vctrs::vec_init(time_span, n = 1L)
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
  if (length(groups) > 0L){
    out <- dplyr::new_grouped_df(out,
                                 groups = group_data(out),
                                 time = time_var,
                                 time_by = time_by,
                                 time_span = time_span,
                                 class = c("time_tbl_df"))
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
  n_groups <- nrow2(group_data(x))
  group_vars <- group_vars(x)
  time_var <- attr(x, "time")
  non_time_group_vars <- setdiff(group_vars, time_var)
  time_by <- time_by_pretty(attr(x, "time_by"))
  time <- group_data(x)[[time_var]]
  time_range <- collapse::frange(time, na.rm = TRUE)
  if (length(non_time_group_vars) > 0L){
    n_non_time_groups <- nrow2(fdistinct(group_data(x),
                                         .cols = non_time_group_vars,
                                         sort = TRUE))
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
  # default_header <- NextMethod()
  c(tbl_header,
    groups_header,
    time_header,
    time_by_header,
    time_range_header)
}
#' print.time_grouped_df <- function(x, ...){
#'   # default_header <- NextMethod()
#'   # c(default_header, "New" = "A new header")
#'   setup <- pillar::tbl_format_setup(x)
#'   header <- pillar::tbl_format_header(x, setup)
#'   body <- pillar::tbl_format_body(x, setup)
#'   footer <- pillar::tbl_format_footer(x, setup)
#'   header_start <- substr(header[1], 1, 13)
#'   header_end <- substr(header[1], nchar(header[1]) - 4, nchar(header[1]))
#'   n_groups <- nrow2(group_data(x))
#'   group_vars <- group_vars(x)
#'   time_var <- attr(x, "time")
#'   non_time_group_vars <- setdiff(group_vars, time_var)
#'   time_by <- time_by_pretty(attr(x, "time_by"))
#'   time <- group_data(x)[[time_var]]
#'   # time_range <- c(collapse::ffirst(time, na.rm = TRUE),
#'   #                 collapse::flast(time, na.rm = TRUE))
#'   time_range <- collapse::frange(time, na.rm = TRUE)
#'
#'   if (length(non_time_group_vars) > 0L){
#'     n_non_time_groups <- nrow2(fdistinct(group_data(x),
#'                                          .cols = non_time_group_vars,
#'                                          sort = TRUE))
#'     n_time_groups <- n_unique(time)
#'     header[2L] <- paste0(header_start,
#'                         "Groups:   ",
#'                         paste(non_time_group_vars, collapse = ", "),
#'                         " [",
#'                         prettyNum(n_non_time_groups, big.mark = ","),
#'                         "]",
#'                         header_end)
#'
#'   } else {
#'     n_time_groups <- n_groups
#'     header <- header[1L]
#'   }
#'   time_header <- paste0("Time:     ",
#'                         time_var,
#'                         " [",
#'                         prettyNum(n_time_groups, big.mark = ","),
#'                         "]")
#'   time_by_header <- paste0("By:       ",
#'                            time_by)
#'   time_range_header <- paste0("Span:     ",
#'                               time_range[1L], " - ", time_range[2L])
#'   # time_header <- paste0(header_start,
#'   #                       "Time:     ",
#'   #                       time_var,
#'   #                       " [",
#'   #                       prettyNum(n_time_groups, big.mark = ","),
#'   #                       "]",
#'   #                       header_end)
#'   # time_by_header <- paste0(header_start,
#'   #                          "By:       ",
#'   #                          time_by,
#'   #                          header_end)
#'   # time_range_header <- paste0(header_start,
#'   #                             "Span:     ",
#'   #                             time_range[1L], " - ", time_range[2L],
#'   #                             header_end).
#'   time_header <- paste0(header_start, time_header, header_end)
#'   time_by_header <- paste0(header_start, time_by_header, header_end)
#'   time_range_header <- paste0(header_start, time_range_header, header_end)
#'   writeLines(c(header,
#'                time_header,
#'                time_by_header,
#'                time_range_header,
#'                body,
#'                footer))
#'   invisible(x)
#' }
