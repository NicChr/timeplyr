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
#' Must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
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
#' @param x A `time_grouped_df`.
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(nycflights13)
#' flights <- flights %>%
#'   time_by(time_hour, "month")
#'
#' flights
#'
#' time_by_span(flights)
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
  # data <- mutate2(safe_ungroup(data), ...)
  # dot_vars <- tidy_transform_names(data, ...)
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
  data <- dplyr::dplyr_col_modify(data, cols = setnames(
    list(
      time_summarisev(data[[time_var]],
                      time_by = time_by,
                      time_type = time_type,
                      time_floor = time_floor,
                      week_start = week_start,
                      roll_month = roll_month,
                      roll_dst = roll_dst,
                      sort = FALSE, unique = FALSE,
                      g = g,
                      use.g.names = FALSE)
    ), time_var
  ))
  time_span <- stat_summarise(data, .cols = time_var,
                              .by = all_of(time_span_groups),
                              stat = c("min", "max"),
                              sort = TRUE)
  time_span <- frename(time_span, .cols = c("start" = "min",
                                            "end" = "max"))
  num_gaps <- time_num_gaps(data[[time_var]],
                            time_by = time_by,
                            time_type = time_type,
                            g = g, use.g.names = FALSE)
  time_span[["num_gaps"]] <- num_gaps
  time_span <- dplyr::as_tibble(time_span)
  }
  groups <- time_var
  if (.add){
    groups <- c(group_vars, time_var)
  }
  out <- fgroup_by(data, .cols = groups)
  if (length(groups) > 0L){
    out <- structure(out,
                     time = time_var,
                     time_by = time_by,
                     time_span = time_span,
                     class = c("time_grouped_df", "grouped_df",
                               "tbl_df", "tbl", "data.frame"))
  }
  out
}
#' @rdname time_by
#' @export
time_by_span <- function(x){
  UseMethod("time_by_span")
}
#' @export
time_by_span.time_grouped_df <- function(x){
  attr(x, "time_span")
}
#' @rdname time_by
#' @export
time_by_var <- function(x){
  UseMethod("time_by_var")
}
#' @export
time_by_var.time_grouped_df <- function(x){
  attr(x, "time")
}
#' @rdname time_by
#' @export
time_by_units <- function(x){
  UseMethod("time_by_units")
}
#' @export
time_by_units.time_grouped_df <- function(x){
  attr(x, "time_by")
}
#' @export
print.time_grouped_df <- function(x, ...){
  setup <- pillar::tbl_format_setup(x)
  header <- pillar::tbl_format_header(x, setup)
  body <- pillar::tbl_format_body(x, setup)
  footer <- pillar::tbl_format_footer(x, setup)

  n_groups <- nrow2(group_data(x))
  group_vars <- group_vars(x)
  time_var <- attr(x, "time")
  non_time_group_vars <- setdiff(group_vars, time_var)
  time_by <- time_by_pretty(attr(x, "time_by"))
  time <- group_data(x)[[time_var]]
  # time_range <- c(collapse::ffirst(time, na.rm = TRUE),
  #                 collapse::flast(time, na.rm = TRUE))
  time_range <- collapse::frange(time, na.rm = TRUE)

  if (length(non_time_group_vars) > 0L){
    n_non_time_groups <- nrow2(dplyr::distinct(group_data(x),
                                               across(all_of(non_time_group_vars))))
    n_time_groups <- n_unique(time)
    header <- paste0("\033[38;5;246m# Groups:   ",
                     paste(non_time_group_vars, collapse = ", "),
                     " [",
                     prettyNum(n_non_time_groups, big.mark = ","),
                     "]\033[39m")

  } else {
    n_time_groups <- n_groups
    header <- character(0)
  }
  time_header <- paste0("\033[38;5;246m# Time:     ",
                        time_var,
                        " [",
                        prettyNum(n_time_groups, big.mark = ","),
                        "]\033[39m")
  time_by_header <- paste0("\033[38;5;246m# By:       ",
                           time_by,
                           "\033[39m")
  time_range_header <- paste0("\033[38;5;246m# Span:     ",
                           time_range[1L], " - ", time_range[2L],
                           "\033[39m")
  writeLines(c(header,
               time_header,
               time_by_header,
               time_range_header,
               body,
               footer))
  invisible(x)
}
