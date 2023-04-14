#' A time based extension to `dplyr::summarise()`/`dplyr::reframe()`
#'
#' @description This works much the same as `dplyr::summarise()`, except that
#' you can supply an additional `time` argument to allow for
#' aggregating time to a higher unit.
#'
#' Currently, this does \bold{not} support
#' filling in missing gaps in time. \cr
#' Use `time_count()` or `time_complete()`
#' before using this if you believe there may be gaps in time.
#'
#' @param data A data frame.
#' @param ... Additional variables to include.
#' `dplyr` "datamasking" semantics are used.
#' @param time Time variable.
#' @param by Time unit to summarise time series by.
#' If `by` is `NULL` then a heuristic will try and estimate the highest
#' order time unit associated with the time variable.
#' If specified, then by must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `by = "days"` or `by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `by = 1`.
#' This is also vectorized where applicable.
#' @param data A data frame.
#' @param ... Additional variables to include.
#' `dplyr` "datamasking" semantics are used.
#' @param time Time variable.
#' @param by Argument to expand and summarise time series.
#' If `by` is `NULL` then a heuristic will try and estimate the highest
#' order time unit associated with the time variable.
#' If specified, then by must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `by = "days"` or `by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `by = 1`.
#' This is also vectorized where applicable.
#' @param from Time series start date.
#' @param to Time series end date.
#' @param seq_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified, and `durations`
#' are used otherwise.
#' @param include_interval Logical. If `TRUE` then
#' a column "interval" of the form `time_min <= x < time_max` is added
#' showing the time interval in which the respective counts belong to.
#' The rightmost interval will always be closed.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param keep_class Logical. If `TRUE` then the class of the input data is retained.
#' If `FALSE`, which is sometimes faster, a `data.table` is returned.
#' @param floor_date Should `from` be floored to the nearest unit specified through the `by`
#' argument? This is particularly useful for starting sequences at the beginning of a week
#' or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday, 7 means Sunday (default). This is only used when `floor_date = TRUE`.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param sort Should the result be sorted? Default is `TRUE`.
#' If `FALSE` then original (input) order is kept.
#' The sorting only applies to groups and time variable.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' library(nycflights13)
#'
#' # Works the same way as summarise()
#' flights %>%
#'   summarise(across(where(is.numeric), mean))
#' flights %>%
#'   time_summarise(across(where(is.numeric), mean))
#'
#' # Like the other time_ functions, it allows for an additional time variable to
#' # aggregate by
#'
#' # Monthly averages for each numeric variable
#' flights %>%
#'   time_summarise(mean_arr_time = mean(arr_time, na.rm = TRUE),
#'                  time = across(time_hour, as_date),
#'                  by = "month",
#'                  include_interval = TRUE)
#' # Example of monthly summary using zoo's yearmon
#' \dontrun{
#' flights %>%
#'   mutate(yearmon = zoo::as.yearmon(as_date(time_hour))) %>%
#'   time_summarise(time = yearmon,
#'                  n = n(),
#'                  mean_arr_time = mean(arr_time, na.rm = TRUE),
#'                  mean_dep_time = mean(dep_time, na.rm = TRUE),
#'                  mean_diff_time = mean(arr_time - dep_time, na.rm = TRUE),
#'                  include_interval = TRUE)
#' }
#' @rdname time_summarise
#' @export
time_summarise <- function(data, ..., time = NULL, by = NULL,
                        from = NULL, to = NULL,
                        seq_type = c("auto", "duration", "period"),
                        include_interval = FALSE,
                        .by = NULL,
                        keep_class = TRUE,
                        floor_date = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        roll_month = "preday", roll_dst = "pre",
                        sort = TRUE){
  int_nm <- new_var_nm(data, "interval")
  out <- dplyr::distinct(time_mutate(data, !!!enquos(...),
              time = !!enquo(time),
              by = by,
              from = !!enquo(from),
              to = !!enquo(to),
              seq_type = seq_type,
              include_interval = include_interval,
              .by = {{ .by }},
              .keep = "none",
              keep_class = keep_class,
              floor_date = floor_date,
              week_start = week_start,
              roll_month = roll_month, roll_dst = roll_dst,
              sort = sort))
  time_var <- tidy_transform_names(safe_ungroup(data), !!enquo(time))
  group_info <- get_group_info(data, !!!enquos(...),
                               type = "data-mask",
                               .by = {{ .by }})
  group_vars <-  group_info[["dplyr_groups"]]
  extra_group_vars <- group_info[["extra_groups"]]
  dplyr::select(out, dplyr::any_of(c(group_vars, time_var, int_nm, extra_group_vars)))
}
#' @rdname time_summarise
#' @export
time_reframe <- function(data, ..., time = NULL, by = NULL,
                         from = NULL, to = NULL,
                         seq_type = c("auto", "duration", "period"),
                         include_interval = FALSE,
                         .by = NULL,
                         keep_class = TRUE,
                         floor_date = FALSE,
                         week_start = getOption("lubridate.week.start", 1),
                         roll_month = "preday", roll_dst = "pre",
                         sort = TRUE){
  group_vars <- get_groups(data, {{ .by }})
  int_nm <- new_var_nm(data, "interval")
  out <- time_mutate(data, time = !!enquo(time),
                     by = by,
                     from = !!enquo(from),
                     to = !!enquo(to),
                     seq_type = seq_type,
                     include_interval = include_interval,
                     .by = {{ .by }},
                     .keep = "all",
                     keep_class = FALSE,
                     floor_date = floor_date,
                     week_start = week_start,
                     roll_month = roll_month, roll_dst = roll_dst,
                     sort = sort)
  time_var <- tidy_transform_names(safe_ungroup(data), !!enquo(time))
  out <- dplyr_summarise(out, !!!enquos(...), .by = dplyr::any_of(c(group_vars, time_var, int_nm)))
  if (keep_class) out <- df_reconstruct(out, safe_ungroup(data))
  safe_ungroup(out)
}
