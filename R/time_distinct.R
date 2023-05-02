#' A time based extension to `dplyr::distinct()`.
#'
#' @description This works much the same as `dplyr::distinct()`, except that
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
#' @param from Time series start date.
#' @param to Time series end date.
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
#' @param seq_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified, and `durations`
#' are used otherwise.
#' @param include_interval Logical. If `TRUE` then
#' a column "interval" of the form `time_min <= x < time_max` is added
#' showing the time interval in which the respective counts belong to.
#' The rightmost interval will always be closed.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
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
#' @export
time_distinct <- function(data, ..., time = NULL, by = NULL,
                          from = NULL, to = NULL,
                          .keep_all = FALSE,
                          seq_type = c("auto", "duration", "period"),
                          include_interval = FALSE,
                          .by = NULL,
                          floor_date = FALSE,
                          week_start = getOption("lubridate.week.start", 1),
                          roll_month = "preday", roll_dst = "pre",
                          sort = FALSE){
  n_dots <- dots_length(...)
  group_vars <- get_groups(data, .by = {{ .by }})
  out <- safe_ungroup(data)
  if (n_dots > 0){
    out <- dplyr::mutate(out, ...)
  }
  dot_vars <- tidy_transform_names(data, ...)
  if (include_interval){
    int_nm <- new_var_nm(out, "interval")
  } else {
    int_nm <- character(0)
  }
  out <- time_mutate(out, across(all_of(dot_vars)),
                     time = !!enquo(time),
                     by = by,
                     from = !!enquo(from),
                     to = !!enquo(to),
                     seq_type = seq_type,
                     include_interval = include_interval,
                     .by = all_of(group_vars),
                     .keep = "all",
                     floor_date = floor_date,
                     week_start = week_start,
                     roll_month = roll_month, roll_dst = roll_dst,
                     sort = sort)
  time_var <- tidy_transform_names(data, !!enquo(time))
  out <- fdistinct(out, across(dplyr::any_of(c(group_vars, time_var, int_nm, dot_vars))),
            .keep_all = .keep_all)
  out
}
