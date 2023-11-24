#' A time based extension to `dplyr::mutate()`.
#'
#' @description
#' This works much the same as `dplyr::mutate()`, except that
#' you can supply an additional `time` argument to allow for
#' aggregating time to a higher unit.
#'
#' Currently, this does \bold{not} support
#' filling in missing gaps in time. \cr
#' Use `time_count()` or `time_complete()`
#' before using this if you believe there may be gaps in time.
#'
#' @param data A data frame.
#' @param time Time variable.
#' @param ... Additional variables to include.
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
#' @param from Time series start date.
#' @param to Time series end date.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations`
#' are used otherwise.
#' @param include_interval Logical. If `TRUE` then
#' a column "interval" of the form `time_min <= x < time_max` is added
#' showing the time interval in which the respective counts belong to.
#' The rightmost interval will always be closed.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param .keep Control which columns are retained.
#' See `?dplyr::mutate` for more details.
#' @param time_floor Should `from` be floored to the nearest unit specified
#' through the `time_by`
#' argument? This is particularly useful for starting
#' sequences at the beginning of a week or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `floor_date = TRUE`.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#'
#' @returns
#' A `data.frame` with added columns.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' library(nycflights13)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # Like the other time_ functions, it allows for an additional time variable to
#' # aggregate by
#' flights %>%
#'   fdistinct(time_hour) %>%
#'   time_mutate(time = across(time_hour, as_date),
#'               time_by = "month", .keep = "none",
#'               include_interval = TRUE) %>%
#'   fdistinct()
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
time_mutate <- function(data, time = NULL, ..., time_by = NULL,
                        from = NULL, to = NULL,
                        time_type = getOption("timeplyr.time_type", "auto"),
                        include_interval = FALSE,
                        .by = NULL,
                        .keep = c("all", "used", "unused", "none"),
                        time_floor = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        roll_month = getOption("timeplyr.roll_month", "preday"),
                        roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  check_is_df(data)
  has_groups <- length(group_vars(data)) > 0
  group_vars <- get_groups(data, {{ .by }})
  out <- data
  if (!has_groups){
    out <- fgroup_by(out, .by = {{ .by }}, order = FALSE)
  }
  group_ids <- df_group_id(out)
  time_info <- mutate_summary_grouped(out, !!enquo(time))
  from_info <- mutate_summary_grouped(out, !!enquo(from), .keep = "none")
  to_info <- mutate_summary_grouped(out, !!enquo(to), .keep = "none")
  time_var <- time_info[["cols"]]
  from_var <- from_info[["cols"]]
  to_var <- to_info[["cols"]]
  check_length_lte(time_var, 1)
  check_length_lte(from_var, 1)
  check_length_lte(to_var, 1)

  # Remove duplicate cols
  time_data <- safe_ungroup(time_info[["data"]])
  from_data <- safe_ungroup(from_info[["data"]])
  to_data <- safe_ungroup(to_info[["data"]])
  from_data <- fselect(from_data,
                       .cols = cpp_which(match(names(from_data), names(time_data), 0L) == 0L))
  to_data <- fselect(to_data,
                     .cols = cpp_which(match(names(to_data), names(time_data), 0L) == 0L))
  out <- vctrs::vec_cbind(time_data, from_data, to_data)
  # Add variable to keep track of group IDs
  grp_nm <- new_var_nm(out, ".group.id")
  out <- dplyr::dplyr_col_modify(out,
                                 cols = add_names(list(group_ids), grp_nm))
  int_nm <- character()
  if (length(time_var) > 0L){
    # User supplied unit
    if (!is.null(time_by)){
      time_by <- time_by_list(time_by)
    } else {
      # Function to determine implicit time units
      granularity <- time_granularity(out[[time_var]], msg = FALSE)
      message(paste("Assuming a time granularity of",
                    granularity[["num"]] / granularity[["scale"]],
                    granularity[["granularity"]], sep = " "))
      time_by <- add_names(list(granularity[["num"]]), granularity[["unit"]])
    }
    # Aggregate time data
    time_agg <- time_aggregate_switch(out[[time_var]],
                                      time_by = time_by,
                                      g = out[[grp_nm]],
                                      start = fpluck(out, from_var),
                                      end = fpluck(out, to_var),
                                      time_type = time_type,
                                      roll_month = roll_month,
                                      roll_dst = roll_dst,
                                      time_floor = time_floor,
                                      week_start = week_start,
                                      as_int = include_interval)
    time_int_end <- time_int_end(time_agg)
    time_agg <- time_int_rm_attrs(time_agg)
    out <- dplyr::dplyr_col_modify(out,
                                   add_names(
                                     list(time_agg), time_var
                                   )
    )
    if (include_interval){
      int_nm <- new_var_nm(names(out), "interval")
      out <- dplyr::dplyr_col_modify(out,
                                     add_names(
                                       list(
                                         time_interval(time_agg, time_int_end)
                                       ), int_nm
                                     )
      )
    }
  }
  out <- df_rm_cols(out, grp_nm)
  out <- mutate2(safe_ungroup(out),
                 ...,
                 .by = all_of(c(group_vars, time_var, int_nm)),
                 .keep = .keep)
  # if (has_groups && groups_equal(time_info[["data"]], data)){
  #  attr(out, "groups") <- attr(data, "groups")
  #  class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  #  out
  # } else {
  #   df_reconstruct(out, data)
  # }
  df_reconstruct(out, data)
}
