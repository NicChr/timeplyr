#' A time based extension to `dplyr::distinct()`.
#'
#' @description
#' This works much the same as `dplyr::distinct()`, except that
#' you can supply an additional `time` argument to allow for
#' aggregating time to a higher unit.
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
#' @param .keep_all If `TRUE` then all columns of data frame are kept,
#' default is `FALSE`.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @param include_interval Logical. If `TRUE` then
#' a column "interval" of the form `time_min <= x < time_max` is added
#' showing the time interval in which the respective counts belong to.
#' The rightmost interval will always be closed.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param time_floor Should `from` be floored to the nearest unit
#'  specified through the `time_by`
#' argument? This is particularly useful for starting
#' sequences at the beginning of a week
#' or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday, 7 means Sunday (default).
#' This is only used when `time_floor = TRUE`.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param sort Should the result be sorted? Default is `TRUE`.
#' If `FALSE` then original (input) order is kept.
#'
#' @returns
#' A `data.frame` of distinct aggregate time values across groups.
#'
#' @export
time_distinct <- function(data, time = NULL, ..., time_by = NULL,
                          from = NULL, to = NULL,
                          .keep_all = FALSE,
                          time_type = getOption("timeplyr.time_type", "auto"),
                          include_interval = FALSE,
                          .by = NULL,
                          time_floor = FALSE,
                          week_start = getOption("lubridate.week.start", 1),
                          roll_month = getOption("timeplyr.roll_month", "preday"),
                          roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                          sort = FALSE){
  check_is_df(data)
  group_vars <- get_groups(data, {{ .by }})
  temp_data <- data
  if (length(group_vars(data)) == 0){
    temp_data <- fgroup_by(temp_data, .by = {{ .by }}, order = FALSE)
  }
  group_ids <- df_group_id(temp_data)
  time_info <- mutate_summary_grouped(temp_data, !!enquo(time))
  from_info <- mutate_summary_grouped(temp_data, !!enquo(from), .keep = "none")
  to_info <- mutate_summary_grouped(temp_data, !!enquo(to), .keep = "none")
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
  temp_data <- vctrs::vec_cbind(time_data, from_data, to_data)
  # Add variable to keep track of group IDs
  grp_nm <- new_var_nm(temp_data, ".group.id")
  temp_data <- dplyr::dplyr_col_modify(temp_data,
                                       cols = add_names(list(group_ids), grp_nm))
  int_nm <- character()
  if (length(time_var) > 0L){
    # User supplied unit
    if (!is.null(time_by)){
      time_by <- time_by_list(time_by)
    } else {
      # Function to determine implicit time units
      granularity <- time_granularity(temp_data[[time_var]], msg = FALSE)
      message(paste("Assuming a time granularity of",
                    granularity[["num"]] / granularity[["scale"]],
                    granularity[["granularity"]], sep = " "))
      time_by <- add_names(list(granularity[["num"]]), granularity[["unit"]])
    }
    # Aggregate time data
    time_agg <- time_aggregate_left(temp_data[[time_var]],
                                    time_by = time_by,
                                    g = temp_data[[grp_nm]],
                                    start = fpluck(temp_data, from_var),
                                    end = fpluck(temp_data, to_var),
                                    time_type = time_type,
                                    roll_month = roll_month,
                                    roll_dst = roll_dst,
                                    time_floor = time_floor,
                                    week_start = week_start,
                                    as_int = include_interval)
    time_int_end <- time_int_end(time_agg)
    time_agg <- time_int_rm_attrs(time_agg)
    temp_data <- dplyr::dplyr_col_modify(temp_data,
                                         add_names(
                                           list(time_agg), time_var
                                         )
    )
    if (include_interval){
      int_nm <- new_var_nm(names(temp_data), "interval")
      temp_data <- dplyr::dplyr_col_modify(temp_data,
                                           add_names(
                                             list(
                                               time_interval(time_agg, time_int_end)
                                             ), int_nm
                                           )
      )
    }
  }
  temp_data <- df_rm_cols(temp_data, grp_nm)
  fdistinct(safe_ungroup(temp_data),
            across(dplyr::any_of(c(group_vars, time_var, int_nm))),
            ...,
            .keep_all = .keep_all,
            sort = sort)
}
