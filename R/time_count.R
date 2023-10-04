#' Fast count, fill implicit missing gaps, and (time)
#' aggregate a time series based data frame.
#'
#' @description This function operates like `dplyr::count()`
#' but with emphasis on
#' a specified time variable. This creates an
#' aggregated frequency time series where
#' implicit missing gaps in time are filled are
#' expanded and further aggregated to
#' both lower and higher levels of time.
#' It is conceptually a wrapper around `count()` and `complete()`,
#' but uses data.table for data frame handling,
#' collapse for grouping calculations,
#' as well as lubridate and timechange for the time sequence calculations. \cr
#'
#' An important note is that when the data are grouped, time ranges are expanded
#' on a group-by-group basis. This works like dplyr where you can supply either a
#' grouped_df or specify the groupings through `.by`.
#' When groups are supplied through
#' `...`, the time range of the entire data is used for
#' the expansion of each group.
#' Depending on the analysis, this may or may not be what you want
#' and can sometimes result in huge expansions,
#' so generally it is recommended to
#' use a grouped_df or supply the additional groups through .`by`.
#'
#' Another important note is that when working with
#' many groups (>= 500k), period calculations
#' may be slow and so duration calculations are recommended in this case.
#' Numeric/duration expansions are
#' internally vectorised so can easily handle many groups.
#'
#' If `complete = TRUE` then the gaps in the time are expanded.
#' If `by` is supplied, then the time series is aggregated
#' to this specified level.
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
#' @param from Time series start date. If `NULL` then min time is used.
#' @param to Time series end date. If `NULL` then max time is used.
#' @param complete Logical. If `TRUE` implicit gaps in time are filled
#' after counting and time aggregation (through `time_by`).
#' The default is `TRUE`.
#' @param wt Frequency weights.
#' `dplyr` "data-masking" is used for variable selection.
#'   Can be `NULL` or a variable:
#'
#'   * If `NULL` (the default), counts the number of rows in each group.
#'   * If a variable, computes `sum(wt)` for each group.
#' @param sort If `TRUE` the groups with largest counts will be sorted first.
#' If `FALSE` the result is sorted by groups + time + ... groups.
#' @param name Character vector of length 1, specifying the name of
#' the new column in the output.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param time_floor Should `from` be floored to the nearest unit
#' specified through the `time_by`
#' argument? This is particularly useful for starting
#' sequences at the beginning of a week
#' or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `time_floor = TRUE`.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or
#' years are specified, and `durations`
#' are used otherwise.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param include_interval Logical. If `TRUE` then
#' a column "interval" of the form `time_min <= x < time_max` is added
#' showing the time interval in which the respective counts belong to.
#' The rightmost interval will always be closed.
#' @return An object of class `data.frame`
#' containing the input time variable
#' which is expanded for each supplied group.
#' When a grouped_df, or the .by argument is supplied, time expansions are done on a
#' group-by-group basis.
#'
#' The `time_by` argument controls the level at which the
#' time variable is expanded/aggregated.
#' If it is set to "1 month", then a seq of months is created, using the
#' `from` and `to` arguments, and every date/datetime is matched to the appropriate
#' intervals within that sequence.
#' Once they are matched, they are aggregated to the month
#' level.
#'
#' To avoid time expansion, simply set `complete = FALSE`,
#' which will simply perform a count across your time variable and specified groups.
#' The time aggregation through `time_by` works even when
#' `complete = FALSE` and even when `time_by` is more granular than the data.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' library(nycflights13)
#'
#' flights <- flights %>%
#'   mutate(date = as_date(time_hour),
#'          date_num = as.integer(date)) %>%
#'   select(year, month, day, origin, dest, date, time_hour, date_num)
#'
#' # By default time_count() guesses the time granularity
#' flights %>%
#'   time_count(time = time_hour)
#' # Aggregated to week level
#' flights %>%
#'   time_count(time = date, time_by = "2 weeks")
#' flights %>%
#'   time_count(time = date, time_by = list("months" = 3),
#'              from = dmy("15-01-2013"),
#'              time_floor = TRUE,
#'              include_interval = TRUE)
#' # By week using numbers
#' flights %>%
#'   time_expand(time = date_num, time_by = 7) %>%
#'   mutate(date = as_date(date_num))
#' @export
time_count <- function(data, time = NULL, ..., time_by = NULL,
                       from = NULL, to = NULL,
                       complete = TRUE,
                       wt = NULL, name = NULL,
                       sort = FALSE,
                       .by = NULL,
                       time_floor = FALSE,
                       week_start = getOption("lubridate.week.start", 1),
                       time_type = c("auto", "duration", "period"),
                       roll_month = "preday", roll_dst = "pre",
                       include_interval = FALSE){
  time_type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
  reconstruct <- TRUE
  ts_data <- mutate2(data,
                     ...,
                     !!enquo(time),
                     !!enquo(from),
                     !!enquo(to),
                     !!enquo(wt),
                     .by = {{ .by }},
                     .keep = "none")
  group_info <- get_group_info(data, ..., type = "data-mask",
                               .by = {{ .by }})
  # Transformed variable names
  # It is important to maintain the first-evaluated result of the expression,
  # As done above
  time_var <- tidy_transform_names(data, !!enquo(time))
  from_var <- tidy_transform_names(data, !!enquo(from))
  to_var <- tidy_transform_names(data, !!enquo(to))
  wt_var <- tidy_transform_names(data, !!enquo(wt))
  if (length(wt_var) > 0L) wtv <- ts_data[[wt_var]]
  group_vars <-  group_info[["dplyr_groups"]]
  extra_group_vars <- group_info[["extra_groups"]]
  all_group_vars <- group_info[["all_groups"]]
  N <- nrow2(ts_data)
  if (length(time_var) > 0){
    ts_data <- as_DT(ts_data)
    # Add variable to keep track of group IDs
    grp_nm <- new_var_nm(ts_data, ".group.id")
    ts_data[, (grp_nm) := group_id(data, .by = {{ .by }}, as_qg = TRUE)]
    n_groups <- attr(ts_data[[grp_nm]], "N.groups")
    # Determine common bounds
    from_nm <- new_var_nm(names(ts_data), ".from")
    to_nm <- new_var_nm(c(names(ts_data), from_nm), ".to")
    ts_data[, c(from_nm, to_nm) := get_from_to(ts_data, time = time_var,
                                               from = from_var,
                                               to = to_var,
                                               .by = all_of(grp_nm))]
    start_end_tbl <- fdistinct(ts_data, .cols = c(grp_nm, from_nm, to_nm))
    if (!isTRUE(nrow2(start_end_tbl) == n_groups)){
      warning("Multiple start-end values detected.
              Please supply one pair per group",
              immediate. = TRUE)
    }
    ts_data <- df_row_slice(ts_data,
                            data.table::between(ts_data[[time_var]],
                                                ts_data[[from_nm]],
                                                ts_data[[to_nm]],
                                                incbounds = TRUE, NAbounds = NA))
    # User supplied unit
    if (!is.null(time_by)){
      time_by <- time_by_list(time_by)
    } else {
      # Function to determine implicit time units
      granularity <- time_granularity(ts_data[[time_var]], is_sorted = FALSE, msg = TRUE)
      time_by <- add_names(list(granularity[["num"]]), granularity[["unit"]])
    }
    # Frequency table
    out <- ts_data %>%
      fcount(.cols = c(grp_nm, group_vars, time_var,
                       from_var, to_var,
                       extra_group_vars),
             wt = across(all_of(wt_var)),
             name = name)
    name <- names(out)[length(names(out))]
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
    int_end_nm <- character(0)
    time_int_end <- time_int_end(time_agg)
    out[, (time_var) := time_int_rm_attrs(time_agg)]
    if (include_interval){
      out[, ("int_end") := time_int_end]
      int_end_nm <- "int_end"
    }
    out <- fcount(out, .cols = c(grp_nm, group_vars,
                                 time_var, extra_group_vars, int_end_nm),
                  wt = across(all_of(name)),
                  name = name)
    # If complete, full-join time sequence df onto ts data
    if (complete){
      # message("\nFilling in implicit gaps in time, counts are replaced with 0
      #         to disable this set complete = FALSE")
      out[start_end_tbl, (c(from_nm, to_nm)) := mget(c(from_nm, to_nm)),
          on = grp_nm, allow.cartesian = FALSE]
      # Expanded time sequences for each group
      out <- time_complete(out,
                           time = across(all_of(time_var)),
                           across(all_of(extra_group_vars)),
                           from = across(all_of(from_nm)),
                           to = across(all_of(to_nm)),
                           time_by = time_by,
                           time_type = time_type,
                           sort = TRUE,
                           .by = all_of(c(grp_nm, group_vars)),
                           time_floor = time_floor, week_start = week_start,
                           keep_class = FALSE,
                           expand_type = "nesting",
                           fill = add_names(list(0L), name))
      set_rm_cols(out, c(from_nm, to_nm))
      if (include_interval){
        out[is.na(get(int_end_nm)) & !is.na(get(time_var)),
            (int_end_nm) := time_add2(get(time_var), time_by = time_by,
                                      roll_dst = roll_dst,
                                      roll_month = roll_month,
                                      time_type = time_type)]
      }
    }
    int_nm <- character(0)
    if (include_interval){
      out <- df_as_tibble(out)
      if (inherits(data, "data.table")){
        reconstruct <- FALSE
        message("data.table converted to tibble as data.table cannot include interval class")
      }

      int_nm <- new_var_nm(out, "interval")
      out[[int_nm]] <- time_interval(out[[time_var]],
                                     out[[int_end_nm]])
    }
    out <- fselect(out, .cols = c(grp_nm,
                                  group_vars,
                                  time_var,
                                  extra_group_vars,
                                  int_nm,
                                  name))
    if (sort){
      out <- farrange(out, desc(.data[[name]]))
    }
    out <- df_rm_cols(out, grp_nm) # Remove group ID
  } else {
    out <- ts_data %>%
      fcount(.cols = c(group_vars, extra_group_vars),
             wt = across(all_of(wt_var)),
             name = name,
             sort = sort)
    name <- names(out)[length(names(out))]
  }
  if (include_interval && !is_interval(out[[int_nm]])){
    attr(out[[int_nm]], "start") <- out[[time_var]]
  }
  if (reconstruct){
    out <- df_reconstruct(out, data)
  }
  out
}
