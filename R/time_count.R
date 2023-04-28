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
#' @param from Time series start date. If `NULL` then min time is used.
#' @param to Time series end date. If `NULL` then max time is used.
#' @param complete Logical. If `TRUE` implicit gaps in time are filled
#' after counting and before time aggregation (through `by`).
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
#' @param floor_date Should `from` be floored to the nearest unit
#' specified through the `by`
#' argument? This is particularly useful for starting
#' sequences at the beginning of a week
#' or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `floor_date = TRUE`.
#' @param seq_type If "auto", `periods` are used for
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
#' @param keep_class Logical. If `TRUE` then the class of
#' the input data is retained.
#' If `FALSE`, which is sometimes faster, a `data.table` is returned.
#'
#' @return An object of class `data.frame`
#' containing the input time variable
#' which is expanded for each supplied group.
#' When a grouped_df, or the .by argument is supplied, time expansions are done on a
#' group-by-group basis.
#'
#' The `by` argument controls the level at which the time variable is expanded/aggregated.
#' If it is set to "1 month", then a seq of months is created, using the
#' `from` and `to` arguments, and every date/datetime is matched to the appropriate
#' intervals within that sequence. Once they are matched, they are aggregated to the month
#' level.
#'
#' To avoid time expansion, simply set `complete = FALSE`,
#' which will simply perform a count across your time variable and specified groups.
#' The time aggregation through `by` works even when `complete = FALSE` and even when
#' `by` is more granular than the data.
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
#'   time_count(time = date, by = "2 weeks")
#' flights %>%
#'   time_count(time = date, by = list("months" = 3),
#'              from = dmy("15-01-2013"),
#'              floor_date = TRUE,
#'              include_interval = TRUE)
#' # By week using numbers
#' flights %>%
#'   time_expand(time = date_num, by = 7) %>%
#'   mutate(date = as_date(date_num))
#' @export
time_count <- function(data, ..., time = NULL, by = NULL,
                       from = NULL, to = NULL,
                       complete = TRUE,
                       wt = NULL, name = NULL,
                       sort = FALSE,
                       .by = NULL,
                       floor_date = FALSE,
                       week_start = getOption("lubridate.week.start", 1),
                       seq_type = c("auto", "duration", "period"),
                       roll_month = "preday", roll_dst = "pre",
                       include_interval = FALSE,
                       keep_class = TRUE){
  seq_type <- match.arg(seq_type)
  ts_data <- dplyr::mutate(data,
                           !!!enquos(...),
                           !!enquo(time),
                           !!enquo(from),
                           !!enquo(to),
                           !!enquo(wt),
                           .by = {{ .by }},
                           .keep = "none")
  group_info <- get_group_info(data, !!!enquos(...), type = "data-mask",
                               .by = {{ .by }})
  # Transformed variable names
  # It is important to maintain the first-evaluated result of the expression,
  # As done above
  time_var <- tidy_transform_names(safe_ungroup(data), !!enquo(time))
  from_var <- tidy_transform_names(data, !!enquo(from))
  to_var <- tidy_transform_names(data, !!enquo(to))
  wt_var <- tidy_transform_names(safe_ungroup(data), !!enquo(wt))
  if (length(wt_var) > 0L) wtv <- ts_data[[wt_var]]
  group_vars <-  group_info[["dplyr_groups"]]
  extra_group_vars <- group_info[["extra_groups"]]
  all_group_vars <- group_info[["all_groups"]]
  N <- nrow2(ts_data)
  if (length(time_var) > 0){
    ts_data <- data.table::copy(ts_data)
    data.table::setDT(ts_data)
    # Add variable to keep track of group IDs
    grp_nm <- new_var_nm(ts_data, ".group.id")
    ts_data[, (grp_nm) := group_id(data, order = TRUE,
                                   as_qg = FALSE,
                                   .by = {{ .by }})]
    # Determine common bounds
    from_nm <- new_var_nm(names(ts_data), ".from")
    to_nm <- new_var_nm(c(names(ts_data), from_nm), ".to")
    ts_data[, c(from_nm, to_nm) := get_from_to(ts_data, time = all_of(time_var),
                                               from = all_of(from_var),
                                               to = all_of(to_var),
                                               .by = all_of(grp_nm))]
    # Order by group vars - time var - additional group vars
    data.table::setorderv(ts_data, cols = c(grp_nm, time_var, extra_group_vars))
    ts_data <- ts_data[data.table::between(get(time_var), get(from_nm), get(to_nm),
                                           incbounds = TRUE, NAbounds = NA), ]
    # Function to determine implicit time units
    granularity <- time_granularity(ts_data[[time_var]], is_sorted = FALSE,
                                    msg = FALSE)
    # User supplied unit
    if (!is.null(by)){
      unit_info <- unit_guess(by)
      by_n <- unit_info[["num"]] * unit_info[["scale"]]
      by_unit <- unit_info[["unit"]]
    } else {
      message(paste("Assuming a time granularity of", granularity[["num"]]/granularity[["scale"]],
                    granularity[["granularity"]], sep = " "))
      by <- granularity[["num_and_unit"]]
      by_n <- granularity[["num"]]
      by_unit <- granularity[["unit"]]
    }
    # This checks if time aggregation is necessary
    seq_by <- setnames(list(by_n), by_unit)
    aggregate <- needs_aggregation(by = seq_by,
                                   granularity = setnames(list(
                                     granularity[["num"]]
                                   ),
                                   granularity[["unit"]]))
    if (aggregate || include_interval || complete){
      # Expanded time sequences for each group
      time_expanded <- ts_data %>%
        time_expand(across(all_of(extra_group_vars)),
                    time = across(all_of(time_var)),
                    from = across(all_of(from_nm)),
                    to = across(all_of(to_nm)),
                    by = seq_by,
                    seq_type = seq_type,
                    sort = TRUE, .by = all_of(c(grp_nm, group_vars)),
                    floor_date = floor_date, week_start = week_start,
                    keep_class = FALSE,
                    expand_type = "nesting")
      # Cast time
      ts_data[, (time_var) := time_cast(get(time_var),
                                        time_expanded[[time_var]])]
      # Save non-aggregate time data for possible interval calculation
      time <- ts_data[[time_var]]
      if (aggregate){
        # Aggregate time using the time sequence data
        ts_data[, (time_var) := taggregate(ts_data[[time_var]],
                                           time_expanded[[time_var]],
                                           gx = ts_data[[grp_nm]],
                                           gseq = time_expanded[[grp_nm]])]
      }
    }
    # Frequency table
    out <- ts_data %>%
      fcount(across(dplyr::any_of(c(grp_nm, group_vars, time_var,
                                    extra_group_vars))),
             wt = across(all_of(wt_var)),
             name = name)
    name <- names(out)[length(names(out))]

    # If complete, full-join time sequence df onto ts data
    if (complete){
      out <- merge(out, time_expanded,
                   all = TRUE, by = names(time_expanded), sort = FALSE)
      # Order by groups and time (ascending)
      data.table::setorderv(out, cols = c(grp_nm, time_var, extra_group_vars), na.last = TRUE)
      # Replace NA with 0 as these are counts
      data.table::setnafill(out, cols = name, type = "const", fill = 0, nan = NaN)
    }
    if (include_interval){
      out <- dplyr::as_tibble(out)
      message("data.table converted to tibble as data.table cannot include interval class")

      int_nm <- new_var_nm(out, "interval")
      out[[int_nm]] <- tseq_interval(time, time_expanded[[time_var]],
                                     gx = ts_data[[grp_nm]],
                                     gseq = time_expanded[[grp_nm]])
    } else {
     int_nm <- character(0)
    }
    out <- collapse::fselect(out, c(grp_nm,
                                    group_vars,
                                    time_var,
                                    extra_group_vars,
                                    int_nm,
                                    name))
    if (sort){
      if (include_interval){
        out <- df_row_slice(out, radix_order(desc(out[[name]])), reconstruct = FALSE)
      } else {
        data.table::setorderv(out, cols = name, order = -1L, na.last = TRUE)
      }
    }
    out[[grp_nm]] <- NULL # Remove group ID
  } else {
    out <- ts_data %>%
      fcount(across(dplyr::any_of(c(group_vars, extra_group_vars))),
             wt = across(all_of(wt_var)),
             name = name,
             sort = sort)
    name <- names(out)[length(names(out))]
  }
  if (keep_class){
    df_reconstruct(out, data)
  } else {
    out
  }
}
