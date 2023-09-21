#' A time based extension to `dplyr::mutate()`.
#'
#' @description  This works much the same as `dplyr::mutate()`, except that
#' you can supply an additional `time` argument to allow for
#' aggregating time to a higher unit.

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
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' library(nycflights13)
#'
#' # Works the same way as mutate()
#' identical(flights %>%
#'             mutate(across(where(is.numeric), mean)),
#'           flights %>%
#'             time_mutate(time = NULL,
#'                         across(where(is.numeric), mean)))
#' # Like the other time_ functions, it allows for an additional time variable to
#' # aggregate by
#' flights %>%
#'   time_mutate(time = across(time_hour, as_date),
#'               time_by = "month", .keep = "none",
#'               include_interval = TRUE) %>%
#'   distinct()
#' @export
time_mutate <- function(data, time = NULL, ..., time_by = NULL,
                        from = NULL, to = NULL,
                        time_type = c("auto", "duration", "period"),
                        include_interval = FALSE,
                        .by = NULL,
                        .keep = c("all", "used", "unused", "none"),
                        time_floor = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        roll_month = "preday", roll_dst = "pre"){
  group_vars <- get_groups(data, {{ .by }})
  data <- mutate2(data,
                  !!enquo(time),
                  !!enquo(from),
                  !!enquo(to),
                  .by = {{ .by }})
  reconstruct <- TRUE
  time_var <- tidy_transform_names(data, !!enquo(time))
  from_var <- tidy_transform_names(data, !!enquo(from))
  to_var <- tidy_transform_names(data, !!enquo(to))
  # Add variable to keep track of group IDs
  grp_nm <- new_var_nm(data, ".group.id")
  data <- add_group_id(data, .by = {{ .by }}, .name = grp_nm)
  int_nm <- character(0)
  if (length(time_var) > 0L){
    # User supplied unit
    if (!is.null(time_by)){
      time_by <- time_by_list(time_by)
    } else {
      # Function to determine implicit time units
      granularity <- time_granularity(data[[time_var]], is_sorted = FALSE,
                                      msg = FALSE)
      message(paste("Assuming a time granularity of", granularity[["num"]]/granularity[["scale"]],
                    granularity[["granularity"]], sep = " "))
      time_by <- add_names(list(granularity[["num"]]), granularity[["unit"]])
    }
    # Aggregate time data
    time_agg <- time_aggregate_switch(data[[time_var]],
                                      time_by = time_by,
                                      g = data[[grp_nm]],
                                      start = fpluck(data, from_var),
                                      end = fpluck(data, to_var),
                                      time_type = time_type,
                                      roll_month = roll_month,
                                      roll_dst = roll_dst,
                                      time_floor = time_floor,
                                      week_start = week_start,
                                      as_int = include_interval)
    time_int_end <- time_int_end(time_agg)
    time_agg <- time_int_rm_attrs(time_agg)
    data <- dplyr::dplyr_col_modify(data,
                                    add_names(
                                      list(time_agg), time_var
                                    )
    )
      if (include_interval){
        if (inherits(data, "data.table")){
          data <- df_as_tibble(data)
          reconstruct <- FALSE
          message("data.table converted to tibble as data.table cannot include interval class")
        }
        int_nm <- new_var_nm(names(data), "interval")
        data <- dplyr::dplyr_col_modify(data,
                                        add_names(
                                          list(
                                            lubridate::interval(time_agg, time_int_end)
                                          ), int_nm
                                        )
        )
      }
  }
  data <- df_rm_cols(data, grp_nm)
  out <- mutate2(safe_ungroup(data),
                ...,
                .by = all_of(c(group_vars, time_var, int_nm)),
                .keep = .keep)
  if (reconstruct){
    out <- df_reconstruct(out, data)
  }
  out
}
