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
#' @param sort Should the result be sorted? Default is `FALSE` and original (input)
#' order is kept. The sorting only applies to groups and time variable.
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
                        roll_month = "preday", roll_dst = "pre",
                        sort = FALSE){
  group_vars <- get_groups(data, {{ .by }})
  data <- mutate2(data,
                  !!enquo(time),
                  !!enquo(from),
                  !!enquo(to),
                  .by = {{ .by }})
  time_var <- tidy_transform_names(data, !!enquo(time))
  from_var <- tidy_transform_names(data, !!enquo(from))
  to_var <- tidy_transform_names(data, !!enquo(to))
  # Add variable to keep track of original order
  sort_nm <- new_var_nm(data, ".sort.index")
  data <- add_row_id(data, .name = sort_nm)
  # Add variable to keep track of group IDs
  grp_nm <- new_var_nm(data, ".group.id")
  data <- add_group_id(data, .by = {{ .by }}, .name = grp_nm)
  data <- farrange(data, .cols = c(grp_nm, time_var))
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
      time_by <- setnames(list(granularity[["num"]]), granularity[["unit"]])
    }
      # Expanded time sequences for each group
      time_expanded <- data %>%
        safe_ungroup() %>%
        time_expand(time = across(all_of(time_var)),
                    from = across(all_of(from_var)),
                    to = across(all_of(to_var)),
                    time_by = time_by,
                    time_type = time_type,
                    sort = TRUE, .by = all_of(c(grp_nm, group_vars)),
                    time_floor = time_floor, week_start = week_start,
                    keep_class = TRUE,
                    expand_type = "nesting") # Irrelevant in this context
      data <- dplyr::dplyr_col_modify(data, setnames(
        list(
          time_cast(data[[time_var]], time_expanded[[time_var]])
        ), time_var)
      )
      time <- data[[time_var]]
      # Aggregate time using the time sequence data
      data <- dplyr::dplyr_col_modify(data, setnames(
        list(
          taggregate(data[[time_var]],
                     time_expanded[[time_var]],
                     gx = data[[grp_nm]],
                     gseq = time_expanded[[grp_nm]])
        ), time_var)
      )
      if (include_interval){
        if (inherits(data, "data.table")){
          data <- list_to_tibble(as.list(data))
          message("data.table converted to tibble as data.table cannot include interval class")
        }
        int_nm <- new_var_nm(names(data), "interval")
        data[[int_nm]] <- tagg_interval(x = time,
                                        xagg = data[[time_var]],
                                        seq = time_expanded[[time_var]],
                                        gagg = data[[grp_nm]],
                                        gx = data[[grp_nm]],
                                        gseq = time_expanded[[grp_nm]])
      }
  }
  if (!sort){
    data <- farrange(data, .cols = sort_nm)
  }
  data <- df_rm_cols(data, c(grp_nm, sort_nm))
  out <- mutate2(safe_ungroup(data),
                ...,
                .by = all_of(c(group_vars, time_var, int_nm)),
                .keep = .keep)
  df_reconstruct(out, data)
}
