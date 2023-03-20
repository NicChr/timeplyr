#' A time based extension to `dplyr::mutate()`.
#'
#' @description Unlike the other `time_` functions,
#' `time_summarise()` and `time_summarisev()` do \bold{not} fill
#' missing gaps in time. They only summarise dates and datetimes to
#' higher levels of aggregation.
#' This works much the same as `dplyr::mutate()`, except that
#' you can supply an additional `time` argument to allow for
#' aggregating time to a higher unit.
#' Every expression is then grouped by your supplied groups + time.
#' E.g., if you supply a date and specify `by = "week"`
#' then time is aggregated up to the week level and then every expression is grouped by week.
#' All `time_` functions build date/datetime sequences from the time start of each group by default.
#'
#' Unlike the other `time_` functions, this does \bold{not} support
#' filling in missing gaps in time. Use `time_count()` or `time_complete()`
#' before using this if you believe there may be gaps in time.
#' A good example of why this function might be useful can be seen when
#' computing summary statistics when there are missing gaps in time.
#' If you calculated the average number of hourly flights in
#' the flights dataset, you might do `flights %>% count(time_hour) %>%
#' summarise(avg = mean(n))` which would yield an answer of `48.6`
#' If you instead did
#' `flights %>% time_count(time = time_hour) %>% summarise(avg = mean(n))`
#' Or equivalently
#' `flights %>% time_count(time = time_hour) %>% time_summarise(avg = mean(n), time = time_hour, by = "year")`
#' Or also
#' `flights %>% fcount(time_hour) %>% time_complete(time = time_hour, fill = list(n = 0)) %>% summarise(avg = mean(n))`
#' You wold get an answer of 38.5. Why the difference?
#' The first answers the question: What was the average number of
#' flights in any hour, for hours where there flights.
#' The second answers our original question.
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
#' @param .keep Control which columns are retained. See `?dplyr::mutate` for more details.
#' @param keep_class Logical. If `TRUE` then the class of the input data is retained.
#' If `FALSE`, which is sometimes faster, a `data.table` is returned.
#' @param floor_date Should `from` be floored to the nearest unit specified through the `by`
#' argument? This is particularly useful for starting sequences at the beginning of a week
#' or month for example.
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
#'             time_mutate(across(where(is.numeric), mean)))
#' # Like the other time_ functions, it allows for an additional time variable to
#' # aggregate by
#' flights %>%
#'   time_mutate(time = across(time_hour, as_date),
#'               by = "month", .keep = "none",
#'               include_interval = TRUE) %>%
#'   distinct()
#' @export
time_mutate <- function(data, ..., time = NULL, by = NULL,
                        from = NULL, to = NULL,
                        seq_type = c("auto", "duration", "period"),
                        include_interval = FALSE,
                        .by = NULL,
                        .keep = c("all", "used", "unused", "none"),
                        keep_class = TRUE,
                        floor_date = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        roll_month = "preday", roll_dst = "pre",
                        sort = FALSE){
  seq_type <- match.arg(seq_type)
  group_vars <- get_groups(data, {{ .by }})
  out <- dplyr::mutate(data,
                       !!enquo(time),
                       !!enquo(from),
                       !!enquo(to),
                       .by = {{ .by }})
  time_var <- tidy_transform_names(safe_ungroup(data), !!enquo(time))
  from_var <- tidy_transform_names(safe_ungroup(data), !!enquo(from))
  to_var <- tidy_transform_names(safe_ungroup(data), !!enquo(to))
  out <- data.table::copy(out)
  data.table::setDT(out)
  # Add variable to keep track of original order
  sort_nm <- new_var_nm(out, ".sort.index")
  out[, (sort_nm) := seq_len(.N)]
  # Add variable to keep track of group IDs
  grp_nm <- new_var_nm(out, ".group.id")
  out[, (grp_nm) := group_id(data, .by = {{ .by }},
                             sort = TRUE, as_qg = FALSE)]
  int_nm <- new_var_nm(out, "interval")
  data.table::setorderv(out, cols = c(grp_nm, time_var))
  if (length(time_var) > 0L){
    # Expanded time sequences for each group
    time_expanded <- out %>%
      time_expand(time = across(all_of(time_var)),
                  from = across(all_of(from_var)),
                  to = across(all_of(to_var)),
                  by = by,
                  seq_type = seq_type,
                  sort = TRUE, .by = all_of(c(grp_nm, group_vars)),
                  floor_date = floor_date, week_start = week_start,
                  keep_class = FALSE,
                  expand_type = "nesting") # Irrelevant in this context
    out[, (time_var) := time_cast(get(time_var), time_expanded[[time_var]])]
    time_agg_df <- time_agg(time_expanded, out, time = time_var, group_id = grp_nm,
                            include_interval = include_interval, to = to_var)
    out[, (time_var) := time_agg_df[[time_var]]]
    int_nm <- character(0)
    if (include_interval){
      if (!keep_class){
        message("data.table converted to tibble as data.table cannot include interval class")
      }
      int_nm <- new_var_nm(out, "interval")
      out <- dplyr::as_tibble(out)
      out[[int_nm]] <- time_agg_df[["interval"]]
    }
  }
  # Sorting
  if (include_interval){
    if (!sort) out <- dplyr::arrange(out, .data[[sort_nm]])
    out <- dplyr::select(out, -all_of(c(grp_nm, sort_nm)))
  } else {
    if (!sort) data.table::setorderv(out, cols = sort_nm)
    out[, (c(grp_nm, sort_nm)) := NULL]
  }
  out <- dplyr::mutate(out,
                       !!!enquos(...),
                       .by = dplyr::any_of(c(group_vars, time_var, int_nm)),
                       .keep = .keep)
  if (keep_class) out <- df_reconstruct(out, data)
  out
}
