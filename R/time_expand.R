#' A time based extension to `tidyr::complete()`.
#'
#' This works much the same as `tidyr::complete()`, except that
#' you can supply an additional `time` argument to allow for filling in time gaps,
#' expansion of time, as well as aggregating time to a higher unit.
#' `lubridate` is used for handling time, while `data.table` and `collapse` are used for
#' the data frame expansion.
#'
#' At the moment, within group combinations are ignored. This means when `expand_type = nesting`,
#' existing combinations of supplied groups across the entire dataset are used, and
#' when `expand_type = crossing`, all possible combinations of supplied groups across the \bold{entire}
#' dataset are used as well.
#'
#'
#' @param data A data frame.
#' @param ... Groups to expand.
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
#' @param expand_type Type of time expansion to use where "nesting" finds combinations already present in the data,
#' "crossing" finds all combinations of values in the group variables.
#' @param seq_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified, and `durations`
#' are used otherwise.
#' @param floor_date Should `from` be floored to the nearest unit specified through the `by`
#' argument? This is particularly useful for starting sequences at the beginning of a week
#' or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `floor_date = TRUE`.
#' @param sort Logical. If `TRUE` expanded/completed variables are sorted.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param keep_class Logical. If `TRUE` then the class of the input data is retained.
#' If `FALSE`, which is sometimes faster, a `data.table` is returned.
#' @param fill A named list containing value-name pairs to fill the named implicit missing values.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param log_limit The maximum log10 number of rows that can be expanded.
#' Anything exceeding this will throw an error.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' library(nycflights13)
#'
#' data(flights)
#' x <- flights$time_hour
#'
#' length(time_missing(x)) # Missing hours
#' length(missing_dates(x)) # No missing dates though
#' x_filled <- time_completev(x) # Expand by hour through heuristic
#' time_missing(x_filled, by  = "hour") # No missing hours
#'
#' # Easier through tidyverse style functions
#'
#' flights_count <- flights %>%
#'   fcount(time_hour)
#'
#' flights_count %>%
#'   time_complete(time = time_hour)
#'
#' # You can specify units too
#' flights_count %>%
#'   time_complete(time = time_hour, by = "hour")
#' flights_count %>%
#'   time_complete(time = as_date(time_hour), by = "day") #  Nothing to complete here
#'
#' # Where time_expand() and time_complete() really shine is how fast they are with groups
#' flights %>%
#'   group_by(origin, dest, tailnum) %>%
#'   time_expand(time = time_hour, by = "week",
#'               seq_type = "duration")
#' @rdname time_expand
#' @export
time_expand <- function(data, ..., time = NULL, by = NULL, from = NULL, to = NULL,
                        .by = NULL,
                        seq_type = c("auto", "duration", "period"),
                        floor_date = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        expand_type = c("nesting", "crossing"),
                        sort = TRUE,
                        keep_class = TRUE,
                        roll_month = "preday", roll_dst = "pre",
                        log_limit = 8){
  expand_type <- match.arg(expand_type)
  group_vars <- get_groups(data, {{ .by }})
  out <- dplyr::mutate(data,
                       !!enquo(time),
                       !!enquo(from),
                       !!enquo(to),
                       .by = {{ .by }},
                       .keep = "none")
  time_var <- tidy_transform_names(data, !!enquo(time))
  from_var <- tidy_transform_names(data, !!enquo(from))
  to_var <- tidy_transform_names(data, !!enquo(to))
  out <- data.table::copy(out)
  data.table::setDT(out)
  if (length(time_var) > 0){
    seq_type <- match.arg(seq_type)
    if (is.null(by)){
      unit_info <- time_granularity(out[[time_var]], is_sorted = FALSE)
      by_n <- unit_info[["num"]]
      by_unit <- unit_info[["unit"]]
    } else {
      unit_info <- unit_guess(by)
      by_n <- unit_info[["num"]] * unit_info[["scale"]]
      by_unit <- unit_info[["unit"]]
    }
    input_seq_type <- seq_type # Save original
    # Ordered group ID
    grp_nm <- new_var_nm(out, ".group.id")
    out[, (grp_nm) := group_id(data, .by = {{ .by }})]
    from_nm <- new_var_nm(names(out), ".from")
    to_nm <- new_var_nm(c(names(out), from_nm), ".to")
    out[, c(from_nm, to_nm) := get_from_to(out, time = all_of(time_var),
                                               from = all_of(from_var),
                                               to = all_of(to_var),
                                               .by = all_of(grp_nm))]
    # if (length(from_var) == 0L){
    #   out[, (from_nm) := gmin(get(time_var),
    #                           g = get(grp_nm))]
    # } else {
    #   out[, (from_nm) := time_cast(get(from_var), get(time_var))]
    # }
    # if (length(to_var) == 0L){
    #   out[, (to_nm) := gmax(get(time_var),
    #                           g = get(grp_nm))]
    # } else {
    #   out[, (to_nm) := time_cast(get(to_var), get(time_var))]
    # }
    # Unique groups
    time_tbl <- collapse::funique(collapse::fselect(out, c(group_vars, grp_nm, from_nm, to_nm)),
                                  cols = grp_nm)
    # Bit-hacky.. probably better to add a floor_date arg to time_seq_len
    if (floor_date){
      if (is_time(out[[time_var]])){
        time_tbl[, (from_nm) := time_floor(get(from_nm),
                                           by_unit,
                                           week_start = week_start)]
      } else {
        time_tbl[, (from_nm) := time_floor(get(from_nm),
                                           by_n,
                                           week_start = week_start)]
      }
    }
    # Reverse by sign in case from > to
    by_nm <- new_var_nm(out, ".by")
    time_tbl[, (by_nm) := by_n]
    time_tbl[, (by_nm) := data.table::fifelse(get(from_nm) > get(to_nm),
                                              -1 * abs(get(by_nm)),
                                              get(by_nm))]
    # Determine size of sequences
    size_nm <- new_var_nm(out, ".size")

    ### Special case where day expansion is used on days/UTC ###
    # With period calculation. This should use durations for speed
    unit_is_days <- by_unit %in% c("days", "weeks")
    unit_is_less_than_days <- by_unit %in% c(setdiff(.duration_units, .period_units),
                                           "seconds")
    ### Special cases where units are days/utc and ranges are also days/utc
    # In these cases we can just use duration calculations which are much faster
    is_special_case_utc <- input_seq_type == "auto" &&
      (unit_is_days || unit_is_less_than_days) &&
      is_datetime(out[[time_var]]) &&
      lubridate::tz(out[[time_var]]) == "UTC" &&
      lubridate::tz(time_tbl[[from_nm]]) == "UTC" &&
      lubridate::tz(time_tbl[[to_nm]]) == "UTC"
    if (is_special_case_utc) seq_type <- "duration"
      time_tbl[, (size_nm) := time_seq_len(get(from_nm),
                                               get(to_nm),
                                               by = setnames(list(get(by_nm)),
                                                             by_unit),
                                               seq_type = seq_type)]
      expanded_nrow <- sum(time_tbl[[size_nm]])
      expand_check(expanded_nrow, log_limit)
      # Vectorised time sequence
      time_seq <- time_seq_v(time_tbl[[from_nm]],
                             time_tbl[[to_nm]],
                             by = setnames(list(time_tbl[[by_nm]]),
                                           by_unit),
                             roll_month = roll_month,
                             roll_dst = roll_dst,
                             seq_type = seq_type)
      out <- vctrs::vec_rep_each(time_tbl, time_tbl[[size_nm]])
      data.table::setDT(out)
      out[, (time_var) := time_seq]
      expanded_df <- fexpand(data,
                             !!!enquos(...),
                             expand_type = expand_type,
                             keep_class = FALSE,
                             sort = FALSE, .by = {{ .by }},
                             log_limit = log_limit)
      expanded_nms <- names(expanded_df)
      # Join our time sequence (table) with our non-grouped expanded table
      out[, c(grp_nm, from_nm, to_nm,
              size_nm,
              by_nm) := NULL]
    if (nrow2(expanded_df) > 0L){
      # If there are no common cols, just cross join them
      if (length(intersect(group_vars, expanded_nms)) == 0L){
        out_n <- nrow2(out)
        expanded_n <- nrow2(expanded_df)
        out <- df_row_slice(out, rep(seq_len(out_n), each = expanded_n))
        for (i in seq_len(length(expanded_nms))){
          out[, (expanded_nms[i]) := rep(expanded_df[[expanded_nms[i]]], out_n)][]
        }
        # If data was grouped, we can do a full join on these variables
      } else {
        if (length(setdiff(expanded_nms, group_vars)) > 0L){
          out <- merge(out, expanded_df,
                       all = TRUE, by = group_vars, sort = FALSE,
                       allow.cartesian = TRUE)
        }
      }
    }
    if (sort){
      sort_nms <- c(group_vars, time_var,
        setdiff(names(out),
                c(group_vars, time_var)))
      data.table::setorderv(out, cols = sort_nms, na.last = TRUE)
    }
  } else {
    out <- fexpand(data,
                   !!!enquos(...),
                   expand_type = expand_type,
                   keep_class = keep_class,
                   sort = sort, .by = {{ .by }},
                   log_limit = log_limit)
    keep_class <- FALSE
  }
  if (keep_class){
    out <- df_reconstruct(out, data)
  }
  out
}
#' @rdname time_expand
#' @export
time_complete <- function(data, ..., time = NULL, by = NULL, from = NULL, to = NULL,
                          seq_type = c("auto", "duration", "period"),
                          floor_date = FALSE,
                          week_start = getOption("lubridate.week.start", 1),
                          expand_type = c("nesting", "crossing"),
                          sort = TRUE, .by = NULL,
                          keep_class = TRUE,
                          fill = NA,
                          roll_month = "preday", roll_dst = "pre",
                          log_limit = 8){
  expand_type <- match.arg(expand_type)
  seq_type <- match.arg(seq_type)
  group_vars <- get_groups(data, {{ .by }})
  out <- dplyr::mutate(data, !!enquo(time))
  time_var <- tidy_transform_names(data, !!enquo(time))
  out <- collapse::qDT(out[TRUE])
  expanded_df <- time_expand(out,
                           !!!enquos(...),
                           time = across(all_of(time_var)),
                           by = by, from = !!enquo(from),
                           to = !!enquo(to),
                           seq_type = seq_type,
                           floor_date = floor_date,
                           week_start = week_start,
                           sort = FALSE,
                           .by = all_of(group_vars),
                           keep_class = FALSE,
                           expand_type = expand_type,
                           roll_month = roll_month, roll_dst = roll_dst,
                           log_limit = log_limit)
  # Full-join
  if (nrow2(expanded_df) > 0 && ncol(expanded_df) > 0){
    # Check to see if time has turned to POSIX
    if (length(time_var) > 0){
      out[, (time_var) := time_cast(get(time_var), expanded_df[[time_var]])]
    }
    out <- merge(out, expanded_df,
                 all = TRUE, by = names(expanded_df), sort = FALSE)
    if (sort){
      data.table::setorderv(out, cols = c(group_vars, time_var,
                                          setdiff(names(expanded_df),
                                                  c(group_vars, time_var))),
                            na.last = TRUE)
    }
  }
  # Replace NA with fill
  if (any(!is.na(fill))){
    fill <- fill[!is.na(fill)]
    fill_nms <- names(fill)
    for (i in seq_along(fill)){
      out[, (fill_nms[[i]]) := data.table::fifelse(is.na(get(fill_nms[[i]])),
                                                   fill[[i]],
                                                   get(fill_nms[[i]]))]
      # data.table::setnafill(out, cols = fill_nms[i], type = "const", fill = fill[[i]], nan = NaN)
    }
  }
  out_vars <- c(names(data), setdiff(names(out), names(data)))
  if (length(out_vars) > 0L){
    data.table::setcolorder(out, neworder = out_vars)
  }
  if (keep_class){
    out <- df_reconstruct(out, data)
  }
  out
}
