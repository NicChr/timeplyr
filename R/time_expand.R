#' A time based extension to `tidyr::complete()`.
#'
#' @param data A data frame.
#' @param time Time variable.
#' @param ... Groups to expand.
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
#' @param expand_type Type of time expansion to use where "nesting"
#' finds combinations already present in the data,
#' "crossing" finds all combinations of values in the group variables.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @param time_floor Should `from` be floored to the
#' nearest unit specified through the `time_by`
#' argument? This is particularly useful for
#' starting sequences at the beginning of a week or month for example.
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
#' @details
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
#' @returns
#' A `data.frame` of expanded time by or across groups.
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
#' x <- flights$time_hour
#'
#' time_num_gaps(x) # Missing hours
#'
#' flights_count <- flights %>%
#'   fcount(time_hour)
#'
#' # Fill in missing hours
#' flights_count %>%
#'   time_complete(time = time_hour)
#'
#' # You can specify units too
#' flights_count %>%
#'   time_complete(time = time_hour, time_by = "hours")
#' flights_count %>%
#'   time_complete(time = as_date(time_hour), time_by = "days") #  Nothing to complete here
#'
#' # Where time_expand() and time_complete() really shine is how fast they are with groups
#' flights %>%
#'   group_by(origin, dest) %>%
#'   time_expand(time = time_hour, time_by = dweeks(1))
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname time_expand
#' @export
time_expand <- function(data, time = NULL, ..., .by = NULL,
                        time_by = NULL, from = NULL, to = NULL,
                        time_type = getOption("timeplyr.time_type", "auto"),
                        time_floor = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        expand_type = c("nesting", "crossing"),
                        sort = TRUE,
                        keep_class = TRUE,
                        roll_month = getOption("timeplyr.roll_month", "preday"), roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                        log_limit = 8){
  check_is_df(data)
  expand_type <- rlang::arg_match(expand_type)
  group_vars <- get_groups(data, {{ .by }})
  temp_data <- data
  if (length(group_vars(data)) == 0){
    temp_data <- fgroup_by(temp_data, .by = {{ .by }}, order = FALSE)
  }
  group_ids <- df_group_id(temp_data)
  time_info <- mutate_summary_grouped(temp_data, !!enquo(time), .keep = "none")
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
  out <- df_cbind(time_data, from_data, to_data)
  out <- data.table::copy(collapse::qDT(out))
  if (length(time_var) > 0){
    time_type <- match_time_type(time_type)
    time_by <- time_by_get(out[[time_var]], time_by = time_by)
    by_unit <- names(time_by)
    by_n <- time_by[[1L]]
    input_time_type <- time_type # Save original
    # Ordered group ID
    grp_nm <- new_var_nm(out, ".group.id")
    data.table::set(out, j = grp_nm, value = group_ids)
    from_nm <- new_var_nm(names(out), ".from")
    to_nm <- new_var_nm(c(names(out), from_nm), ".to")
    out[, c(from_nm, to_nm) := get_from_to(out, time = time_var,
                                           from = from_var,
                                           to = to_var,
                                           .by = all_of(grp_nm))]
    # Unique groups
    time_tbl <- fdistinct(
      fselect(
        out, .cols = c(group_vars, grp_nm, from_nm, to_nm)
      ),
      .cols = grp_nm, .keep_all = TRUE
    )
    if (time_floor){
      data.table::set(time_tbl,
                      j = from_nm,
                      value = time_floor2(fpluck(time_tbl, from_nm),
                                          time_by, week_start = week_start))
    }
    # Reverse by sign in case from > to
    by_nm <- new_var_nm(out, ".by")
    data.table::set(time_tbl, j = by_nm, value = by_n)
    time_tbl[, (by_nm) := data.table::fifelse(get(from_nm) > get(to_nm),
                                              -1 * abs(get(by_nm)),
                                              get(by_nm))]
    # Determine size of sequences
    size_nm <- new_var_nm(out, ".size")
    time_tbl[, (size_nm) := time_seq_sizes(get(from_nm),
                                           get(to_nm),
                                           time_by = add_names(list(get(by_nm)),
                                                              by_unit),
                                           time_type = time_type)]
      expanded_nrow <- sum(time_tbl[[size_nm]])
      expand_check(expanded_nrow, log_limit)
      # Vectorised time sequence
      time_seq <- time_seq_v2(time_tbl[[size_nm]],
                              time_tbl[[from_nm]],
                              time_by = add_names(list(time_tbl[[by_nm]]),
                                                 by_unit),
                              roll_month = roll_month,
                              roll_dst = roll_dst,
                              time_type = time_type)
      time_seq_sizes <- time_tbl[[size_nm]]
      set_rm_cols(time_tbl, c(from_nm, to_nm,
                              size_nm,
                              by_nm))
      out <- df_rep(time_tbl, time_seq_sizes)
      data.table::set(out, j = time_var, value = time_seq)
      set_rm_cols(out, grp_nm)
      if (dots_length(...) > 0){
        expanded_df <- fexpand(data,
                               ...,
                               expand_type = expand_type,
                               keep_class = FALSE,
                               sort = FALSE, .by = {{ .by }},
                               log_limit = log_limit)
        expanded_nms <- names(expanded_df)
        if (df_nrow(expanded_df) > 0L){
          # If there are no common cols, just cross join them
          if (length(intersect(group_vars, expanded_nms)) == 0L){
            out_n <- df_nrow(out)
            expanded_n <- df_nrow(expanded_df)
            out <- df_rep_each(out, expanded_n)
            for (i in seq_along(expanded_nms)){
              out[, (expanded_nms[i]) := rep(expanded_df[[expanded_nms[i]]], out_n)]
            }
            # If data was grouped, we can do a full join on these variables
          } else {
            if (length(setdiff(expanded_nms, group_vars)) > 0L){
              out <- dplyr::full_join(out, expanded_df, by = group_vars,
                                      relationship = "many-to-many")
              # out <- collapse_join(out, expanded_df,
              #                      on = group_vars,
              #                      how = "full",
              #                      sort = FALSE)
            }
          }
        }
      }
    if (sort){
      sort_nms <- c(group_vars, time_var,
        setdiff(names(out),
                c(group_vars, time_var)))
      setorderv2(out, cols = sort_nms)
    }
  } else {
    out <- fexpand(data,
                   ...,
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
time_complete <- function(data, time = NULL, ..., .by = NULL,
                          time_by = NULL, from = NULL, to = NULL,
                          time_type = getOption("timeplyr.time_type", "auto"),
                          time_floor = FALSE,
                          week_start = getOption("lubridate.week.start", 1),
                          expand_type = c("nesting", "crossing"),
                          sort = TRUE,
                          keep_class = TRUE,
                          fill = NA,
                          roll_month = getOption("timeplyr.roll_month", "preday"),
                          roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                          log_limit = 8){
  check_is_df(data)
  expand_type <- rlang::arg_match(expand_type)
  time_type <- match_time_type(time_type)
  group_vars <- get_groups(data, {{ .by }})
  out <- data
  out_info <- mutate_summary_grouped(out, !!enquo(time), .by = {{ .by }})
  time_var <- out_info[["cols"]]
  check_length_lte(time_var, 1)
  out <- as_DT(out_info[["data"]])
  expanded_df <- time_expand(out,
                             ...,
                             time = across(all_of(time_var)),
                             time_by = time_by,
                             from = !!enquo(from),
                             to = !!enquo(to),
                             time_type = time_type,
                             time_floor = time_floor,
                             week_start = week_start,
                             sort = FALSE,
                             .by = all_of(group_vars),
                             keep_class = FALSE,
                             expand_type = expand_type,
                             roll_month = roll_month, roll_dst = roll_dst,
                             log_limit = log_limit)
  # Full-join
  if (df_nrow(expanded_df) > 0 && ncol(expanded_df) > 0){
    # Check to see if time has turned to POSIX
    if (length(time_var) > 0){
      out[, (time_var) := time_cast(get(time_var), expanded_df[[time_var]])]
    }
    out <- dplyr::full_join(out, expanded_df, by = names(expanded_df))
    # out <- collapse_join(out, expanded_df,
    #                      on = names(expanded_df),
    #                      how = "full",
    #                      sort = FALSE)
    if (sort){
      setorderv2(out, c(group_vars, time_var,
                        setdiff(names(expanded_df),
                                c(group_vars, time_var))))
      # out <- farrange(out, .cols = c(group_vars, time_var,
      #                                setdiff(names(expanded_df),
      #                                        c(group_vars, time_var))))
    }
  }
  # Replace NA with fill
  if (any(!is.na(fill))){
    fill <- fill[!is.na(fill)]
    fill_nms <- names(fill)
    for (i in seq_along(fill)){
      out[cpp_which(is.na(get(fill_nms[[i]]))),
          (fill_nms[[i]]) := fill[[i]]]
    }
  }
  out_vars <- c(names(data), setdiff(names(out), names(data)))
  out <- fselect(out, .cols = out_vars)
  if (keep_class){
    out <- df_reconstruct(out, data)
  }
  out
}
