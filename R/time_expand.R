#' A time based extension to `tidyr::complete()`.
#'
#' @param data A data frame.
#' @param time Time variable.
#' @param ... Groups to expand.
#' @param time_by A [timespan].
#' @param from Time series start date.
#' @param to Time series end date.
#' @param sort Logical. If `TRUE` expanded/completed variables are sorted.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param fill A named list containing value-name pairs to fill
#' the named implicit missing values.
#'
#' @details
#' This works much the same as `tidyr::complete()`, except that
#' you can supply an additional `time` argument to allow for
#' completing implicit time gaps and creating time sequences by group.
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
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' x <- flights$time_hour
#'
#' time_num_gaps(x) # Missing hours
#'
#' flights_count <- flights |>
#'   fastplyr::f_count(time_hour)
#'
#' # Fill in missing hours
#' flights_count |>
#'   time_complete(time = time_hour)
#'
#' # You can specify units too
#' flights_count |>
#'   time_complete(time = time_hour, time_by = "hours")
#' flights_count |>
#'   time_complete(time = as_date(time_hour), time_by = "days") #  Nothing to complete here
#'
#' # Where time_expand() and time_complete() really shine is how fast they are with groups
#' flights |>
#'   group_by(origin, dest) |>
#'   time_expand(time = time_hour, time_by = dweeks(1))
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname time_expand
#' @export
time_expand <- function(data, time = NULL, ..., .by = NULL,
                        time_by = NULL, from = NULL, to = NULL,
                        sort = TRUE){
  check_is_df(data)
  group_vars <- get_groups(data, {{ .by }})
  temp_data <- data
  if (length(fastplyr::f_group_vars(data)) == 0){
    temp_data <- fastplyr::f_group_by(temp_data, .by = {{ .by }}, .order = FALSE)
  }
  group_ids <- df_group_id(temp_data)
  time_data <- mutate_one(temp_data, !!enquo(time))
  from_data <- mutate_one(temp_data, !!enquo(from))
  to_data <- mutate_one(temp_data, !!enquo(to))
  time_var <- names(time_data)
  from_var <- names(from_data)
  to_var <- names(to_data)
  check_length_lte(time_var, 1)
  check_length_lte(from_var, 1)
  check_length_lte(to_var, 1)

  out <- fastplyr::f_bind_cols(
    fastplyr::f_select(temp_data, .cols = group_vars),
    time_data, from_data, to_data
  )
  if (length(time_var) > 0){
    time_by <- get_granularity(out[[time_var]], time_by)
    by_unit <- timespan_unit(time_by)
    by_n <- timespan_num(time_by)
    # Ordered group ID
    grp_nm <- unique_col_name(out, ".group.id")
    out[[grp_nm]] <- group_ids
    from_nm <- unique_col_name(names(out), ".from")
    to_nm <- unique_col_name(c(names(out), from_nm), ".to")
    from_to_list <- get_from_to(out, time = time_var,
                                from = from_var,
                                to = to_var,
                                .by = all_of(grp_nm))
    out[[from_nm]] <- from_to_list[[1]]
    out[[to_nm]] <- from_to_list[[2]]
    # Unique groups
    time_tbl <- fastplyr::f_distinct(
      fastplyr::f_select(
        out, .cols = c(group_vars, grp_nm, from_nm, to_nm)
      ),
      .cols = grp_nm, .keep_all = TRUE
    )
    # Reverse by sign in case from > to
    by_nm <- unique_col_name(out, ".by")
    by_n <- cheapr::cheapr_rep_len(by_n, nrow(time_tbl))
    which_wrong_sign <- which(time_tbl[[from_nm]] > time_tbl[[to_nm]])
    by_n[which_wrong_sign] <- -abs(by_n[which_wrong_sign])
    time_tbl[[by_nm]] <- by_n
    # Determine size of sequences
    size_nm <- unique_col_name(out, ".size")
    time_tbl[[size_nm]] <- time_seq_sizes(time_tbl[[from_nm]],
                                          time_tbl[[to_nm]],
                                          timespan(by_unit, time_tbl[[by_nm]]))
    # Vectorised time sequence
    time_seq <- time_seq_v2(time_tbl[[size_nm]],
                            time_tbl[[from_nm]],
                            timespan(by_unit, time_tbl[[by_nm]]))
    time_seq_sizes <- time_tbl[[size_nm]]
    time_tbl <- df_rm_cols(time_tbl, c(from_nm, to_nm, size_nm, by_nm))
    out <- cheapr::cheapr_rep(time_tbl, time_seq_sizes)
    out[[time_var]] <- time_seq
    out <- df_rm_cols(out, grp_nm)
    if (dots_length(...) > 0){
        expanded_df <- fastplyr::f_expand(data, ...,
                                          .sort = FALSE, .by = {{ .by }})
      expanded_nms <- names(expanded_df)
      if (df_nrow(expanded_df) > 0L){
        # If there are no common cols, just cross join them
        if (length(intersect(group_vars, expanded_nms)) == 0L){
          out_n <- df_nrow(out)
          expanded_n <- df_nrow(expanded_df)
          out <- cheapr::cheapr_rep_each(out, expanded_n)
          for (i in seq_along(expanded_nms)){
            out[[expanded_nms[i]]] <- rep(expanded_df[[expanded_nms[i]]], out_n)
          }
          # If data was grouped, we can do a full join on these variables
        } else {
          if (length(setdiff(expanded_nms, group_vars)) > 0L){
            out <- fastplyr::f_full_join(out, expanded_df, by = group_vars)
          }
        }
      }
    }
    if (sort){
      sort_nms <- c(group_vars, time_var,
                    setdiff(names(out),
                            c(group_vars, time_var)))
      out <- fastplyr::f_arrange(out, .cols = sort_nms)
    }
  } else {
      out <- fastplyr::f_expand(data, ..., .sort = sort, .by = {{ .by }})
  }
  cheapr::rebuild(out, data)
}
#' @rdname time_expand
#' @export
time_complete <- function(data, time = NULL, ..., .by = NULL,
                          time_by = NULL, from = NULL, to = NULL,
                          sort = TRUE,
                          fill = NULL){
  check_is_df(data)
  if (!is.null(fill) && !is.list(fill) && !(length(fill) == 1 && is.na(fill))){
    cli::cli_abort("{.arg fill} must be either a list or `NULL`")
  }
  group_vars <- get_groups(data, {{ .by }})
  time_info <- mutate_one(data, !!enquo(time), .by = {{ .by }})
  out <- cheapr::df_modify(data, time_info)
  time_var <- names(time_info)
  check_length_lte(time_var, 1)
  expanded_df <- time_expand(out,
                             ...,
                             time = across(all_of(time_var)),
                             time_by = time_by,
                             from = !!enquo(from),
                             to = !!enquo(to),
                             sort = FALSE,
                             .by = {{ .by }})
  # Full-join
  if (df_nrow(expanded_df) > 0 && df_ncol(expanded_df) > 0){
    # Check to see if time has turned to POSIX
    if (length(time_var) > 0){
      out[[time_var]] <- time_cast(out[[time_var]], expanded_df[[time_var]])
    }
    out <- fastplyr::f_full_join(
      out, expanded_df, by = names(expanded_df)
    )
    if (sort){
      out <- fastplyr::f_arrange(
        out, .cols = c(group_vars, time_var,
                       setdiff(names(expanded_df),
                               c(group_vars, time_var)))
      )
    }
  }

  # Replace NA with fill

  if (is.list(fill)){
    fill_nms <- names(fill)
    for (i in seq_along(fill)){
      if (length(fill[[i]]) != 1){
        cli::cli_abort("{.arg fill} values must be of length 1")
      }
      var <- fill_nms[[i]]
      na_fill <- fill[[i]]
      out[[var]] <- cheapr::na_replace(out[[var]], na_fill)
    }
  }
  out_vars <- c(names(data), setdiff(names(out), names(data)))
  out <- fastplyr::f_select(out, .cols = out_vars)
  cheapr::rebuild(out, data)
}
