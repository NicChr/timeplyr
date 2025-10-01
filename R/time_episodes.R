#' Episodic calculation of time-since-event data
#'
#' @description This function assigns episodes to events
#' based on a pre-defined threshold of a chosen time unit.
#'
#' @param data A data frame.
#' @param time Date or datetime variable to use for the episode calculation.
#' Supply the variable using `tidyselect` notation.
#' @param time_by Time units used to calculate episode flags.
#' If `time_by` is `NULL` then a heuristic will try and estimate the highest
#' order time unit associated with the time variable.
#' If specified, then by must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param window Single number defining the episode threshold.
#' When `rolling = TRUE` events with a
#' `t_elapsed >= window` since the last event
#' are defined as a new episode. \cr
#' When `rolling = FALSE` events with a
#' `t_elapsed >= window` since the first event of the corresponding episode
#' are defined as a new episode. \cr
#' By default, `window = 1` which assigns every event to a new episode.
#' @param roll_episode Logical.
#' Should episodes be calculated using a rolling or fixed window?
#' If `TRUE` (the default), an amount of time must have passed (`>= window`)
#' since the last event, with each new event
#' effectively resetting the time at which
#' you start counting. \cr
#' If `FALSE`, the elapsed time is fixed and
#' new episodes are defined based on how much cumulative time has
#' passed since the first event of each episode.
#' @param switch_on_boundary When an exact amount of time
#' (specified in `time_by`) has passed, should there be an increment in ID? \cr
#' The default is `TRUE`. \cr
#' For example, if `time_by = "days"` and
#' `switch_on_boundary = FALSE`, `> 1` day must have passed, otherwise
#' `>= 1` day must have passed.
#' @param fill Value to fill first time elapsed value. Only applicable when
#' `roll_episode = TRUE`. \cr
#' Default is `0`.
#' @param .add 	Should episodic variables be added to the data? \cr
#' If `FALSE` (the default), then only the relevant variables are returned. \cr
#' If `TRUE`, the episodic variables are added to the original data.
#' In both cases, the order of the data is unchanged.
#' @param event (\bold{Optional}) List that encodes which rows are events,
#' and which aren't.
#' By default `time_episodes()`
#' assumes every observation (row) is an event
#' but this need not be the case. \cr
#' `event` must be a named list of length 1 where the values of the
#' list element represent the event. For example, if your events were coded as
#' `0` and `1` in a variable named "evt" where `1` represents the event,
#' you would supply `event = list(evt = 1)`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using `tidyselect`.
#'
#' @returns
#' A `data.frame` in the same order as it was given.
#'
#' @details
#' `time_episodes()` calculates the time elapsed (rolling or fixed) between
#' successive events, and flags these events as episodes or not based on how much
#' time has passed.
#'
#' An example of episodic analysis can include disease infections over time.
#'
#' In this example, a positive test result represents an \bold{event} and \cr
#' a new infection represents a new \bold{episode}.
#'
#' It is assumed that after a pre-determined amount of time, a positive result
#' represents a new episode of infection.
#'
#' To perform simple time-since-event analysis, which means one
#' is not interested in episodes, simply use `time_elapsed()` instead.
#'
#' To find implicit missing gaps in time, set `window` to `1` and
#' `switch_on_boundary` to `FALSE`. Any event classified as an
#' episode in this scenario is an event following a gap in time.
#'
#' The data are always sorted before calculation and then
#' sorted back to the input order.
#'
#' 4 Key variables will be calculated:
#' * \bold{ep_id} - An integer variable signifying
#' which episode each event belongs to. \cr
#' Non-events are assigned `NA`. \cr
#' `ep_id` is an increasing integer starting at 1.
#' In the infections scenario, 1 are positives within the
#' first episode of infection,
#' 2 are positives within the second episode of infection and so on.
#' * \bold{ep_id_new} - An integer variable signifying the first
#' instance of each new episode.
#' This is an increasing integer where
#' 0 signifies within-episode observations and >= 1
#' signifies the first instance of the respective episode.
#' * \bold{t_elapsed} - The time elapsed since the last event. \cr
#' When `roll_episode = FALSE`, this becomes the time elapsed since the
#' first event of the current episode.
#' Time units are specified in the by argument.
#' * \bold{ep_start} - Start date/datetime of the episode.
#'
#' `data.table` and `collapse` are used for speed and efficiency.
#'
#' @seealso [time_elapsed] [time_seq_id]
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(nycflights13)
#' library(lubridate)
#' library(ggplot2)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # Say we want to flag origin-destination pairs
#' # that haven't seen departures or arrivals for a week
#'
#' events <- flights |>
#'   mutate(date = as_date(time_hour)) |>
#'   group_by(origin, dest) |>
#'   time_episodes(date, "week", window = 1)
#'
#' events
#'
#' episodes <- events |>
#'   filter(ep_id_new > 1)
#' nrow(fastplyr::f_distinct(episodes, origin, dest)) # 55 origin-destinations
#'
#' # As expected summer months saw the least number of
#' # dry-periods
#' episodes |>
#'   ungroup() |>
#'   time_by(ep_start, "week", .name = "ep_start") |>
#'   count(ep_start = interval_start(ep_start)) |>
#'   ggplot(aes(x = ep_start, y = n)) +
#'   geom_bar(stat = "identity")
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#' }
#' @export
time_episodes <- function(data, time, time_by = NULL,
                          window = 1,
                          roll_episode = TRUE,
                          switch_on_boundary = TRUE,
                          fill = 0,
                          .add = FALSE,
                          event = NULL,
                          .by = NULL){
  rlang::check_required(time)
  N <- df_nrow(data)
  check_length(window, 1)
  check_is_num(window)
  if (window < 0){
    stop("window must be strictly greater or equal to 0")
  }
  start_nms <- names(data)
  time_quo <- enquo(time)
  data <- fastplyr::f_group_by(data, .by = {{ .by }}, .order = TRUE, .add = TRUE)
  group_vars <- get_groups(data)
  time_col <- tidy_select_names(data, !!time_quo)
  if (length(time_col) == 0){
    stop("Please supply date or datetime for episode calculation")
  }
  temp <- fastplyr::f_ungroup(data)
  # Data names after data-masking
  data_nms <- names(temp)
  if (is.null(event)){
    event_col <- character(0)
    event_id_nm <- character(0)
  } else {
    if (!isTRUE(is.list(event) &&
                collapse::fncol(event) == 1L &&
                length(names(event)) == 1L)){
      stop("event must be named list of length 1")

    }
    event_col <- names(event)
    if (!event_col %in% data_nms){
      stop(paste0("Column `", event_col, "` doesn't exist"))
    }
    # Add event identifier col
    event_id_nm <- unique_col_name(temp, ".event.id")
    temp <- cheapr::df_modify(
      temp, add_names(list(
        cheapr::cheapr_if_else(
          temp[[event_col]] %in_% event[[1L]], 1L, 0L
        )
      ), event_id_nm)
    )
    if (cheapr::na_count(temp[[event_col]]) != cheapr::na_count(temp[[time_col]])){
      warning(paste0("There is a mismatch of NAs between ",
                     time_col, " and ",
                     event_col, ", please check."))
    }
  }
  groups <- fastplyr::f_select(fastplyr::as_tbl(temp), .cols = group_vars)
  events <- fastplyr::f_select(fastplyr::as_tbl(temp), .cols = event_col)
  temp <- fastplyr::f_select(temp, .cols = c(group_vars, time_col,
                                             event_col, event_id_nm))
  time_by <- time_by_get(temp[[time_col]], time_by = time_by)
  # Create group ID variable
  grp_nm <- unique_col_name(data_nms, ".group")
  temp <- cheapr::df_modify(temp, add_names(
    list(
      old_group_id(data, as_qg = TRUE, order = TRUE)
    ), grp_nm
  ))
  n_groups <- attr(temp[[grp_nm]], "N.groups")
  group_sizes <- attr(temp[[grp_nm]], "group.sizes")

  # Remove qG class
  cheapr::attrs_clear(temp[[grp_nm]], .set = TRUE)
  # Group by group vars + time
  grp_nm2 <- unique_col_name(temp, ".group")
  temp <- cheapr::df_modify(temp, add_names(list(
    old_group_id(temp, .cols = c(grp_nm, time_col), order = TRUE)
  ), grp_nm2))
  data_is_sorted <- is_sorted(temp[[grp_nm2]])
  # Add row ID
  row_id_nm <- unique_col_name(temp, ".row_id")
  temp <- fastplyr::add_row_id(temp, .name = row_id_nm)

  temp <- fastplyr::f_select(temp, .cols = c(grp_nm, grp_nm2, row_id_nm, time_col, event_id_nm))

  # If data is already sorted correctly, no need to sort it
  if (!data_is_sorted){

    # This does work and sorts in-place but not sure
    # if I can reliably depend on this working in the future
    # temp <- r_copy(temp)
    # df_set_order(temp, grp_nm2)

    temp <- fastplyr::f_arrange(temp, .cols = grp_nm2)
  }
  # # Group info
  # Since group IDs are sorted at this point
  # We use our very fast internal group ID to GRP conversion
  g <- sorted_group_id_to_GRP(temp[[grp_nm]],
                              n_groups = n_groups,
                              group_sizes = group_sizes)
  # Convert non-event dates to NA
  # So that they can be skipped/ignored
  if (length(event_col) > 0){
    which_non_event <- cheapr::val_find(temp[[event_id_nm]], 0L)
    event_dates <- temp[[time_col]][which_non_event] # Save to re-add later
    temp[[time_col]][which_non_event] <- na_init(temp[[time_col]])
  }
  ### Episode calculation ###
  temp <- calc_episodes(
    temp, time = time_col,
    time_by = time_by,
    switch_on_boundary = switch_on_boundary,
    g = g,
    gid = grp_nm,
    window = window,
    roll_episode = roll_episode,
    fill = fill
  )
  # Re-add dates that were modified
  if (length(event_col) > 0){
    temp[[time_col]][which_non_event] <- event_dates
  }
  # Newly added episodic columns
  new_cols <- c("t_elapsed", "ep_start", "ep_id", "ep_id_new")
  temp <- df_rm_cols(temp, c(grp_nm, grp_nm2, event_id_nm))
  # Sort by initial order
  if (!data_is_sorted){

    # df_set_order(temp, row_id_nm)

    temp <- fastplyr::f_arrange(temp, .cols = row_id_nm)
  }
  temp <- df_rm_cols(temp, row_id_nm)
  if (.add){
    # Simply bind the cols together
    out <- fastplyr::f_bind_cols(data, fastplyr::f_select(temp, .cols = new_cols))
  } else {
    # Only keep the key variables
    out_nms <- c(group_vars, time_col, event_col, new_cols)
    out <- fastplyr::f_bind_cols(groups, temp, events)
    # Set the column order
    out <- fastplyr::f_select(out, .cols = out_nms)
  }
  out <- cheapr::rebuild(out, data)
  threshold <- time_by
  threshold[[1L]] <- time_by_num(time_by) * window
  out <- structure(out, time = time_col, time_by = time_by, threshold = threshold)
  class(out) <- c("episodes_tbl_df", class(out))
  out
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.episodes_tbl_df <- function(x, ...){
  # TO-DO: Add avg events per episode
  groups_header <- character()
  episodes_header <- character()
  elapsed_header <- character()
  threshold_header <- character()
  # Groups
  group_vars <- fastplyr::f_group_vars(x)
  GRPS <- df_to_GRP(x, return.groups = FALSE)
  if (length(group_vars) > 0){
    groups <- fastplyr::f_group_data(x)
    groups_header <- c("Groups" =
                         paste0(paste(group_vars, collapse = ", "),
                                " [",
                                prettyNum(df_nrow(groups), big.mark = ","),
                                "]"))
  }
  # Episodes
  if ("ep_id_new" %in% names(x)){
    # max_episodes <- collapse::fmax(x[["ep_id_new"]], g = GRPS,
    #                                use.g.names = FALSE, na.rm = TRUE)
    n_episodes <- collapse::fsum(x[["ep_id_new"]] > 0L, g = GRPS,
                                 use.g.names = FALSE, na.rm = TRUE)
    median_episodes <- collapse::fmedian(n_episodes)
    total_episodes <- sum(n_episodes)
    mean_episodes <- total_episodes / length(n_episodes)
    episodes_header <- c(
      "Episodes" = paste0(
        "N: ",
        prettyNum(total_episodes, big.mark = ","),
        ", ",
        "Median: ",
        prettyNum(round(median_episodes), big.mark = ","),
        ", ",
        "Mean: ",
        prettyNum(round(mean_episodes, 2), big.mark = ","),
        " ",
        inline_hist(n_episodes, n_bins = 7)
      )
    )
  }
  if ("t_elapsed" %in% names(x) && "ep_id_new" %in% names(x)){
    which_index <- cheapr::val_find(x[["ep_id_new"]], 1L)
    elapsed <- x[["t_elapsed"]]
    elapsed[which_index] <- NA
    pooled_elapsed <- collapse::fmean(elapsed, na.rm = TRUE)
    if (length(pooled_elapsed) == 0){
      pooled_string <- "NaN"
    } else {
      pretty_mean <- add_names(
        list(
          pooled_elapsed * time_by_num(attr(x, "time_by"))
        ), time_by_unit(attr(x, "time_by"))
      )
      if (is.null(names(pretty_mean))){
        pooled_string <- "NA"
      } else {
        pooled_string <- time_by_pretty(pretty_mean)
      }
    }
    elapsed_header <- c(
      "Time b/w events" = paste0(
        "Mean: ",
        pooled_string
      )
    )
  }
  if (!is.null(attr(x, "threshold"))){
    threshold_header <- c("Threshold" = time_by_pretty(attr(x, "threshold")))
  }
  num_row <- prettyNum(df_nrow(x), big.mark = ",")
  num_col <- prettyNum(df_ncol(x), big.mark = ",")
  tbl_header <- c("A tibble" = paste0(num_row, " x ", num_col))
  c(tbl_header,
    groups_header,
    episodes_header,
    elapsed_header,
    threshold_header)
}

# Internal helper to calculate time episodes
# Data must be sorted by groups + time
calc_episodes <- function(data,
                          time, # time col
                          time_by, # time unit (days, etc)
                          switch_on_boundary,
                          g, # GRP object
                          gid, # group id col
                          window, # Window col
                          roll_episode, # Should episode calc be rolling?
                          fill){ # How to fill first time elapsed for rolling calc
  N <- df_nrow(data)
  lag <- min(N, 1L) # Bound lag to >= 0
  time_na <- na_init(data[[time]]) # time NA with correct class
  time_num <- time_by_num(time_by)
  time_unit <- time_by_unit(time_by)
  # Time elapsed
  data <- cheapr::df_modify(
    data, list(
      t_elapsed = time_elapsed(data[[time]],
                               time_by_list_as_timespan(time_by),
                               g = g,
                               fill = fill,
                               rolling = roll_episode,
                               na_skip = TRUE)
    )
  )
  # Binary variable indicating if new episode or not
  # The first event is always a new episode
  # Events where t_elapsed >= window are new episodes
  data <- cheapr::df_modify(data, list(
    ep_id = time_seq_id(data[[time]],
                        time_by_list_as_timespan(time_by),
                        g = g,
                        threshold = window,
                        rolling = roll_episode,
                        switch_on_boundary = switch_on_boundary,
                        na_skip = TRUE)
  ))
  g3 <- collapse::GRP(fastplyr::f_select(data, .cols = c(gid, "ep_id")))
  g3_starts <- GRP_starts(g3)
  data <- cheapr::df_modify(data, list(ep_id_new = 0L))
  cpp_loc_set_replace(data[["ep_id_new"]], g3_starts, data[["ep_id"]][g3_starts])
  cpp_loc_set_replace(data[["ep_id_new"]], cheapr::na_find(data[["ep_id"]]), NA_integer_)

  # Add episode start dates
  # Get min episode dates for each subject + episode
  data <- cheapr::df_modify(data, list(
    ep_start = gfirst(data[[time]],
                      g = g3,
                      na.rm = FALSE)
  ))
  data
}


# time_episodes2 <- function(data, time, time_by = NULL,
#                            window = 1,
#                            roll_episode = TRUE,
#                            switch_on_boundary = TRUE,
#                            fill = 0,
#                            .add = FALSE,
#                            event = NULL,
#                            .by = NULL){
#   rlang::check_required(time)
#   N <- df_nrow(data)
#   check_length(window, 1)
#   check_is_num(window)
#   if (window < 0){
#     cli::cli_abort("{.arg window} must be strictly greater or equal to 0")
#   }
#   data_nms <- names(data)
#   time_quo <- enquo(time)
#   new_data <- fastplyr::f_group_by(data, .by = {{ .by }}, .order = TRUE, .add = TRUE)
#   group_vars <- get_groups(new_data)
#   time_col <- tidy_select_names(new_data, !!time_quo)
#   if (length(time_col) == 0){
#     cli::cli_abort("Please supply time column for episode calculation")
#   }
#   if (is.null(event)){
#     event_col <- character(0)
#     event_id_nm <- character(0)
#   } else {
#     if (!isTRUE(is.list(event) &&
#                 collapse::fncol(event) == 1L &&
#                 length(names(event)) == 1L)){
#       cli::cli_abort("{.arg event} must be named {.cls list} of length 1")
#
#     }
#     event_col <- names(event)
#     if (!event_col %in% data_nms){
#       cli::cli_abort("Column {event_col} doesn't exist")
#     }
#     # Add event identifier col
#     new_data <- cheapr::df_modify(
#       new_data, list(
#         .event.id = cheapr::cheapr_if_else(
#           temp[[event_col]] %in_% event[[1L]], 1L, 0L
#         )
#       )
#     )
#     if (cheapr::na_count(temp[[event_col]]) != cheapr::na_count(temp[[time_col]])){
#       cli::cli_warn(
#         paste0("There is a mismatch of NAs between ",
#                time_col, " and ",
#                event_col, ", please check.")
#       )
#     }
#   }
#   time_by <- get_granularity(new_data[[time_col]], time_by)
#
#   groups <- attributes(new_data)[["GRP"]]
#
#   out <- new_data |>
#     fastplyr::f_ungroup() |>
#     fastplyr::f_mutate(
#       t_elapsed = time_elapsed(
#         .data[[time_col]],
#         time_by, g = groups,
#         rolling = roll_episode, fill = 0
#       ),
#
#       ep_id = time_seq_id(
#         .data[[time_col]], time_by, threshold = window,
#         switch_on_boundary = switch_on_boundary
#       )
#     )
#
#   out <- cheapr::rebuild(out, data)
#   threshold <- time_by
#   threshold[[1L]] <- time_by_num(time_by) * window
#   out <- structure(out, time = time_col, time_by = time_by, threshold = threshold)
#   class(out) <- c("episodes_tbl_df", class(out))
#   out
# }


#' @exportS3Method cheapr::rebuild
rebuild.episodes_tbl_df <- function(x, template, ...){

  class(template) <- cheapr::val_rm(class(template), "episodes_tbl_df")

  out <- cheapr::rebuild(x, template, ...)


  extra_attrs <- cheapr::list_drop_null(attributes(template)[c("time", "time_by", "threshold")])
  attributes(out) <- c(attributes(out), extra_attrs)
  class(out) <- c("episodes_tbl_df", class(out))
  out
}
