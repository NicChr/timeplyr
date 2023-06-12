#' Episodic calculation of time-since-event data
#'
#' @description This function calculates episodic flag
#' variables
#' based on a pre-defined episode threshold of a chosen time unit.
#'
#' @param data A data frame.
#' @param time Date or datetime variable to use for the episode calculation.
#' Supply the variable using `tidyselect` notation.
#' @param window Numeric variable defining the episode threshold.
#' Observations with a period >= window since the last observation
#' are defined as a new episode.
#' By default, every event is classed as a new episode, which can be useful
#' when analysis time-since-event data. \cr
#' A vector of window values may be supplied.
#' Tidy `data-masking` applies.
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
#' This is also vectorized where applicable.
#' @param .add 	Should episodic variables be added to the data? \cr
#' If `FALSE` (the default), then only the relevant variables are returned. \cr
#' If `TRUE`, the episodic variables are added to the original data using
#' `dplyr::bind_cols()`. \cr
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
#' @param type Time difference type, auto, duration, period.
#' With larger data, it is recommended to use `type = "duration"` for
#' speed and efficiency.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using `tidyselect`.
#' @return
#' A `data.frame` in the same order as it was given.
#' @details
#' An example of episodic analysis can include disease infections over time
#' where episodes can be seen as reinfections, where a reinfection is defined
#' as a positive result after a pre-determined number of days since a
#' subject's last positive result.
#'
#' To perform simple time-since-event analysis, set `window` to `0`, which is
#' the default.
#'
#' 4 Key variables will be calculated:
#' * \bold{ep_id} - An integer variable signifying
#' which episode each record/observation belongs to.
#' This is an increasing integer starting at 1.
#' In the infections scenario, 1 are positives within the
#' first episode of infection,
#' 2 are positives within the second episode of infection and so on.
#' * \bold{ep_id_new} - An integer variable signifying the first
#' instance of each episode.
#' This is an increasing integer starting at 0 where
#' 0 signifies within-episode observations and >= 1
#' signifies each first instance of that episode.
#' * \bold{t_elapsed} - The time elapsed since the last episode.
#' On the occurrence of a new episode, this will be the time since
#' the previous episode.
#' Time units are specified in the by argument.
#' * \bold{ep_start} - Start date/datetime of the episode.
#'
#' `data.table` and `collapse` are used for speed and efficiency.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(nycflights13)
#' library(lubridate)
#' library(ggplot2)
#'
#' # Say we want to flag origin-destination pairs
#' # that haven't seen departures or arrivals for a week
#'
#' events <- flights %>%
#'   mutate(date = as_date(time_hour)) %>%
#'   group_by(origin, dest) %>%
#'   time_episodes(date, time_by = "week", window = 1)
#' episodes <- events %>%
#'   filter(ep_id_new > 1)
#' nrow(fdistinct(episodes, origin, dest)) # 55 origin-destinations
#'
#' # As expected summer months saw the least number of
#' # dry-periods
#' episodes %>%
#'   ungroup() %>%
#'   time_count(time = ep_start, by = "week", floor_date = TRUE) %>%
#'   ggplot(aes(x = ep_start, y = n)) +
#'   geom_bar(stat = "identity")
#' @export
time_episodes <- function(data, time, window = 0L,
                          time_by = NULL,
                          .add = FALSE,
                          event = NULL,
                          type = c("auto", "duration", "period"),
                          .by = NULL){
  N <- nrow2(data)
  start_nms <- names(data)
  time_quo <- enquo(time)
  window_quo <- enquo(window)
  data <- dplyr::mutate(data, !!window_quo,
                        .by = {{ .by }})
  group_vars <- get_groups(data, .by = {{ .by }})
  time_col <- tidy_select_names(data, !!time_quo)
  window_col <- tidy_transform_names(data, !!window_quo)
  # Data names after data-masking
  data_nms <- names(data)
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
    event_id_nm <- new_var_nm(data, ".event.id")
    data <- dplyr::dplyr_col_modify(data, cols = setnames(list(
      data.table::fifelse(data[[event_col]] %in%
                            event[[1L]],
                          1L, 0L)
    ), event_id_nm))
    if (sum(is.na(data[[event_col]]) != is.na(data[[time_col]])) > 0){
      warning(paste0("There is a mismatch of NAs between ",
                     time_col, " and ",
                     event_col, ", please check."))
    }
  }
  out <- fselect(data, .cols = c(group_vars, time_col, window_col,
                                 event_col, event_id_nm))
  out <- data.table::copy(as_DT(out))
  if (length(time_col) == 0){
    stop("Please supply date or datetime for episode calculation")
  }
  if (length(window_col) == 0){
    stop("Please supply window for episode calculation")
  }
  time_by <- time_by_get(out[[time_col]], by = time_by)
  # Create group ID variable
  grp_nm <- new_var_nm(data_nms, ".group")
  data.table::set(out,
                  j = grp_nm,
                  value = group_id(data, .by = {{ .by }},
                                   as_qg = TRUE, order = TRUE))
  # Group by group vars + time
  g2 <- collapse::GRP(fselect(out, .cols = c(grp_nm, time_col)),
                      sort = TRUE, return.order = FALSE, return.groups = FALSE)
  # If data is already sorted correctly, no need to sort it
  if (!isTRUE(g2[["ordered"]]["sorted"])){
    # Keep track of initial order
    sort_nm <- new_var_nm(data_nms, ".sort.index")
    out[, (sort_nm) := seq_len(.N)] # Index variable
    # Sort by groups > case ID > date col
    grp_nm2 <- new_var_nm(data_nms, ".group2")
    data.table::set(out, j = grp_nm2, value = g2[["group.id"]])
    data.table::setorderv(out, cols = grp_nm2)
    data.table::set(out, j = grp_nm2, value = NULL)
  } else {
    sort_nm <- character(0)
  }
  # Group info
  g <- collapse::GRP(out[[grp_nm]], sort = TRUE,
                     return.groups = FALSE, call = FALSE,
                     return.order = FALSE)

  ### Episode calculation ###
  # Calculation by reference (data.table set notation)
  calc_episodes(out, time = time_col,
                time_by = time_by,
                type = type,
                g = g,
                gid = grp_nm,
                event = event_id_nm,
                window = window_col)
  # Remove window variable
  set_rm_cols(out, window_col)
  # Newly added episodic columns
  new_cols <- c("t_elapsed", "ep_start", "ep_id", "ep_id_new")
  # Sort by initial order
  setorderv2(out, cols = sort_nm)
  # Remove sort ID col
  set_rm_cols(out, sort_nm)
  if (.add){
    if (!window_col %in% start_nms){
      # Remove window variable
      data <- dplyr::dplyr_col_modify(data, cols = setnames(list(NULL), window_col))
    }
   dplyr::bind_cols(data, fselect(out, .cols = new_cols))
  } else {
    out_nms <- c(group_vars, time_col, event_col, new_cols)
    # Set the column order
    out <- fselect(out, .cols = out_nms)
    df_reconstruct(out, data)
  }
}
# Internal helper to calculate time episodes
# Data must be sorted by groups + time
calc_episodes <- function(data,
                          time, # time col
                          time_by, # time unit (days, etc)
                          type, # Type (duration/period)
                          g, # GRP object
                          gid, # group id col
                          event, # Event col
                          window){ # Window col
  N <- nrow2(data)
  lag <- min(N, 1L) # Bound lag to >= 0

  ##### (More efficient) METHOD assuming all rows are events #####

  if (length(event) == 0L){
    roll_event_id_nm <- character(0)
    time_lag_nm <- new_var_nm(data, "date_lag")
    # Calculating elapsed time
    data.table::set(data,
                    j = time_lag_nm,
                    value = collapse::flag(data[[time]],
                                           n = lag,
                                           g = g))
    # Time difference
    data.table::set(data,
                    j = "t_elapsed",
                    value = time_diff(data[[time_lag_nm]], data[[time]],
                                      by = time_by, type = type))
    # Row IDs per subject
    subject_row_id_nm <- new_var_nm(data, "subject_row_id")
    data.table::set(data,
                    j = subject_row_id_nm,
                    value = frowid(g[["group.id"]], g = g))
    # Binary variable indicating if new episode or not
    new_episode_nm <- new_var_nm(data, "new_episode")
    # The first event is always a new episode
    # Events where t_elapsed >= window are new episodes
    data.table::set(data,
                    j = new_episode_nm,
                    value = data.table::fifelse(
                      data[[subject_row_id_nm]] == 1L |
                        data[["t_elapsed"]] >=
                        data[[window]], 1L, 0L))
    # Episode ID at record level
    data.table::set(data, j = "ep_id",
                    value = fcumsum(data[[new_episode_nm]],
                                    g = g,
                                    na.rm = TRUE))
    # Episode ID at group level
    data.table::set(data,
                    j = "ep_id_new",
                    value = data.table::fifelse(data[[new_episode_nm]] == 1L,
                                                data[["ep_id"]], 0L))
    # Add episode start dates
    # Get min episode dates for each subject + episode
    data.table::set(data,
                    j = "ep_start",
                    value = gmin(data[[time]],
                                 g = collapse::GRP(list(g[["group.id"]],
                                                        data[["ep_id"]]),
                                                   sort = TRUE,
                                                   return.groups = FALSE),
                                 na.rm = TRUE))
  } else {

    ##### METHOD for data with a mix of event and non-event rows #####

    ## This method needs a lot of improvement! ##

    time_lag_nm <- new_var_nm(data, "date_lag")
    subject_row_id_nm <- character(0)
    time_na <- vctrs::vec_init(data[[time]]) # time NA with correct class
    is_event <- data[[event]] == 1L # Logical
    which_time_na <- collapse::whichNA(data[[time]]) # Which time are NA
    # Method:
    # For rows that are events, use the date, otherwise NA
    data.table::set(data,
                    j = time_lag_nm,
                    value = data.table::fifelse(is_event,
                                                data[[time]],
                                                time_na))
    # NA fill the dates by group
    data[, (time_lag_nm) :=
           vctrs::vec_fill_missing(.SD, direction = "down"),
         .SDcols = time_lag_nm,
         by = gid]
    # Replace incorrectly filled time with NA
    data.table::set(data,
                    i = which_time_na,
                    j = time_lag_nm,
                    value = time_na)
    # For event rows, take the lagged filled date
    data.table::set(data,
                    j = time_lag_nm,
                    value = data.table::fifelse(is_event,
                                                collapse::flag(data[[time_lag_nm]],
                                                               n = lag,
                                                               g = g),
                                                data[[time_lag_nm]]))
    # Cumulative sum of event ID
    roll_event_id_nm <- new_var_nm(data, ".event.rolling.id")
    data.table::set(data,
                    j = roll_event_id_nm,
                    value = fcumsum(data[[event]], g = g, na.rm = TRUE))
    # Time difference
    data.table::set(data,
                    j = "t_elapsed",
                    value = time_diff(data[[time_lag_nm]], data[[time]],
                                      by = time_by, type = type))
    # Binary variable indicating if new episode or not
    new_episode_nm <- new_var_nm(data, "new_episode")
    # The first event is always a new episode
    # Events where t_elapsed >= window are new episodes
    # Non-events are NA
    data.table::set(data,
                    j = new_episode_nm,
                    value = data.table::fcase(is_event & (
                      data[[roll_event_id_nm]] == 1L |
                        data[["t_elapsed"]] >=
                        data[[window]]
                    ), 1L,
                    is_event, 0L))
    # Episode ID at record level
    data.table::set(data,
                    j = "ep_id",
                    value = fcumsum(data[[new_episode_nm]],
                                    g = g,
                                    na.rm = TRUE))
    # Episode ID at group level
    data.table::set(data,
                    j = "ep_id_new",
                    value = data.table::fifelse(data[[new_episode_nm]] == 1L,
                                                data[["ep_id"]], 0L))
    # Add episode start dates
    # Get min episode dates for each subject + episode
    data.table::set(data,
                    j = "ep_start",
                    value = data.table::fifelse(is_event,
                                                gmin(data[[time]],
                                                     g = collapse::GRP(list(g[["group.id"]],
                                                                            data[["ep_id"]]),
                                                                       sort = TRUE,
                                                                       return.groups = FALSE),
                                                     na.rm = TRUE),
                                                time_na))
  }
  # Remove unnecessary cols
  set_rm_cols(data, c(time_lag_nm, event, roll_event_id_nm,
                     subject_row_id_nm, new_episode_nm))
}

# Faster but less readable code and less memory efficient.
# calc_episodes2 <- function(data,
#                           time, # time col
#                           time_by, # time unit (days, etc)
#                           type, # Type (duration/period)
#                           g, # GRP object
#                           gid, # group id col
#                           event, # Event col
#                           window){ # Window col
#   N <- nrow2(data)
#   lag <- min(N, 1L) # Bound lag to >= 0
#
#   ##### (More efficient) METHOD assuming all rows are events #####
#
#   if (length(event) == 0L){
#     roll_event_id_nm <- character(0)
#     time_lag_nm <- new_var_nm(data, "date_lag")
#     # Calculating elapsed time
#     data.table::set(data,
#                     j = time_lag_nm,
#                     value = collapse::flag(data[[time]],
#                                            n = lag,
#                                            g = g))
#     # Time difference
#     data.table::set(data,
#                     j = "t_elapsed",
#                     value = time_diff(data[[time_lag_nm]], data[[time]],
#                                       by = time_by, type = type))
#     # Row IDs per subject
#     subject_row_id_nm <- new_var_nm(data, "subject_row_id")
#     data.table::set(data,
#                     j = subject_row_id_nm,
#                     value = frowid(g[["group.id"]], g = g))
#     # Binary variable indicating if new episode or not
#     new_episode_nm <- new_var_nm(data, "new_episode")
#     # The first event is always a new episode
#     # Events where t_elapsed >= window are new episodes
#     data.table::set(data,
#                     j = new_episode_nm,
#                     value = data.table::fifelse(
#                       data[[subject_row_id_nm]] == 1L |
#                         data[["t_elapsed"]] >=
#                         data[[window]], 1L, 0L))
#     # Episode ID at record level
#     data.table::set(data, j = "ep_id",
#                     value = fcumsum(data[[new_episode_nm]],
#                                     g = g,
#                                     na.rm = TRUE))
#     # Episode ID at group level
#     data.table::set(data,
#                     j = "ep_id_new",
#                     value = data.table::fifelse(data[[new_episode_nm]] == 1L,
#                                                 data[["ep_id"]], 0L))
#     # Add episode start dates
#     # Get min episode dates for each subject + episode
#     data.table::set(data,
#                     j = "ep_start",
#                     value = gmin(data[[time]],
#                                  g = collapse::GRP(list(g[["group.id"]],
#                                                         data[["ep_id"]]),
#                                                    sort = TRUE,
#                                                    return.groups = FALSE),
#                                  na.rm = TRUE))
#   } else {
#
#     ##### METHOD for data with a mix of event and non-event rows #####
#
#     ## This method needs a lot of improvement! ##
#
#     time_lag_nm <- new_var_nm(data, "date_lag")
#     subject_row_id_nm <- character(0)
#     time_na <- vctrs::vec_init(data[[time]]) # time NA with correct class
#     is_event <- data[[event]] == 1L # Logical
#     # Method:
#     # For rows that are events, use the date, otherwise NA
#     data.table::set(data,
#                     j = time_lag_nm,
#                     value = data.table::fifelse(is_event,
#                                                 data[[time]],
#                                                 time_na))
#     # NA fill the dates
#     data.table::set(data,
#                     j = time_lag_nm,
#                     value = data.table::fifelse(!is.na(data[[time]]),
#                                                 data.table::nafill(data[[time_lag_nm]],
#                                                                    type = "locf"),
#                                                 time_na))
#     # For event rows, take the lagged filled date
#     data.table::set(data,
#                     j = time_lag_nm,
#                     value = data.table::fifelse(is_event,
#                                                 collapse::flag(data[[time_lag_nm]],
#                                                                n = lag,
#                                                                g = g),
#                                                 data[[time_lag_nm]]))
#     # Row IDs per subject
#     subject_row_id_nm <- new_var_nm(data, "subject_row_id")
#     data.table::set(data,
#                     j = subject_row_id_nm,
#                     value = frowid(g[["group.id"]], g = g))
#
#     # At row ID 1 for each subject, make the lagged time NA
#     data.table::set(data,
#                     i = collapse::whichv(data[[subject_row_id_nm]], 1L),
#                     j = time_lag_nm,
#                     value = time_na)
#     # Number of total events by group
#     data.table::set(data,
#                     j = ".n.events",
#                     value = gsum(data[[event]], g = g, na.rm = TRUE))
#     # If a group had no events, just replace the lagged time values with NA
#     data.table::set(data,
#                     i = collapse::whichv(data[[".n.events"]], 0L),
#                     j = time_lag_nm,
#                     value = time_na)
#     # Remove number of events col
#     data.table::set(data, j = ".n.events", value = NULL)
#     # Cumulative sum of event ID
#     roll_event_id_nm <- new_var_nm(data, ".event.rolling.id")
#     data.table::set(data,
#                     j = roll_event_id_nm,
#                     value = fcumsum(data[[event]], g = g, na.rm = TRUE))
#     # setv(data[[time_lag_nm]], which(data[[roll_event_id_nm]] == 1L & is_event),
#     #      time_na, vind1 = TRUE)
#     data.table::set(data,
#                     i = which(data[[roll_event_id_nm]] == 1L & is_event),
#                     j = time_lag_nm,
#                     value = time_na)
#
#     # Time difference
#     data.table::set(data,
#                     j = "t_elapsed",
#                     value = time_diff(data[[time_lag_nm]], data[[time]],
#                                       by = time_by, type = type))
#     # Binary variable indicating if new episode or not
#     new_episode_nm <- new_var_nm(data, "new_episode")
#     # The first event is always a new episode
#     # Events where t_elapsed >= window are new episodes
#     # Non-events are NA
#     data.table::set(data,
#                     j = new_episode_nm,
#                     value = data.table::fcase(is_event & (
#                       data[[roll_event_id_nm]] == 1L |
#                         data[["t_elapsed"]] >=
#                         data[[window]]
#                     ), 1L,
#                     is_event, 0L))
#     # Episode ID at record level
#     data.table::set(data,
#                     j = "ep_id",
#                     value = fcumsum(data[[new_episode_nm]],
#                                     g = g,
#                                     na.rm = TRUE))
#     # Episode ID at group level
#     data.table::set(data,
#                     j = "ep_id_new",
#                     value = data.table::fifelse(data[[new_episode_nm]] == 1L,
#                                                 data[["ep_id"]], 0L))
#     # Add episode start dates
#     # Get min episode dates for each subject + episode
#     data.table::set(data,
#                     j = "ep_start",
#                     value = data.table::fifelse(is_event,
#                                                 gmin(data[[time]],
#                                                      g = collapse::GRP(list(g[["group.id"]],
#                                                                             data[["ep_id"]]),
#                                                                        sort = TRUE,
#                                                                        return.groups = FALSE),
#                                                      na.rm = TRUE),
#                                                 time_na))
#   }
#   # Remove unnecessary cols
#   set_rm_cols(data, c(time_lag_nm, event, roll_event_id_nm,
#                       subject_row_id_nm, new_episode_nm))
# }
