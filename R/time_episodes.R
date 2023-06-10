#' Episodic calculation of time-since-event data
#'
#' @description This function calculates episodic flag
#' variables
#' based on a pre-defined episode threshold of a chosen time unit.
#'
#' @param data A data frame.
#' @param time Date or datetime variable to perform episodic window calculation.
#' @param window Numeric variable defining the episode threshold.
#' Observations with a period >= window since the last observation
#' are defined as a new episode.
#' By default, every event is classed as a new episode, which can be useful
#' when analysis time-since-event data. \cr
#' A vector of window values may be supplied.
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
#' @param event (Optional) List that encodes which rows are events,
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
#' Columns are specified using tidy-select.,
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
#' 2 are positives within the second episode of infection.
#' * \bold{ep_id_new} - An integer variable signifying the first
#' instance of each episode.]
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
time_episodes <- function(data, time, window = 1L,
                          time_by = NULL,
                          event = NULL,
                          type = c("auto", "duration", "period"),
                          .by = NULL){
  N <- nrow2(data)
  start_nms <- names(data)
  time_quo <- enquo(time)
  window_quo <- enquo(window)
  data <- data %>%
    dplyr::mutate(!!time_quo,
                  !!window_quo,
                  .by = {{ .by }})
  group_vars <- get_groups(data, .by = {{ .by }})
  time_col <- tidy_transform_names(data, !!time_quo)
  window_col <- tidy_transform_names(data, !!window_quo)
  # Data names after data-masking
  data_nms <- names(data)
  # Add event identifier col
  event_id_nm <- new_var_nm(data_nms, ".event.id")
  if (is.null(event)){
    .event.id <- data.table::fifelse(is.na(data[[time_col]]),
                                     NA_integer_,
                                     1L)
    # .event.id <- alloc(1L, N)
    # setv(.event.id, which(is.na(data[[time_col]])), NA_integer_, vind1 = TRUE)
    event_col <- character(0)
  } else {
    if (!isTRUE(is.list(event) &&
                collapse::fncol(event) == 1L &&
                length(names(event)) == 1L)){
      stop("event must be named list of length 1")

    }
    event_col <- names(event)
    .event.id <- data.table::fifelse(data[[event_col]] %in%
                                       event[[1L]],
                                     1L, 0L)
    if (sum(is.na(data[[event_col]]) != is.na(data[[time_col]])) > 0){
      warning(paste0("There is a mismatch of NAs between ",
                     time_col, " and ",
                     event_col, ", please check."))
    }
  }
  out <- fselect(data, .cols = c(group_vars, time_col, window_col,
                                 event_col))
  out <- data.table::copy(as_DT(out))
  data.table::set(out, j = event_id_nm, value = .event.id)
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
  lag <- min(N, 1L) # Bound lag to >= 0
  date_lag_nm <- new_var_nm(data_nms, "date_lag")
  # Calculating elapsed time
  # Method:
  # For rows that are events, use the date, otherwise NA
  # Fill the NAs using last-observation-carried-forward (locf)
  #  excluding the NA values in time
  # Where there is an event, take the lagged date, which now corresponds
  # to the time of the last event.
  out[, (date_lag_nm) := data.table::fcase(get(event_id_nm) == 1L,
                                           get(time_col))]
  out[, (date_lag_nm) := data.table::fcase(!is.na(get(time_col)),
                                           data.table::nafill(get(date_lag_nm),
                                                              type = "locf"))]
  # data.table::setnafill(out, cols = date_lag_nm,
  #                       type = "locf")
  data.table::set(out, j = date_lag_nm,
                  value = data.table::fifelse(out[[event_id_nm]] == 1L,
                                              collapse::flag(out[[date_lag_nm]],
                                                             n = lag,
                                                             g = g),
                                              out[[date_lag_nm]]))
  # Time difference
  t_elapsed_nm <- "t_elapsed"
  out[, (t_elapsed_nm) := time_diff(get(date_lag_nm), get(time_col),
                                                by = time_by, type = type)]
  # Row IDs per subject
  subject_row_id_nm <- new_var_nm(data_nms, "subject_row_id")
  data.table::set(out, j = subject_row_id_nm,
                  value = frowid(g[["group.id"]], g = g))
  # Binary variable indicating if new episode or not
  new_episode_nm <- new_var_nm(data_nms, "new_episode")
  # The first event is always a new episode
  # Events where t_elapsed >= window are new episodes
  # Non-events are NA
  out[, (new_episode_nm) := data.table::fcase(get(event_id_nm) == 1L & (
    get(subject_row_id_nm) == 1L |
      get(t_elapsed_nm) >=
      get(window_col)
  ), 1L,
  get(event_id_nm) == 1L, 0L)]
  # Episode ID at record level
  ep_id_nm <- "ep_id"
  data.table::set(out, j = ep_id_nm,
                  value = fcumsum(out[[new_episode_nm]],
                                  g = g,
                                  na.rm = TRUE))
  # out[, (ep_id_nm) := fcumsum(.SD, g = g,
  #                             na.rm = TRUE),
  #     .SDcols = new_episode_nm]
  # Episode ID at group level
  ep_id_new_nm <- "ep_id_new"
  out[, (ep_id_new_nm) := data.table::fifelse(get(new_episode_nm) == 1L,
                                                    get(ep_id_nm), 0L)]
  # Add episode start dates
  # Get min episode dates for each subject + episode
  ep_start_nm <- new_var_nm(data_nms, "ep_start")
  data.table::set(out, j = ep_start_nm,
                  value = data.table::fcase(out[[event_id_nm]] == 1L,
                                            gmin(out[[time_col]],
                                                 g = list(g[["group.id"]],
                                                          out[[ep_id_nm]]),
                                                 na.rm = TRUE)))
  # Remove unnecessary cols
  set_rm_cols(out, c(grp_nm, date_lag_nm, event_id_nm,
                     subject_row_id_nm, new_episode_nm))
  # Newly added episodic columns
  new_cols <- c(t_elapsed_nm, ep_start_nm,
                ep_id_nm, ep_id_new_nm)
  # if (.add){
  #   # If window col was a variable in data, then don't remove it
  #   if (!window_col %in% start_nms){
  #     set_rm_cols(data, window_col)
  #   }
  #   # Sort by initial order
  #   data.table::setorderv(out, cols = sort_nm)
  #   # Remove sort ID col
  #   set_rm_cols(out, sort_nm)
  #   set_bind_cols(data, fselect(out, .cols = new_cols))
  #   df_reconstruct(data, template)
  # } else {
    # If window col was a variable in data, then don't remove it
  if (!window_col %in% start_nms){
    set_rm_cols(out, window_col)
  }
  # Sort by initial order
  setorderv2(out, cols = sort_nm)
  # Remove sort ID col
  set_rm_cols(out, sort_nm)
  out_nms <- c(group_vars, time_col, event_col, new_cols)
  # Set the column order
  data.table::setcolorder(out, out_nms)
  df_reconstruct(out, data)
  # }
}
