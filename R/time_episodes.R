#' Episodic calculation of time-to-event data
#'
#' @description This function calculates episodic flag
#' variables
#' based on a pre-defined episode threshold of a chosen time unit.
#' This can for example consist of a data frame of infections by date and thus
#' new episodes can be seen as reinfections. \cr
#'
#' 4 Key variables will be calculated:
#' * \bold{episode_id} - An integer variable signifying
#' which episode each record/observation belongs to.
#' This is an increasing integer starting at 1.
#' In the infections scenario, 1 are positives within the
#' first episode of infection,
#' 2 are positives within the second episode of infection.
#' * \bold{episode_id_group} - An integer variable signifying the first
#' instance of each new episode for each group.
#' This is an increasing integer starting at 0 where
#' 0 signifies between-episode observations and >= 1
#' signifies each first instance of that episode.
#' * \bold{time_elapsed} - The time elapsed since each group's
#' last observation. Time units are specified in the by argument.
#' * \bold{episode_start} - Start date/datetime of the episode.
#'
#' `data.table` and `collapse` are used for speed and efficiency.
#'
#' @param data A data frame.
#' @param ... (Optional) Groups. This can for example be individual IDs.
#' @param time Date or datetime variable to perform episodic window calculation.
#' @param window Numeric variable defining the episode threshold.
#' Must be greater than 0.
#' Observations with a period >= window since the last observation
#' are defined as a new episode.
#' A vector of window values may be supplied.
#' @param by Time units used to calculate episode flags.
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
#' @param event_id \bold{Not currently used} \cr
#' (Optional) Column that encodes which rows are the event,
#' and which aren't. By default `time_episodes()`
#' assumes every observation (row) is an event
#' but this need not be the case. \cr
#' `event_id` must be a named list of length 1 where the values of the
#' list element represent the event. For example, if your events were coded as
#' `0` and `1` in a variable named "event" where `1` represents the event,
#' you would supply `event_id = list(event = 1)`.
#' @param type Time difference type, auto, duration, period.
#' @param .add Should episodic variables be added to the data?
#' If `FALSE` (the default),
#' then only the relevant variables are returned, ordered by groups + time.
#' If `TRUE`, the episodic variables are added to the original data,
#' in the order of
#' the original data.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
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
#'   group_by(origin, dest) %>%
#'   time_episodes(time = time_hour, by = "week", window = 1)
#' episodes <- events %>%
#'   filter(episode_id_group > 1)
#' nrow(fdistinct(episodes, origin, dest)) # 54 origin-destinations
#'
#' # As expected summer months saw the least number of
#' # dry-periods
#' episodes %>%
#'   ungroup() %>%
#'   time_count(time = episode_start, by = "week", floor_date = TRUE) %>%
#'   ggplot(aes(x = episode_start, y = n)) +
#'   geom_bar(stat = "identity")
#' @export
time_episodes <- function(data, ..., time, window = 1,
                          by = NULL,
                          event_id = NULL,
                          type = c("auto", "duration", "period"),
                          .add = FALSE, .by = NULL){
  N <- nrow2(data)
  if (.add){
    .keep <- "all"
  } else {
    .keep <- "used"
  }
  start_nms <- names(data)
  data <- data %>%
    mutate2(...,
            !!enquo(time),
            !!enquo(window),
            .by = {{ .by }},
            .keep = .keep)
  # Copy data frame
  out <- data.table::copy(data)
  data.table::setDT(out)
  # out <- list_to_DT(data)
  group_info <- get_group_info(data, ...,
                               .by = {{ .by }},
                               type = "data-mask")
  group_vars <- group_info[["dplyr_groups"]]
  extra_groups <- group_info[["extra_groups"]]
  all_groups <- group_info[["all_groups"]]
  sort_nm <- new_var_nm(names(out), ".sort.index")
  out[, (sort_nm) := seq_len(.N)] # Index variable
  time_col <- tidy_transform_names(data, !!enquo(time))
  window_col <- tidy_transform_names(data, !!enquo(window))
  if (length(time_col) == 0){
    stop("Please supply date or datetime for episode calculation")
  }
  if (length(window_col) == 0){
    stop("Please supply window for episode calculation")
  }
  time_by <- time_by_get(out[[time_col]], by = by)
  # Create grouped ID variable
  grp_nm <- new_var_nm(out, ".group")
  out[, (grp_nm) := group_id(data, .cols = extra_groups,
                             .by = {{ .by }}, as_qg = TRUE)]
  # Sort by groups > case ID > date col
  setorderv2(out, cols = c(grp_nm, time_col))
  # out <- farrange(out, .cols = c(grp_nm, time_col))
  g <- collapse::GRP(out[[grp_nm]], sort = TRUE, call = FALSE,
                     return.groups = FALSE)
  lag <- min(N, 1L) # Bound lag to >= 0
  date_lag_nm <- new_var_nm(names(out), "date_lag")
  out[, (date_lag_nm) := collapse::flag(.SD,
                                        n = lag,
                                        g = g),
      .SDcols = time_col]
  time_elapsed_nm <- new_var_nm(names(out), "time_elapsed")
  out[, (time_elapsed_nm) := time_diff(get(date_lag_nm), get(time_col),
                                                by = time_by, type = type)]
  # Row IDs per subject
  subject_row_id_nm <- new_var_nm(names(out), "subject_row_id")
  out[, (subject_row_id_nm) := gseq_len(N, g = g)]
  # Binary variable indicating if new episode or not
  new_episode_nm <- new_var_nm(names(out), "new_episode")
  out[, (new_episode_nm) := data.table::fifelse(get(subject_row_id_nm) == 1 |
                                                 get(time_elapsed_nm) >=
                                                 get(window_col),
                                               1L, 0L)]
  # Episode ID at record level
  episode_id_nm <- new_var_nm(names(out), "episode_id")
  out[, (episode_id_nm) := fcumsum(.SD,
                                   g = g,
                                   na.rm = FALSE),
      .SDcols = new_episode_nm]
  # Episode ID at group level
  episode_id_group_nm <- new_var_nm(names(out), "episode_id_group")
  out[, (episode_id_group_nm) := data.table::fifelse(get(new_episode_nm) == 1L,
                                                    get(episode_id_nm), 0L)]
  # Add episode start dates
  # Get min episode dates for each subject + episode
  episode_start_nm <- new_var_nm(names(out), "episode_start")
  out[, (episode_start_nm) := gmin(get(time_col),
                                   g = .SD,
                                   na.rm = TRUE),
                                   .SDcols = c(all_groups, episode_id_nm)]
  # Remove uneccessary cols
  set_rm_cols(out, c(date_lag_nm, subject_row_id_nm, new_episode_nm, grp_nm))
  # Newly added cols
  new_cols <- setdiff(names(out), names(data))
  if (.add){
    # Sort by initial order
    data.table::setorderv(out, cols = sort_nm)
    # out <- farrange(out, .cols = sort_nm)
  }
  # Remove sort ID col
  set_rm_cols(out, sort_nm)
  # Output names
  out_nms <- c(names(data), setdiff(names(out), names(data)))
  # If window col was a variable in data, then don't remove it
  if (!window_col %in% start_nms){
    set_rm_cols(out, window_col)
    out_nms <- setdiff(out_nms, window_col)
  }
  # Make sure output names start with group vars
  out_nms <- c(all_groups, setdiff(out_nms, all_groups))
  # Set the column order
  data.table::setcolorder(out, out_nms)
  df_reconstruct(out, data)
}
