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
#' @param type Time difference type, auto, duration, period.
#' @param .add Should episodic variables be added to the data? If `FALSE` (the default),
#' then only the relevant variables are returned, ordered by groups + time.
#' If `TRUE`, the episodic variables are added to the original data, in the order of
#' the original data.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @export
time_episodes <- function(data, ..., time, window,
                          by = NULL, type = c("auto", "duration", "period"),
                          .add = FALSE, .by = NULL){
  N <- nrow2(data)
  if (.add){
    .keep <- "all"
  } else {
    .keep <- "used"
  }
  start_nms <- data.table::copy(names(data))
  data <- data %>%
    dplyr::mutate(!!!enquos(...),
                  !!enquo(time),
                  !!enquo(window),
                  .by = {{ .by }},
                  .keep = .keep)
  # Copy data frame
  out <- data.table::copy(data)
  # Coerce to data table
  data.table::setDT(out)
  group_info <- get_group_info(data, !!!enquos(...), .by = {{ .by }},
                               type = "data-mask")
  group_vars <- group_info[["dplyr_groups"]]
  extra_groups <- group_info[["extra_groups"]]
  all_groups <- group_info[["all_groups"]]
  sort_nm <- new_var_nm(names(out), ".sort.index")
  out[, (sort_nm) := seq_len(.N)] # Index variable
  time_col <- tidy_transform_names(safe_ungroup(data), !!enquo(time))
  window_col <- tidy_transform_names(safe_ungroup(data), !!enquo(window))
  if (length(time_col) == 0){
    stop("Please supply date or datetime for episode calculation")
  }
  if (length(window_col) == 0){
    stop("Please supply window for episode calculation")
  }
  if (is.null(by)){
    unit_info <- time_granularity(data[[time_col]], is_sorted = FALSE)
    by_n <- unit_info[["num"]]
    by_unit <- unit_info[["unit"]]
  } else {
    unit_info <- unit_guess(by)
    by_n <- unit_info[["num"]] * unit_info[["scale"]]
    by_unit <- unit_info[["unit"]]
  }
  by <- setnames(list(by_n), by_unit)
  # Create grouped ID variable
  grp_nm <- new_var_nm(out, ".group")
  out[, (grp_nm) := group_id(data, all_of(extra_groups), .by = {{ .by }},
                             sort = TRUE, as_qg = FALSE)]
  # Sort by groups > case ID > date col
  data.table::setorderv(out, cols = c(grp_nm, time_col), na.last = TRUE)
  g <- collapse::GRP(out[[grp_nm]], sort = TRUE, call = FALSE, return.groups = FALSE)
  lag <- min(N, 1L) # Bound lag to >= 0
  date_lag_nm <- new_var_nm(names(out), "date_lag")
  out[, (date_lag_nm) := collapse::flag(get(time_col),
                                       n = lag,
                                       g = g)]
  time_elapsed_nm <- new_var_nm(names(out), "time_elapsed")
  out[, (time_elapsed_nm) := time_diff(get(date_lag_nm), get(time_col),
                                                by = by, type = type)]
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
  out[, (episode_id_nm) := collapse::fcumsum(get("new_episode"),
                                             g = g,
                                             na.rm = FALSE)]
  # Episode ID at group level
  episode_id_group_nm <- new_var_nm(names(out), "episode_id_group")
  out[, (episode_id_group_nm) := data.table::fifelse(get(new_episode_nm) == 1L,
                                                    get(episode_id_nm), 0L)]
  # Add episode start dates
  # Get min episode dates for each subject + episode
  episode_start_nm <- new_var_nm(names(out), "episode_start")
  out[, (episode_start_nm) := gmin(get(time_col),
                                   g = mget(c(all_groups, episode_id_nm)),
                                   na.rm = TRUE)]
  # Remove uneccessary cols
  set_rm_cols(out, c(date_lag_nm, subject_row_id_nm, new_episode_nm, grp_nm))
  # Newly added cols
  new_cols <- setdiff(names(out), names(data))
  if (.add){
    # Sort by initial order
    data.table::setorderv(out, cols = sort_nm)
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