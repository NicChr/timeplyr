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
#' @param time_type Time type, either "auto", "duration" or "period".
#' With larger data, it is recommended to use `time_type = "duration"` for
#' speed and efficiency.
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
#' To perform simple time-since-event analysis, set `window` to `1`, which is
#' the default.
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
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
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
#'   time_count(time = ep_start, time_by = "week", time_floor = TRUE) %>%
#'   ggplot(aes(x = ep_start, y = n)) +
#'   geom_bar(stat = "identity")
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
time_episodes <- function(data, time, time_by = NULL,
                          window = 1,
                          roll_episode = TRUE,
                          switch_on_boundary = TRUE,
                          fill = 0,
                          .add = FALSE,
                          event = NULL,
                          time_type = c("auto", "duration", "period"),
                          .by = NULL){
  rlang::check_required(time)
  N <- df_nrow(data)
  if (window < 0){
    stop("window must be strictly greater or equal to 0")
  }
  start_nms <- names(data)
  time_quo <- enquo(time)
  group_vars <- get_groups(data, .by = {{ .by }})
  time_col <- tidy_select_names(data, !!time_quo)
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
    data <- dplyr::dplyr_col_modify(data, cols = add_names(list(
      data.table::fifelse(fpluck(data, event_col) %in%
                            event[[1L]],
                          1L, 0L)
    ), event_id_nm))
    if (num_na(fpluck(data, event_col)) != num_na(fpluck(data, time_col))){
      warning(paste0("There is a mismatch of NAs between ",
                     time_col, " and ",
                     event_col, ", please check."))
    }
  }
  out <- fselect(data, .cols = c(group_vars, time_col,
                                 event_col, event_id_nm))
  out <- as_DT(out)
  if (length(time_col) == 0){
    stop("Please supply date or datetime for episode calculation")
  }
  if (length(window) != 1){
    stop("Please supply one window for episode calculation")
  }
  time_by <- time_by_get(fpluck(out, time_col), time_by = time_by)
  # Create group ID variable
  grp_nm <- new_var_nm(data_nms, ".group")
  data.table::set(out,
                  j = grp_nm,
                  value = group_id(data, .by = {{ .by }},
                                   as_qg = FALSE, order = TRUE))
  # Group by group vars + time
  g2 <- collapse::GRP(fselect(out, .cols = c(grp_nm, time_col)),
                      sort = TRUE)
  data_is_sorted <- GRP_is_sorted(g2)
  # If data is already sorted correctly, no need to sort it
  if (!data_is_sorted){
    out <- df_row_slice(out, GRP_order(g2))
  }
  # # Group info
  g <- collapse::GRP(fpluck(out, grp_nm))

  ### Episode calculation ###
  # Calculation by reference (data.table set notation)
  calc_episodes(out, time = time_col,
                time_by = time_by,
                time_type = time_type,
                switch_on_boundary = switch_on_boundary,
                g = g,
                gid = grp_nm,
                event = event_id_nm,
                window = window,
                roll_episode = roll_episode,
                fill = fill)
  # Newly added episodic columns
  new_cols <- c("t_elapsed", "ep_start", "ep_id", "ep_id_new")
  set_rm_cols(out, grp_nm)
  # Sort by initial order
  if (!data_is_sorted){
    out <- df_reorder(out, g = g2)
  }
  if (.add){
    # Simply bind the cols together
    dplyr::bind_cols(data, fselect(out, .cols = new_cols))
  } else {
    # Only keep the key variables
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
                          time_type, # time_type (duration/period)
                          switch_on_boundary,
                          g, # GRP object
                          gid, # group id col
                          event, # Event col
                          window, # Window col
                          roll_episode, # Should episode calc be rolling?
                          fill){ # How to fill first time elapsed for rolling calc
  N <- df_nrow(data)
  lag <- min(N, 1L) # Bound lag to >= 0
  time_na <- na_init(fpluck(data, time)) # time NA with correct class
  time_num <- time_by_num(time_by)
  time_unit <- time_by_unit(time_by)
  # time_threshold <- add_names(list(time_num * window), time_unit)

  ##### (More efficient) METHOD assuming all rows are events #####

  if (length(event) == 0L){
    time_lag_nm <- character(0)
    time_lag_nm2 <- character(0)
    # Time elapsed
    data.table::set(data,
                    j = "t_elapsed",
                    value = time_elapsed(fpluck(data, time), g = g,
                                         time_by = time_by,
                                         fill = fill,
                                         time_type = time_type,
                                         rolling = roll_episode,
                                         na_skip = TRUE))
    # Binary variable indicating if new episode or not
    # The first event is always a new episode
    # Events where t_elapsed >= window are new episodes
    data.table::set(data,
                    j = "ep_id",
                    value = time_seq_id(fpluck(data, time),
                                        g = g,
                                        time_by = time_by,
                                        threshold = window,
                                        time_type = time_type,
                                        rolling = roll_episode,
                                        switch_on_boundary = switch_on_boundary,
                                        na_skip = TRUE))
    g3 <- collapse::GRP(fselect(data, .cols = c(gid, "ep_id")))
    g3_starts <- GRP_starts(g3)
    data.table::set(data,
                    j = "ep_id_new",
                    value = 0L)
    data.table::set(data,
                    i = g3_starts,
                    j = "ep_id_new",
                    value = fpluck(data, "ep_id")[g3_starts])
    data.table::set(data,
                    i = collapse::whichNA(fpluck(data, "ep_id")),
                    j = "ep_id_new",
                    value = NA_integer_)
    # Add episode start dates
    # Get min episode dates for each subject + episode
    data.table::set(data,
                    j = "ep_start",
                    value = gfirst(fpluck(data, time),
                                   g = g3,
                                   na.rm = FALSE))
  } else {

    ##### METHOD for data with a mix of event and non-event rows #####
    time_lag_nm <- new_var_nm(data, "date_lag")
    time_lag_nm2 <- new_var_nm(c(names(data), time_lag_nm), "date_lag2")
    # group_row_id_nm <- character(0)
    is_event <- fpluck(data, event) == 1L # Logical
    which_is_event <- cpp_which(is_event)
    which_not_event <- cpp_which(!is_event)
    event_data <- df_row_slice(data, which_is_event)
    event_groups <- collapse::GRP(GRP_group_id(g)[which_is_event])
    which_time_na <- collapse::whichNA(fpluck(data, time)) # Which time are NA
    # Initialise lagged time as NA
    data.table::set(data,
                    j = time_lag_nm,
                    value = time_na)
    if (roll_episode){
      data.table::set(data,
                      i = which_is_event,
                      j = time_lag_nm,
                      value = flag2(fpluck(event_data, time),
                                    g = event_groups))
      # g = fpluck(event_data, gid)))
    } else {
      data.table::set(data,
                      i = which_is_event,
                      j = time_lag_nm,
                      value = gfirst(fpluck(event_data, time),
                                     g = event_groups,
                                     # g = fpluck(event_data, gid),
                                     na.rm = TRUE))
    }
    # Replace the first NA values (for event rows) with time
    which_replace_na <- cpp_which(is_event & !is.na(fpluck(data, time)) &
                                    is.na(fpluck(data, time_lag_nm)))
    data.table::set(data,
                    i = which_replace_na,
                    j = time_lag_nm,
                    value = fpluck(data, time)[which_replace_na])
    # LOCF NA fill
    data.table::set(data,
                    j = time_lag_nm2,
                    value = data.table::fifelse(is_event,
                                                fpluck(data, time),
                                                time_na))
    data.table::set(data,
                    j = time_lag_nm2,
                    value = roll_na_fill(fpluck(data, time_lag_nm2),
                                         g = g))

    # Time elapsed
    data.table::set(data,
                    i = which_is_event,
                    j = "t_elapsed",
                    value = time_elapsed(fpluck(event_data, time),
                                         g = event_groups,
                                         time_by = time_by,
                                         fill = fill,
                                         time_type = time_type,
                                         rolling = roll_episode,
                                         na_skip = TRUE))
    data.table::set(data,
                    i = which_not_event,
                    j = "t_elapsed",
                    value = time_diff(fpluck(df_row_slice(data, which_not_event),
                                             time_lag_nm2),
                                      fpluck(df_row_slice(data, which_not_event),
                                             time),
                                      time_by = time_by,
                                      time_type = time_type))
    # Initialise episode ID
    data.table::set(data,
                    j = "ep_id",
                    value = NA_integer_)
    data.table::set(data,
                    i = which_is_event,
                    j = "ep_id",
                    value = time_seq_id(fpluck(event_data, time),
                                        fpluck(event_data, gid),
                                        time_by = time_by,
                                        threshold = window,
                                        time_type = time_type,
                                        rolling = roll_episode,
                                        switch_on_boundary = switch_on_boundary,
                                        na_skip = TRUE))
    event_data <- df_row_slice(data, which_is_event)
    g3 <- collapse::GRP(fselect(event_data,
                                .cols = c(gid, "ep_id")))
    g3_starts <- GRP_starts(g3)
    # Initialise new episode ID
    data.table::set(data,
                    j = "ep_id_new",
                    value = NA_integer_)
    data.table::set(data,
                    i = which_is_event,
                    j = "ep_id_new",
                    value = data.table::fifelse(df_seq_along(event_data) %in%
                                                  g3_starts,
                                                fpluck(event_data, "ep_id"),
                                                0L))
    # Initialise episode start date
    data.table::set(data,
                    j = "ep_start",
                    value = time_na)
    # Add episode start dates
    # Get min episode dates for each subject + episode
    data.table::set(data,
                    i = which_is_event,
                    j = "ep_start",
                    value = gfirst(fpluck(event_data, time),
                                   g = g3,
                                   na.rm = TRUE))

  }
  # Remove unnecessary cols
  set_rm_cols(data, c(time_lag_nm, time_lag_nm2, event))
}
