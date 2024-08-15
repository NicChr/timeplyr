time_summarise <- function(data, time = NULL, ..., time_by = NULL,
                           from = NULL, to = NULL,
                           .name = "{.col}",
                           time_type = getOption("timeplyr.time_type", "auto"),
                           .by = NULL,
                           time_floor = FALSE,
                           week_start = getOption("lubridate.week.start", 1),
                           roll_month = getOption("timeplyr.roll_month", "preday"),
                           roll_dst = getOption("timeplyr.roll_dst", "NA"),
                           as_interval = getOption("timeplyr.use_intervals", TRUE),
                           sort = TRUE){
  check_is_df(data)
  group_vars <- get_groups(data, {{ .by }})
  temp_data <- data
  if (length(group_vars(data)) == 0){
    temp_data <- fgroup_by(temp_data, .by = {{ .by }}, order = FALSE)
  }
  group_ids <- df_group_id(temp_data)
  time_info <- mutate_summary_grouped(temp_data, !!enquo(time))
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
                       .cols = which_(match(names(from_data), names(time_data), 0L) == 0L))
  to_data <- fselect(to_data,
                     .cols = which_(match(names(to_data), names(time_data), 0L) == 0L))
  temp_data <- df_cbind(time_data, from_data, to_data)
  # Add variable to keep track of group IDs
  grp_nm <- new_var_nm(temp_data, ".group.id")
  temp_data <- df_add_cols(temp_data,
                           cols = add_names(list(group_ids), grp_nm))
  if (length(time_var) > 0L){
    # User supplied unit
    if (!is.null(time_by)){
      time_by <- time_by_list(time_by)
    } else {
      # Function to determine implicit time units
      granularity <- time_granularity(temp_data[[time_var]], msg = FALSE)
      message(paste("Assuming a time granularity of",
                    granularity[["num"]] / granularity[["scale"]],
                    granularity[["granularity"]], sep = " "))
      time_by <- add_names(list(granularity[["num"]]), granularity[["unit"]])
    }
    # Aggregate time data
    time_agg <- time_aggregate_left(temp_data[[time_var]],
                                    time_by = time_by,
                                    g = temp_data[[grp_nm]],
                                    start = fpluck(temp_data, from_var),
                                    end = fpluck(temp_data, to_var),
                                    time_type = time_type,
                                    roll_month = roll_month,
                                    roll_dst = roll_dst,
                                    time_floor = time_floor,
                                    week_start = week_start,
                                    as_interval = as_interval)
    time_var <- across_col_names(time_var, .fns = "", .names = .name)
    temp_data <- df_add_cols(temp_data,
                             add_names(
                               list(time_agg), time_var
                             )
    )
  }
  temp_data <- df_rm_cols(temp_data, grp_nm)
  out <- dplyr::summarise(safe_ungroup(temp_data),
                          ...,
                          .by = all_of(c(group_vars, time_var)))
  if (sort){
    out <- farrange(out, .cols = c(group_vars, time_var))
  }
  df_reconstruct(out, data)
}

time_distinct <- function(data, time = NULL, ..., time_by = NULL,
                          from = NULL, to = NULL,
                          .name = "{.col}",
                          .keep_all = FALSE,
                          time_type = getOption("timeplyr.time_type", "auto"),
                          .by = NULL,
                          time_floor = FALSE,
                          week_start = getOption("lubridate.week.start", 1),
                          roll_month = getOption("timeplyr.roll_month", "preday"),
                          roll_dst = getOption("timeplyr.roll_dst", "NA"),
                          as_interval = getOption("timeplyr.use_intervals", TRUE),
                          sort = FALSE){
  check_is_df(data)
  group_vars <- get_groups(data, {{ .by }})
  temp_data <- data
  if (length(group_vars(data)) == 0){
    temp_data <- fgroup_by(temp_data, .by = {{ .by }}, order = FALSE)
  }
  group_ids <- df_group_id(temp_data)
  time_info <- mutate_summary_grouped(temp_data, !!enquo(time))
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
                       .cols = which_(match(names(from_data), names(time_data), 0L) == 0L))
  to_data <- fselect(to_data,
                     .cols = which_(match(names(to_data), names(time_data), 0L) == 0L))
  temp_data <- df_cbind(time_data, from_data, to_data)
  # Add variable to keep track of group IDs
  grp_nm <- new_var_nm(temp_data, ".group.id")
  temp_data <- df_add_cols(temp_data,
                           cols = add_names(list(group_ids), grp_nm))
  if (length(time_var) > 0L){
    # User supplied unit
    if (!is.null(time_by)){
      time_by <- time_by_list(time_by)
    } else {
      # Function to determine implicit time units
      granularity <- time_granularity(temp_data[[time_var]], msg = FALSE)
      message(paste("Assuming a time granularity of",
                    granularity[["num"]] / granularity[["scale"]],
                    granularity[["granularity"]], sep = " "))
      time_by <- add_names(list(granularity[["num"]]), granularity[["unit"]])
    }
    # Aggregate time data
    time_agg <- time_aggregate_left(temp_data[[time_var]],
                                    time_by = time_by,
                                    g = temp_data[[grp_nm]],
                                    start = fpluck(temp_data, from_var),
                                    end = fpluck(temp_data, to_var),
                                    time_type = time_type,
                                    roll_month = roll_month,
                                    roll_dst = roll_dst,
                                    time_floor = time_floor,
                                    week_start = week_start,
                                    as_interval = as_interval)
    time_var <- across_col_names(time_var, .fns = "", .names = .name)
    temp_data <- df_add_cols(temp_data,
                             add_names(
                               list(time_agg), time_var
                             )
    )
  }
  temp_data <- df_rm_cols(temp_data, grp_nm)
  out <- fdistinct(safe_ungroup(temp_data),
                   across(dplyr::any_of(c(group_vars, time_var))),
                   ...,
                   .keep_all = .keep_all,
                   sort = sort)
  df_reconstruct(out, data)
}
time_mutate <- function(data, time = NULL, ..., time_by = NULL,
                        from = NULL, to = NULL,
                        .name = "{.col}",
                        time_type = getOption("timeplyr.time_type", "auto"),
                        .by = NULL,
                        .keep = c("all", "used", "unused", "none"),
                        time_floor = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        roll_month = getOption("timeplyr.roll_month", "preday"),
                        roll_dst = getOption("timeplyr.roll_dst", "NA"),
                        as_interval = getOption("timeplyr.use_intervals", TRUE)){
  check_is_df(data)
  has_groups <- length(group_vars(data)) > 0
  group_vars <- get_groups(data, {{ .by }})
  out <- data
  if (!has_groups){
    out <- fgroup_by(out, .by = {{ .by }}, order = FALSE)
  }
  group_ids <- df_group_id(out)
  time_info <- mutate_summary_grouped(out, !!enquo(time))
  from_info <- mutate_summary_grouped(out, !!enquo(from), .keep = "none")
  to_info <- mutate_summary_grouped(out, !!enquo(to), .keep = "none")
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
                       .cols = which_(match(names(from_data), names(time_data), 0L) == 0L))
  to_data <- fselect(to_data,
                     .cols = which_(match(names(to_data), names(time_data), 0L) == 0L))
  out <- df_cbind(time_data, from_data, to_data)
  # Add variable to keep track of group IDs
  grp_nm <- new_var_nm(out, ".group.id")
  out <- df_add_cols(out, cols = add_names(list(group_ids), grp_nm))
  if (length(time_var) > 0L){
    # User supplied unit
    if (!is.null(time_by)){
      time_by <- time_by_list(time_by)
    } else {
      # Function to determine implicit time units
      granularity <- time_granularity(out[[time_var]], msg = FALSE)
      message(paste("Assuming a time granularity of",
                    granularity[["num"]] / granularity[["scale"]],
                    granularity[["granularity"]], sep = " "))
      time_by <- add_names(list(granularity[["num"]]), granularity[["unit"]])
    }
    # Aggregate time data
    time_agg <- time_aggregate_left(out[[time_var]],
                                    time_by = time_by,
                                    g = out[[grp_nm]],
                                    start = fpluck(out, from_var),
                                    end = fpluck(out, to_var),
                                    time_type = time_type,
                                    roll_month = roll_month,
                                    roll_dst = roll_dst,
                                    time_floor = time_floor,
                                    week_start = week_start,
                                    as_interval = as_interval)
    time_agg_var <- across_col_names(time_var, .fns = "", .names = .name)
    out <- df_add_cols(
      out, add_names(
        list(time_agg), time_agg_var
      )
    )
  }
  out <- df_rm_cols(out, grp_nm)
  out <- mutate2(safe_ungroup(out),
                 ...,
                 .by = all_of(c(group_vars, time_agg_var)),
                 .keep = .keep)
  df_reconstruct(out, data)
}
