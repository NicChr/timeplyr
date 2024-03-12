time_summarise <- function(data, time = NULL, ..., time_by = NULL,
                           from = NULL, to = NULL,
                           .name = "{.col}",
                           time_type = getOption("timeplyr.time_type", "auto"),
                           .by = NULL,
                           time_floor = FALSE,
                           week_start = getOption("lubridate.week.start", 1),
                           roll_month = getOption("timeplyr.roll_month", "preday"),
                           roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                           as_interval = getOption("timeplyr.use_intervals", FALSE),
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
time_count <- function(data, time = NULL, ..., time_by = NULL,
                       from = NULL, to = NULL,
                       .name = "{.col}",
                       complete = FALSE,
                       wt = NULL, name = NULL,
                       sort = FALSE,
                       .by = NULL,
                       time_floor = FALSE,
                       week_start = getOption("lubridate.week.start", 1),
                       time_type = getOption("timeplyr.time_type", "auto"),
                       roll_month = getOption("timeplyr.roll_month", "preday"),
                       roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                       as_interval = getOption("timeplyr.use_intervals", FALSE)){
  original_groups <- get_groups(data, {{ .by }})
  by_groups <- tidy_select_pos(data, {{ .by }})
  # Determine common bounds
  from_info <- mutate_summary_grouped(data, !!enquo(from), .keep = "none")
  to_info <- mutate_summary_grouped(data, !!enquo(to), .keep = "none")
  from_var <- from_info[["cols"]]
  to_var <- to_info[["cols"]]
  check_length_lte(from_var, 1)
  check_length_lte(to_var, 1)
  from_data <- fselect(safe_ungroup(from_info[["data"]]), .cols = from_var)
  to_data <- fselect(safe_ungroup(to_info[["data"]]), .cols = to_var)
  out <- fgroup_by(data, ..., order = FALSE, .add = TRUE,
                   .by = {{ .by }})
  group_vars <- group_vars(out)
  extra_groups <- setdiff(group_vars, original_groups)
  # # #
  from_nm <- character()
  if (length(from_var) > 0){
    out <- df_add_cols(out, add_names(list(from_data[[from_var]]), from_var))
  }
  to_nm <- character()
  if (length(to_var) > 0){
    out <- df_add_cols(out, add_names(list(to_data[[to_var]]), to_var))
  }
  # Ungroup and use the .by to avoid reconstruction..
  out <- fcount(safe_ungroup(out), !!enquo(time), wt = !!enquo(wt),
                across(all_of(c(from_var, to_var))),
                order = FALSE,
                name = name,
                .by = all_of(group_vars))
  out <- fgroup_by(out, .cols = original_groups, order = FALSE)
  n_nm <- names(out)[length(names(out))]
  time_var <- setdiff2(names(out), c(group_vars, n_nm, from_var, to_var))
  time_agg_var <- across_col_names(time_var, .fns = "", .names = .name)
  if (!missing(complete) || complete){
    rlang::warn(c("x" = "'complete' has been deprecated and will not be applied",
                  "Please use `time_complete()`",
                  "",
                  "For example:",
                  "data %>%",
                  " " = paste0("time_count(", time_var, ")", " %>%"),
                  " " = paste0("time_complete(", time_var, ", fill = list(n = 0))")),
                use_cli_format = TRUE)
  }
  if (length(time_var) > 0){
    time_by <- time_by_get(out[[time_var]], time_by = time_by)
    groups <- df_to_GRP(out, .cols = original_groups, order = TRUE)
    if (length(from_var) == 0L){
      from_var <- new_var_nm(out, ".from")
      out <- df_add_cols(out, add_names(list(gmin(out[[time_var]], g = groups)),
                                        from_var))
    }
    if (length(to_var) == 0L){
      to_var <- new_var_nm(out, ".to")
      out <- df_add_cols(out, add_names(list(gmax(out[[time_var]], g = groups)),
                                        to_var))
    }
    out <- df_add_cols(out, add_names(list(time_cast(out[[from_var]],
                                                     out[[time_var]])),
                                      from_var))
    out <- df_add_cols(out, add_names(list(time_cast(out[[to_var]],
                                                     out[[time_var]])),
                                      to_var))

    time_agg <- time_aggregate_left(out[[time_var]],
                                    time_by = time_by,
                                    start = out[[from_var]],
                                    end = out[[to_var]],
                                    g = groups,
                                    time_type = time_type,
                                    roll_month = roll_month,
                                    roll_dst = roll_dst,
                                    time_floor = time_floor,
                                    week_start = week_start,
                                    as_interval = as_interval)
    out <- df_add_cols(out, add_names(list(time_agg), time_agg_var))
    groups2 <- df_to_GRP(safe_ungroup(out), .cols = c(original_groups, time_agg_var, extra_groups))
    counts <- collapse::fsum(out[[n_nm]], g = groups2, use.g.names = FALSE, na.rm = FALSE)
    group_start_locs <- GRP_starts(groups2)
    out <- df_row_slice(out, group_start_locs)
    out <- df_add_cols(out, add_names(list(counts), n_nm))
  }
  out <- fselect(out, .cols = c(original_groups, time_agg_var, extra_groups, n_nm))
  if (sort){
    out <- df_row_slice(out, radix_order(desc(out[[n_nm]])), reconstruct = FALSE)
  }
  if (length(by_groups) > 0 || sort){
    out <- df_reconstruct(out, data)
  }
  out
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
                          roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                          as_interval = getOption("timeplyr.use_intervals", FALSE),
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
                        roll_dst = getOption("timeplyr.roll_dst", "boundary"),
                        as_interval = getOption("timeplyr.use_intervals", FALSE)){
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
