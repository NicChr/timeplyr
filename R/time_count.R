#' `time_count` is deprecated
#'
#' @param data \bold{Deprecated}.
#' @param time \bold{Deprecated}.
#' @param ... \bold{Deprecated}.
#' @param time_by \bold{Deprecated}.
#' @param from \bold{Deprecated}.
#' @param to \bold{Deprecated}.
#' @param .name \bold{Deprecated}.
#' @param complete \bold{Deprecated}.
#' @param wt \bold{Deprecated}.
#' @param name \bold{Deprecated}.
#' @param sort \bold{Deprecated}.
#' @param .by \bold{Deprecated}.
#' @param time_floor \bold{Deprecated}.
#' @param time_type \bold{Deprecated}.
#' @param week_start \bold{Deprecated}.
#' @param roll_month \bold{Deprecated}.
#' @param roll_dst \bold{Deprecated}.
#' @param as_interval \bold{Deprecated}.
#' @export
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
                       roll_dst = getOption("timeplyr.roll_dst", "NA"),
                       as_interval = getOption("timeplyr.use_intervals", FALSE)){
  warning("'time_count is deprecated.\nUse 'time_by' and 'count' instead.")
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
