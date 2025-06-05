#' Get summary statistics of time delay
#'
#' @description The output is a `list` containing summary statistics of time delay between two date/datetime vectors.
#' This can be especially useful in estimating reporting delay for example.
#' * \bold{data} - A data frame containing the origin, end and calculated time delay.
#' * \bold{unit} - The chosen time unit.
#' * \bold{num} - The number of time units.
#' * \bold{summary} - `tibble` with summary statistics.
#' * \bold{delay} - `tibble` containing the empirical cumulative distribution function
#'  values by time delay.
#' * \bold{plot} - A `ggplot` of the time delay distribution.
#'
#' @param data A data frame.
#' @param origin Origin date variable.
#' @param end End date variable.
#' @param timespan [timespan].
#' @param min_delay The minimum acceptable delay,
#' all delays less than this are removed before calculation.
#' Default is `min_delay = -Inf`.
#' @param max_delay The maximum acceptable delay,
#' all delays greater than this are removed before calculation.
#' Default is `max_delay = Inf`.
#' @param probs Probabilities used in the quantile summary.
#' Default is `probs = c(0.25, 0.5, 0.75, 0.95)`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param include_plot Should a `ggplot` graph of delay distributions be included in the output?
#' @param x_scales Option to control how the x-axis is displayed for multiple facets.
#' Choices are "fixed" or "free_x".
#' @param bw The smoothing bandwidth selector for the Kernel Density estimator.
#' If numeric, the standard deviation of the smoothing kernel.
#' If character, a rule to choose the bandwidth. See `?stats::bw.nrd` for more details.
#' The default has been set to "SJ" which implements the Sheather & Jones (1991) method,
#' as recommended by the R team `?stats::density`.
#' This differs from the default implemented by `stats::density()`
#' which uses Silverman's rule-of-thumb.
#' @param ... Further arguments to be passed on to `ggplot2::geom_density()`.
#'
#' @returns
#' A list containing summary data, summary statistics and an optional `ggplot`.
#'
#' @examples
#' library(timeplyr)
#' library(outbreaks)
#' library(dplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' ebola_linelist <- ebola_sim_clean$linelist
#'
#' # Incubation period distribution
#'
#' # 95% of individuals experienced an incubation period of <= 26 days
#' inc_distr_days <- ebola_linelist |>
#'   get_time_delay(date_of_infection,
#'                  date_of_onset,
#'                  time = "days")
#' head(inc_distr_days$data)
#' inc_distr_days$unit
#' inc_distr_days$num
#' inc_distr_days$summary
#' head(inc_distr_days$delay) # ECDF and freq by delay
#' inc_distr_days$plot
#'
#' # Can change bandwidth selector
#' inc_distr_days <- ebola_linelist |>
#'   get_time_delay(date_of_infection,
#'                  date_of_onset,
#'                  time = "day",
#'                  bw = "nrd")
#' inc_distr_days$plot
#'
#' # Can choose any time units
#' inc_distr_weeks <- ebola_linelist |>
#'   get_time_delay(date_of_infection,
#'                  date_of_onset,
#'                  time = "weeks",
#'                  bw = "nrd")
#' inc_distr_weeks$plot
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
get_time_delay <- function(data, origin, end, timespan = 1L,
                           min_delay = -Inf, max_delay = Inf,
                           probs = c(0.25, 0.5, 0.75, 0.95),
                           .by = NULL,
                           include_plot = TRUE, x_scales = "fixed",
                           bw = "sj",
                           ...){
  group_vars <- get_groups(data, {{ .by }})
  origin_data <- mutate_one(
    data, !!enquo(origin), .by = {{ .by }}
  )
  end_data <- mutate_one(
    data, !!enquo(end), .by = {{ .by }}
  )
  origin <- names(origin_data)
  end <- names(end_data)
  check_length(origin, 1)
  check_length(end, 1)
  start_time <- origin
  end_time <- end
  out <- fastplyr::f_bind_cols(
    fastplyr::f_ungroup(
      fastplyr::f_select(data, .cols = group_vars)
    ), origin_data, end_data
  )
  grp_nm <- unique_col_name(out, ".group.id")
  out <- cheapr::df_modify(out, add_names(list(
    fastplyr::add_group_id(data, .by = {{ .by }}, .name = grp_nm)[[grp_nm]]
    ), grp_nm))
  out <- df_rm_cols(out, setdiff(names(out), c(grp_nm, group_vars, start_time, end_time)))
  grp_df <- fastplyr::f_distinct(
    fastplyr::f_select(out, .cols = c(grp_nm, group_vars)),
    .cols = grp_nm,
    .keep_all = TRUE
  )
  timespan <- timespan(timespan)
  by_unit <- timespan_unit(timespan)
  by_n <- timespan_num(timespan)
  delay_nm <- unique_col_name(out, "delay")
  out <- cheapr::df_modify(out, add_names(
    list(
      time_diff(out[[start_time]],
                out[[end_time]], timespan)
    ),
    delay_nm
  ))
  n_miss_delays <- cheapr::na_count(out[[delay_nm]])
  if (n_miss_delays > 0){
    cli::cli_warn(paste(n_miss_delays, "missing observations will be
                  removed before calculation.",
                  sep = " "))
  }
  # Remove outliers
  out <- sset(out, data.table::between(out[[delay_nm]], min_delay, max_delay,
                                       incbounds = TRUE, NAbounds = NA))
  # Quantile summary
  iqr_p_missed <- setdiff(c(0.25, 0.75), probs)
  if (length(iqr_p_missed) > 0L){
    for (iqr_p in iqr_p_missed){
      probs <- c(probs, iqr_p)
    }
  }
  sd <- stats::sd
  summary_stats_df <- fastplyr::f_summarise(
    out,
    n = dplyr::n(),
    dplyr::across(
      dplyr::all_of(delay_nm),
      list(min = min,
           max = max,
           mean = mean,
           sd = sd)
    ),
    .order = FALSE,
    .by = dplyr::all_of(grp_nm)
  )

  quantiles_df <- fastplyr::tidy_quantiles(
    out, .cols = delay_nm,
    .order = FALSE,
    .by = dplyr::all_of(grp_nm),
    pivot = "wide",
    probs = c(0.05, 0.25, 0.5, 0.75, 0.95)
  )

  summary_stats_df <- fastplyr::f_bind_cols(
    summary_stats_df,
    dplyr::select(quantiles_df, -dplyr::all_of(grp_nm))
  )

  summary_stats_df[["iqr"]] <- summary_stats_df[["p75"]] - summary_stats_df[["p25"]]
  summary_stats_df[["se"]] <- summary_stats_df[["sd"]] / sqrt(summary_stats_df[["n"]])

  if (length(group_vars) > 0L){
    summary_stats_df <- fastplyr::f_left_join(
      grp_df, summary_stats_df, by = grp_nm
    )

  }
  summary_stats_df <- fastplyr::f_arrange(summary_stats_df, .cols = grp_nm)
  summary_stats_df <- df_rm_cols(summary_stats_df, c(grp_nm, iqr_p_missed))
  # Create delay table
  min_delay <- max(min(out[[delay_nm]]), min_delay)
  min_delay <- min_delay[!is.infinite(min_delay)]
  max_delay <- min(max(out[[delay_nm]]), max_delay)
  max_delay <- max_delay[!is.infinite(max_delay)]
  if (length(min_delay) == 0 || length(max_delay) == 0){
    delay_tbl <- fastplyr::new_tbl(
      delay = numeric(),
      n = integer(),
      cumulative = integer(),
      edf = numeric()
    )
  } else {
    delay_tbl <- out |>
      fastplyr::f_count(across(all_of(c(grp_nm, group_vars))),
             across(all_of(delay_nm), ceiling),
             name = "n")

    delay_tbl[["cumulative"]] <- collapse::fcumsum(
      delay_tbl[["n"]],
      g = delay_tbl[[grp_nm]],
      na.rm = TRUE
    )
    delay_tbl[["edf"]] <- delay_tbl[["cumulative"]] /
      gsum(
        delay_tbl[["n"]],
        g = delay_tbl[[grp_nm]],
        na.rm = FALSE,
        fill = TRUE
      )
    delay_tbl <- df_rm_cols(delay_tbl, setdiff(names(delay_tbl),
                                   c(group_vars, delay_nm,
                                     "n", "cumulative", "edf")))
  }
  out <- df_rm_cols(out, grp_nm)
  out <- fastplyr::f_select(out, .cols = c(group_vars, setdiff(names(out), group_vars)))
  out <- cheapr::rebuild(out, data)
  delay_summary <- cheapr::rebuild(summary_stats_df, data)
  delay_tbl <- cheapr::rebuild(delay_tbl, data)
  # Delay values
  delay_list <- list("data" = out,
                     "units" = by_unit,
                     "num" = by_n,
                     "summary" = summary_stats_df,
                     "delay" = delay_tbl)
  if (include_plot){
    x_scales <- match.arg(x_scales, c("fixed", "free_x"))
    # Control x-axis plot text
    if (by_n != 1){
      if (!timespan_has_unit(timespan)){
        plot_unit_text <- paste0("/", by_n)
      }
      else {
        plot_unit_text <- paste0("(/", by_n, " ", by_unit, ")")
      }
    }
    else {
      if (!timespan_has_unit(timespan)){
        plot_unit_text <- ""
      }
      else {
        plot_unit_text <- paste0("(", by_unit, ")")
      }
    }
    # Custom number format
    num_fmt <- function(x, drop_leading_zeros = TRUE, ...){
      out <- formatC(x, ...)
      if (drop_leading_zeros) out <- drop_leading_zeros(out)
      out
    }
    delay_summary_plot <- out |>
      ggplot2::ggplot(ggplot2::aes(x = .data[[delay_nm]])) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(ndensity)),
                              binwidth = 1,
                              fill = "white", col = "black", alpha = 0.75) +
      ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(scaled)),
                            fill = "#3F3685", alpha = 0.3,
                            bw = bw, ...) +
      ggplot2::stat_ecdf(col = "#0078D4") +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_continuous(n.breaks = 8,
                                  labels = function(x){
                                    num_fmt(x, digits = 2, big.mark = ",",
                                            drop_leading_zeros = FALSE,
                                            format = "f", flag = "",
                                            drop0trailing = TRUE)
                                  }) +
      ggplot2::scale_y_continuous(n.breaks = 5,
                                  labels = function(x){
                                    num_fmt(x, digits = 2, big.mark = ",",
                                            drop_leading_zeros = TRUE,
                                            format = "fg", flag = "#",
                                            drop0trailing = TRUE)
                                  }) +
      ggplot2::labs(x = paste("Delay", plot_unit_text, sep = " "),
                    y = "Normalized density and ECDF",
                    title = paste0("Empirical distribution of time delay\nbetween ",
                                   start_time,
                                   " and ",
                                   end_time))
    if (length(group_vars) > 0){
      delay_summary_plot <- delay_summary_plot +
        ggplot2::facet_wrap(group_vars, scales = x_scales)
    }
    delay_list <- c(delay_list, list("plot" = delay_summary_plot))
  }
  return(delay_list)
}
