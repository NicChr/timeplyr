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
#' @param time_by Must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
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
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' ebola_linelist <- ebola_sim_clean$linelist
#'
#' # Incubation period distribution
#'
#' # 95% of individuals experienced an incubation period of <= 26 days
#' inc_distr_days <- ebola_linelist %>%
#'   get_time_delay(date_of_infection,
#'                  date_of_onset,
#'                  time_by = "days")
#' head(inc_distr_days$data)
#' inc_distr_days$unit
#' inc_distr_days$num
#' inc_distr_days$summary
#' head(inc_distr_days$delay) # ECDF and freq by delay
#' inc_distr_days$plot
#'
#' # Can change bandwidth selector
#' inc_distr_days <- ebola_linelist %>%
#'   get_time_delay(date_of_infection,
#'                  date_of_onset,
#'                  time_by = "day",
#'                  bw = "nrd")
#' inc_distr_days$plot
#'
#' # Can choose any time units
#' inc_distr_weeks <- ebola_linelist %>%
#'   get_time_delay(date_of_infection,
#'                  date_of_onset,
#'                  time_by = "weeks",
#'                  bw = "nrd")
#' inc_distr_weeks$plot
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
get_time_delay <- function(data, origin, end, time_by = 1L,
                           time_type = getOption("timeplyr.time_type", "auto"),
                           min_delay = -Inf, max_delay = Inf,
                           probs = c(0.25, 0.5, 0.75, 0.95),
                           .by = NULL,
                           include_plot = TRUE, x_scales = "fixed",
                           bw = "sj",
                           ...){
  group_vars <- get_groups(data, {{ .by }})
  origin_info <- mutate_summary_grouped(data,
                                        !!enquo(origin),
                                        .by = {{ .by }},
                                        .keep = "none")
  end_info <- mutate_summary_grouped(data,
                                     !!enquo(end),
                                     .by = {{ .by }},
                                     .keep = "none")
  origin <- origin_info[["cols"]]
  end <- end_info[["cols"]]
  check_length(origin, 1)
  check_length(end, 1)
  start_time <- origin
  end_time <- end
  origin_df <- safe_ungroup(origin_info[["data"]])
  end_df <- fselect(safe_ungroup(end_info[["data"]]), .cols = end)
  out <- df_as_dt(df_cbind(origin_df, end_df))
  grp_nm <- new_var_nm(out, ".group.id")
  set_add_cols(out, add_names(list(group_id(data, .by = {{ .by }})), grp_nm))
  set_rm_cols(out, setdiff(names(out),
                           c(grp_nm, group_vars, start_time, end_time)))
  grp_df <- fdistinct(fselect(out, .cols = c(grp_nm, group_vars)),
                      .cols = grp_nm,
                      .keep_all = TRUE)
  time_by <- time_by_list(time_by)
  by_unit <- time_by_unit(time_by)
  by_n <- time_by_num(time_by)
  delay_nm <- new_var_nm(out, "delay")
  set_add_cols(out, add_names(
    list(
      time_diff(out[[start_time]],
                out[[end_time]],
                time_by = time_by,
                time_type = time_type)
    ),
    delay_nm
  ))
  n_miss_delays <- num_na(out[[delay_nm]])
  if (n_miss_delays > 0){
    warning(paste(n_miss_delays, "missing observations will be
                  removed before calculation.",
                  sep = " "))
  }
  # Remove outliers
  out <- out[which_(data.table::between(get(delay_nm), min_delay, max_delay,
                                 incbounds = TRUE, NAbounds = NA)), ]
  # Quantile summary
  iqr_p_missed <- setdiff(c(0.25, 0.75), probs)
  if (length(iqr_p_missed) > 0L){
    for (iqr_p in iqr_p_missed){
      probs <- c(probs, iqr_p)
    }
  }
  q_prcnts <- round(probs * 100)
  q_nms <- paste0(rep_len("p", length(probs)), q_prcnts)
  # Descriptive statistical summary
  delay_summary <- stat_summarise(out, .cols = delay_nm,
                                  .by = all_of(grp_nm),
                                  stat = c("n", "min", "max",
                                           "mean", "sd"),
                                  sort = FALSE,
                                  q_probs = probs,
                                  inform_stats = FALSE)
  delay_summary[, ("se") := get("sd")/sqrt(get("n"))]
  delay_summary[, ("iqr") := get("p75") - get("p25")]
  if (length(group_vars) > 0L){
    # Left-join
    delay_summary[grp_df, (group_vars) := mget(group_vars),
                  on = grp_nm, allow.cartesian = FALSE]
  }
  setorderv2(delay_summary, cols = grp_nm)
  set_rm_cols(delay_summary, c(grp_nm, iqr_p_missed))
  delay_summary <- fselect(delay_summary, .cols = c(group_vars, "n", "min",
                                                    "max", "mean", "sd",
                                                    q_nms, "iqr", "se"))
  # Create delay table
  min_delay <- max(min(out[[delay_nm]]), min_delay)
  min_delay <- min_delay[!is.infinite(min_delay)]
  max_delay <- min(max(out[[delay_nm]]), max_delay)
  max_delay <- max_delay[!is.infinite(max_delay)]
  if (length(min_delay) == 0 || length(max_delay) == 0){
    delay_tbl <- new_tbl(delay = numeric(),
                         n = integer(),
                         cumulative = integer(),
                         edf = numeric())
  } else {
    delay_tbl <- out %>%
      fcount(across(all_of(c(grp_nm, group_vars))),
             across(all_of(delay_nm), ceiling),
             name = "n")
    delay_tbl[, ("cumulative") := collapse::fcumsum(get("n"),
                                                    g = get(grp_nm),
                                                    na.rm = TRUE)]
    # delay_tbl[, ("edf") :=
    #             get("cumulative") / sum(get("n")), by = grp_nm]
    delay_tbl[, ("edf") :=
                get("cumulative") / gsum(get("n"),
                                         g = get(grp_nm),
                                         na.rm = FALSE,
                                         fill = TRUE)]
    # delay_tbl[, ("edf") := edf(get(delay_nm),
    #                            g = get(grp_nm),
    #                            wt = get("n"))]
    set_rm_cols(delay_tbl, setdiff(names(delay_tbl),
                                   c(group_vars, delay_nm,
                                     "n", "cumulative", "edf")))
  }
  set_rm_cols(out, grp_nm)
  out <- fselect(out, .cols = c(group_vars, setdiff(names(out), group_vars)))
  out <- df_reconstruct(out, data)
  delay_summary <- df_reconstruct(delay_summary, data)
  delay_tbl <- df_reconstruct(delay_tbl, data)
  # Delay values
  delay_list <- list("data" = out,
                     "units" = by_unit,
                     "num" = by_n,
                     "summary" = delay_summary,
                     "delay" = delay_tbl)
  if (include_plot){
    x_scales <- match.arg(x_scales, c("fixed", "free_x"))
    # Control x-axis plot text
    if (by_n != 1){
      if (by_unit == "numeric"){
        plot_unit_text <- paste0("/", by_n)
      }
      else {
        plot_unit_text <- paste0("(/", by_n, " ", by_unit, ")")
      }
    }
    else {
      if (by_unit == "numeric"){
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
    delay_summary_plot <- out %>%
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
