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
#' @param data A data frame.
#' @param origin Origin date variable.
#' @param end End date variable.
#' @param by Argument to expand and summarise time series.
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
#' @param min_delay The minimum acceptable delay, all delays less than this are removed before calculation.
#' Default is `min_delay = -Inf`.
#' @param max_delay The maximum acceptable delay, all delays greater than this are removed before calculation.
#' Default is `max_delay = Inf`.
#' @param probs Probabilities used in the quantile summary. Default is `probs = c(0.25, 0.5, 0.75, 0.95)`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param include_plot Should a `ggplot` graph of delay distributions be included in the output?
#' @param x_scales Option to control how the x-axis is displayed for multiple facets.
#' Choices are "fixed" or "free_x".
#' @param bw The smoothing bandwidth selector for the Kernal Density estimator.
#' If numeric, the standard deviation of the smoothing kernel.
#' If character, a rule to choose the bandwidth. See `?stats::bw.nrd` for more details.
#' The default has been set to "SJ" which implements the Sheather & Jones (1991) method,
#' as recommended by the R team `?stats::density`.
#' This differs from the default implemented by `stats::density()`
#' which uses Silverman's rule-of-thumb.
#' @param ... Further arguments to be passed on to `ggplot2::geom_density()`.
#' @examples
#' library(timeplyr)
#' library(outbreaks)
#' library(dplyr)
#' library(purrr)
#' library(lubridate)
#'
#' ebola_linelist <- outbreaks::ebola_sim_clean$linelist
#'
#' # Incubation period distribution
#'
#' # 95% of individuals experienced an incubation period of <= 26 days
#' inc_distr_days <- ebola_linelist %>%
#'   get_time_delay(date_of_infection,
#'                  date_of_onset,
#'                  by = "days")
#' head(inc_distr_days$data) # Data with calculated delay
#' inc_distr_days$unit # # Specified time unit
#' inc_distr_days$num # # Specified time unit size
#' inc_distr_days$summary # Summary statistics
#' head(inc_distr_days$delay) # ECDF and freq by delay
#' inc_distr_days$plot # Plot of distribution
#'
#' # Can change bandwidth selector
#' ebola_linelist %>%
#'   get_time_delay(date_of_infection,
#'                  date_of_onset,
#'                  by = "day",
#'                  bw = "nrd") %>% # Scott's rule-of-thumb, normality assumption
#'   pluck("plot")
#'
#' # Can choose any time units
#' inc_distr_weeks <- ebola_linelist %>%
#'   get_time_delay(date_of_infection,
#'                  date_of_onset,
#'                  by = "weeks")
#' inc_distr_weeks$plot
#'
#' # Similar distribution by gender
#' ebola_linelist %>%
#'   group_by(gender) %>%
#'   get_time_delay(date_of_infection,
#'                  date_of_onset,
#'                  by = "day",
#'                  include_plot = FALSE) %>%
#'   pluck("summary")
#'
#' # Time from symptom onset to hospitalisation stratified by clinical outcome
#' ebola_linelist %>%
#'   group_by(outcome) %>%
#'   get_time_delay(date_of_onset,
#'                  date_of_hospitalisation,
#'                  by = "days",
#'                  include_plot = FALSE) %>%
#'   pluck("summary")
#'
#' # Those who died may have presented to hospital faster than those that recovered
#'
#' mers_cases <- outbreaks::mers_korea_2015$linelist
#'
#' # Reporting delay
#' mers_cases %>%
#'   get_time_delay(dt_onset, dt_report,
#'                  by = "days",
#'                  include_plot = FALSE) %>%
#'   pluck("summary")  # Mean of ~ 6 days
#' @export
get_time_delay <- function(data, origin, end, by = "day",
                           min_delay = -Inf, max_delay = Inf,
                           probs = c(0.25, 0.5, 0.75, 0.95),
                           .by = NULL,
                           include_plot = TRUE, x_scales = "fixed",
                           bw = "SJ",
                           ...){
  group_vars <- get_groups(data, {{ .by }})
  out <- data %>%
    dplyr::mutate(!!enquo(origin),
                  !!enquo(end),
                  .by = {{ .by }})
  start_time <- tidy_transform_names(safe_ungroup(data),
                                     !!enquo(origin))
  end_time <- tidy_transform_names(safe_ungroup(data),
                                   !!enquo(end))
  out <- data.table::copy(out)
  data.table::setDT(out)
  grp_nm <- new_var_nm(out, ".group.id")
  out[, (grp_nm) := group_id(data, .by = {{ .by }},
                             sort = TRUE)]
  set_rm_cols(out, setdiff(names(out),
                           c(grp_nm, group_vars, start_time, end_time)))
  grp_df <- collapse::funique(out[, c(grp_nm, group_vars),
                                  with = FALSE],
                              cols = grp_nm,
                              sort = FALSE)
  unit_info <- unit_guess(by)
  by_n <- unit_info[["num"]] * unit_info[["scale"]]
  by_unit <- unit_info[["unit"]]
  delay_nm <- new_var_nm(out, "delay")
  out[, (delay_nm) := time_diff(get(start_time), get(end_time),
                                    by = setnames(list(by_n), by_unit),
                                    type = "duration")]
  n_miss_delays <- sum(is.na(out[[delay_nm]]))
  if (n_miss_delays > 0){
    warning(paste(n_miss_delays, "missing observations will be
                  removed before calculation.",
                  sep = " "))
  }
  # Remove outliers
  out <- out[data.table::between(get(delay_nm), min_delay, max_delay,
                          incbounds = TRUE, NAbounds = NA), ]
  out <- out[!is.na(get(delay_nm)), ]
  quantile_prcnts <- round(probs * 100)
  quantile_nms <- paste0(rep_len("p", length(probs)), quantile_prcnts)
  quantile_summary <- out[, list("quantile_nms" = quantile_nms,
                                 "probs" = stats::quantile(get(delay_nm),
                                                           probs = probs)),
                          by = grp_nm]
  quantile_summary <- tidyr::pivot_wider(quantile_summary, names_from = .data[["quantile_nms"]],
                       values_from = .data[["probs"]])
  delay_summary <- out[, list("n" = .N,
                              "min" = min(get(delay_nm)),
                              "max" = max(get(delay_nm)),
                              "mean" = mean(get(delay_nm)),
                              "sd" = stats::sd(get(delay_nm)),
                              "iqr" = stats::quantile(get(delay_nm), 0.75) -
                                stats::quantile(get(delay_nm), 0.25),
                              "mad" = stats::mad(get(delay_nm))),
                       by = grp_nm]
  delay_summary[, ("se") := get("sd")/sqrt(get("n"))]
  delay_summary <- delay_summary %>%
    dplyr::full_join(quantile_summary, by = grp_nm, keep = FALSE) %>%
    dplyr::left_join(grp_df, by = grp_nm) %>%
    dplyr::arrange(across(all_of(grp_nm))) %>%
    dplyr::select(all_of(c(group_vars, "n", "min", "max", "mean", "sd",
                           quantile_nms, "iqr", "mad", "se")))
  # Create delay table
  min_delay <- max(min(out[[delay_nm]]),
                   min_delay)
  min_delay <- min_delay[!is.infinite(min_delay)]
  max_delay <- min(max(out[[delay_nm]]),
                   max_delay)
  max_delay <- max_delay[!is.infinite(max_delay)]
  if (length(min_delay) == 0 || length(max_delay) == 0){
    delay_tbl <- dplyr::tibble(delay = numeric(0),
                               edf = numeric(0),
                               n = integer(0),
                               cumulative = integer(0))
  } else {
    out[, ("delay_ceiling") := ceiling(get(delay_nm))]
    out[, ("edf") := edf(get("delay_ceiling"),
                         g = get(grp_nm))]
    delay_tbl <- out %>%
      fcount(across(all_of(c(grp_nm, group_vars, "delay_ceiling", "edf"))),
             name = "n")
    data.table::setDT(delay_tbl)[, ("cumulative") := collapse::fcumsum(get("n"),
                                                    g = get(grp_nm),
                                                    na.rm = TRUE)]
    set_rm_cols(delay_tbl, setdiff(names(delay_tbl),
                                   c(group_vars, "delay_ceiling",
                                     "n", "cumulative", "edf")))
    data.table::setnames(delay_tbl, old = "delay_ceiling", new = delay_nm)
  }
  set_rm_cols(out, c(grp_nm, "delay_ceiling", "edf"))
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
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(ndensity)), binwidth = 1, fill = "white", col = "black", alpha = 0.75) +
      ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(scaled)), fill = "#3F3685", alpha = 0.3,
                            bw = bw, ...) +
      ggplot2::stat_ecdf(col = "#0078D4") +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_continuous(n.breaks = 8, labels = function(x) num_fmt(x, digits = 2, big.mark = ",",
                                                                             drop_leading_zeros = FALSE,
                                                                             format = "f", flag = "",
                                                                             drop0trailing = TRUE)) +
      ggplot2::scale_y_continuous(n.breaks = 5, labels = function(x) num_fmt(x, digits = 2, big.mark = ",",
                                                                             drop_leading_zeros = TRUE,
                                                                             format = "fg", flag = "#",
                                                                             drop0trailing = TRUE)) +
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
  else {
  }
  return(delay_list)
}
