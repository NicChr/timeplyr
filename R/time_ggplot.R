#' Quick time-series ggplot
#'
#' @description `time_ggplot()` is a neat way to quickly
#' plot aggregate time-series data.
#'
#' @param data A data frame
#' @param time Time variable using `tidyselect`.
#' @param value Value variable using `tidyselect`.
#' @param group (Optional) Group variable using `tidyselect`.
#' @param facet When groups are supplied, should multi-series be
#' plotted separately or on the same plot?
#' Default is `FALSE`, or together.
#' @param geom `ggplot2` 'geom' type. Default is `geom_line()`.
#' @param ... Further arguments passed to the chosen 'geom'.
#'
#' @returns
#' A `ggplot`.
#'
#' @seealso [ts_as_tibble]
#'
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(ggplot2)
#' library(lubridate)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # It's as easy as this
#' AirPassengers %>%
#'   ts_as_tibble() %>%
#'   time_ggplot(time, value)
#'
#' # And this
#' EuStockMarkets %>%
#'   ts_as_tibble() %>%
#'   time_ggplot(time, value, group)
#'
#' # zoo example
#' x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14) - 1
#' x <- zoo::zoo(rnorm(5), x.Date)
#' x %>%
#'   ts_as_tibble() %>%
#'   time_ggplot(time, value)
#'
#' # An example using raw data
#'
#' ebola <- outbreaks::ebola_sim$linelist
#'
#' # We can build a helper to count and complete
#' # Using the same time grid
#'
#' count_and_complete <- function(.data, time, .name,
#'                                from = NULL, ...,
#'                                time_by = NULL){
#'   .data %>%
#'     time_by(!!dplyr::enquo(time), time_by = time_by,
#'                .name = .name, from = !!dplyr::enquo(from),
#'             as_interval = FALSE) %>%
#'     dplyr::count(...) %>%
#'     dplyr::ungroup() %>%
#'     time_complete(.data[[.name]], ..., time_by = time_by,
#'                   fill = list(n = 0))
#' }
#' ebola %>%
#'   count_and_complete(date_of_onset, outcome, time_by = "week", .name = "date_of_onset",
#'                      from = floor_date(min(date_of_onset), "week")) %>%
#'   time_ggplot(date_of_onset, n, geom = geom_blank) +
#'   geom_col(aes(fill = outcome))
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
time_ggplot <- function(data, time, value, group = NULL,
                        facet = FALSE, geom = ggplot2::geom_line, ...){
  # Tidyselect variables
  time <- tidy_select_pos(data, !!enquo(time))
  value <- tidy_select_pos(data, !!enquo(value))
  group <- tidy_select_pos(data, !!enquo(group))
  time <- names(time)
  value <- names(value)
  group <- names(group)
  time_var <- data[[time]]
  # Must be date, datetime or number
  if (!is_time_or_num(time_var)){
    stop("time must be a date, datetime or numeric variable")
  }
  # Pretty x-axis breaks
  time_breaks <- time_breaks(time_var,
                             n = 7, time_floor = TRUE)
  if (is_datetime(time_var)){
    x_scale <- ggplot2::scale_x_datetime(breaks = time_breaks,
                                         labels = scales::label_date_short())
  } else if (is_date(time_var)){
    x_scale <- ggplot2::scale_x_date(breaks = time_breaks,
                                     labels = scales::label_date_short())
  } else if (is_year_month(time_var)){
    x_scale <- scale_x_year_month(breaks = time_breaks)
  } else if (is_year_quarter(time_var)){
    x_scale <- scale_x_year_quarter(breaks = time_breaks)
  } else {
    x_scale <- ggplot2::scale_x_continuous(breaks = time_breaks)
  }
  # Concatenate group names together
  if (length(group) > 1L){
    group_nm <- new_var_nm(data, "group")
    group_col <- list(
      df_paste_names(
        fselect(safe_ungroup(data), .cols = group)
      )
    )
    names(group_col) <- group_nm
    data <- df_add_cols(data, cols = group_col)
  } else {
    group_nm <- group
  }
  # Time-series plot
  out <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[time]],
                                 y = .data[[value]])) +
    ggplot2::theme_minimal() +
    x_scale
  extra_gg_args <- list(...)
  if (length(group) > 0L){
    if (facet){
      # Add a new col every 6 rows
      facet_ncol <- (n_unique(fpluck(data, group_nm)) %/% 6) + 1
      out <- out +
        do.call(geom, extra_gg_args) +
        ggplot2::facet_wrap(group, ncol = facet_ncol,
                            scales = "free_y")
    } else {
      out <- out +
        do.call(geom, c(list(ggplot2::aes(col = .data[[group_nm]])), extra_gg_args))
    }
  } else {
    out <- out + do.call(geom, extra_gg_args)
  }
  out
}
