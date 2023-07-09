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
#' @param ... Further arguments passed to `geom_line()`.
#' @seealso \link[timeplyr]{ts_as_tibble}
#' @examples
#' library(dplyr)
#' library(timeplyr)
#'
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
#' \dontrun{
#' # xts example
#' data(sample_matrix, package = "xts")
#' sample.xts <- xts::as.xts(sample_matrix)
#' sample.xts %>%
#'   ts_as_tibble() %>%
#'   time_ggplot(time, value, group)
#'
#' # timeSeries example
#' timeSeries::MSFT %>%
#'   ts_as_tibble() %>%
#'   time_ggplot(time, value, group, facet = TRUE)
#' }
#' #'
#' # An example using raw data
#'
#' ebola <- outbreaks::ebola_sim$linelist
#' ebola %>%
#'   time_count(date_of_infection) %>%
#'   time_ggplot(date_of_infection, n)
#' @export
time_ggplot <- function(data, time, value, group = NULL,
                        facet = FALSE,
                        ...){
  # Tidyselect variables
  time <- tidy_select_pos(data, !!enquo(time))
  value <- tidy_select_pos(data, !!enquo(value))
  group <- tidy_select_pos(data, !!enquo(group))
  time <- names(time)
  value <- names(value)
  group <- names(group)
  # Must be date, datetime or number
  if (!is_time_or_num(fpluck(data, time))){
    stop("time must be a date, datetime or numeric variable")
  }
  # Pretty x-axis breaks
  time_breaks <- time_breaks(fpluck(data, time),
                             n = 7, time_floor = TRUE)
  if (is_datetime(fpluck(data, time))){
    x_scale <- ggplot2::scale_x_datetime(breaks = time_breaks,
                                         labels = label_date_short())
  } else if (is_date(fpluck(data, time))){
    x_scale <- ggplot2::scale_x_date(breaks = time_breaks,
                                     labels = label_date_short())
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
    data <- dplyr::dplyr_col_modify(data, cols = group_col)
  } else {
    group_nm <- group
  }
  # Time-series plot
  out <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[time]],
                                 y = .data[[value]])) +
    ggplot2::theme_minimal() +
    x_scale
  if (length(group) > 0L){
    if (facet){
      # Add a new col every 6 rows
      facet_ncol <- (n_unique(fpluck(data, group_nm)) %/% 6) + 1
      out <- out +
        ggplot2::geom_line(...) +
        ggplot2::facet_wrap(group, ncol = facet_ncol,
                            scales = "free_y")
    } else {
      out <- out +
        ggplot2::geom_line(ggplot2::aes(col = .data[[group_nm]]), ...)
    }
  } else {
    out <- out +
      ggplot2::geom_line(...)
  }
  out
}
