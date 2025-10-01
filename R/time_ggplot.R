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
#' @seealso [ts_as_tbl]
#'
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(ggplot2)
#' library(lubridate)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # It's as easy as this
#' AirPassengers |>
#'   ts_as_tbl() |>
#'   time_ggplot(time, value)
#'
#' # And this
#' EuStockMarkets |>
#'   ts_as_tbl() |>
#'   time_ggplot(time, value, group)
#'
#' # Converting this to monthly averages
#'
#' EuStockMarkets |>
#'   ts_as_tbl() |>
#'   mutate(month = year_month_decimal(time)) |>
#'   summarise(avg = mean(value),
#'             .by = c(group, month)) |>
#'   time_ggplot(month, avg, group)
#'
#' # zoo example
#' x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14) - 1
#' x <- zoo::zoo(rnorm(5), x.Date)
#' x |>
#'   ts_as_tbl() |>
#'   time_ggplot(time, value)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
time_ggplot <- function(data, time, value, group = NULL,
                        facet = FALSE, geom = ggplot2::geom_line, ...){

  if (!is.logical(facet)){
   cli::cli_abort("{.arg facet} must be a logical scalar")
  }

  # Tidyselect variables
  time <- tidy_select_names(data, !!enquo(time))
  value <- tidy_select_names(data, !!enquo(value))
  group <- tidy_select_names(data, !!enquo(group))
  time <- names(time)
  value <- names(value)
  group <- names(group)
  time_var <- data[[time]]
  # Pretty x-axis breaks
  breakpoints <- time_breakpoints(time_var, n = 10)
  if (is_datetime(time_var)){
    x_scale <- ggplot2::scale_x_datetime(breaks = breakpoints,
                                         labels = scales::label_date_short())
  } else if (is_date(time_var)){
    x_scale <- ggplot2::scale_x_date(breaks = breakpoints,
                                     labels = scales::label_date_short())
  } else if (is_year_month(time_var)){
    x_scale <- scale_x_year_month(breaks = breakpoints)
  } else if (is_year_quarter(time_var)){
    x_scale <- scale_x_year_quarter(breaks = breakpoints)
  } else {
    x_scale <- ggplot2::scale_x_continuous(breaks = breakpoints)
  }
  # Concatenate group names together
  if (length(group) > 1L){
    group_nm <- unique_col_name(data, "group")
    group_col <- list(
      df_paste_names(
        fastplyr::f_select(fastplyr::f_ungroup(data), .cols = group)
      )
    )
    names(group_col) <- group_nm
    data <- cheapr::df_modify(data, cols = group_col)
  } else {
    group_nm <- group
  }
  # Time-series plot
  out <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[time]],
                                 y = .data[[value]])) +
    ggplot2::theme_minimal() +
    x_scale
  extra_gg_args <- list(...)
  if (length(group) > 0L){
    if (facet){
      # Add a new col every 6 rows
      facet_ncol <- (collapse::fnunique(data[[group_nm]]) %/% 6) + 1
      out <- out +
        do.call(geom, extra_gg_args) +
        ggplot2::facet_wrap(group, ncol = facet_ncol, scales = "free_y")
    } else {
      out <- out +
        do.call(geom, c(list(ggplot2::aes(col = .data[[group_nm]])), extra_gg_args))
    }
  } else {
    out <- out + do.call(geom, extra_gg_args)
  }
  out
}
