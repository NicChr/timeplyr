#' Fast time-based by-group rolling sum/mean - Currently experimental
#'
#' @description
#' `time_roll_sum` and `time_roll_mean` are efficient
#' methods for calculating a rolling sum and mean respectively given
#' many groups and with respect to a date or datetime time index. \cr
#' It is always aligned "right". \cr
#' `time_roll_window` splits `x` into windows based on the index. \cr
#' `time_roll_window_size` returns the window sizes for all indices of `x`. \cr
#' `time_roll_apply` is a generic function that applies any function
#' on a rolling basis with respect to a time index. \cr
#'
#' `time_roll_growth_rate` can efficiently calculate by-group
#' rolling growth rates with respect to a date/datetime index.
#'
#' @param x Numeric vector.
#' @param window Time window size (Default is `Inf`).
#' Must be one of the following:
#' * string, e.g `window = "day"` or `window = "2 weeks"`
#' * lubridate duration or period object, e.g. `days(1)` or `ddays(1)`.
#' * named list of length one, e.g. `list("days" = 7)`.
#' * Numeric vector, e.g. `window = 7`.
#' @param time (Optional) time index. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`,
#' `yearmon`, or `yearqtr` vector.
#' @param partial Should calculations be done using partial windows?
#' Default is \code{TRUE}.
#' @param close_left_boundary Should the left boundary be closed?
#' For example, if you specify `window = "day"` and
#' `time = c(today(), today() + 1)`, \cr
#' a value of `FALSE` would result in the window vector `c(1, 1)` whereas
#' a value of `TRUE` would result in the window vector `c(1, 2)`.
#' @param weights Importance weights. Must be the same length as x.
#' Currently, no normalisation of weights occurs.
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame.
#' @param na.rm Should missing values be removed for the calculation?
#' The default is `TRUE`.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when lubridate periods are specified or when
#' days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param fun A function.
#' @param unlist Should the output of `time_roll_apply` be unlisted with
#' `unlist`? Default is `FALSE`.
#' @param time_step An optional but \bold{important} argument
#' that follows the same input rules as `window`. \cr
#' It is currently only used only in `time_roll_growth_rate`. \cr
#' If this is supplied, the time differences across
#' gaps in time are incorporated into the growth
#' rate calculation. See \bold{details} for more info.
#' @param ... Additional arguments passed to `data.table::frollmean` and
#' `data.table::frollsum`.
#'
#' @details
#' It is much faster if your data are already sorted such that
#' `!is.unsorted(order(g, x))` is `TRUE`.
#'
#' ### Growth rates
#' For growth rates across time, one can use `time_step` to incorporate
#' gaps in time into the calculation.
#'
#' For example: \cr
#' `x <- c(10, 20)` \cr
#' `t <- c(1, 10)` \cr
#' `k <- Inf`\cr
#' `time_roll_growth_rate(x, time = t, window = k)` = `c(1, 2)`
#' whereas \cr
#' `time_roll_growth_rate(x, time = t, window = k, time_step = 1)` = `c(1, 1.08)` \cr
#' The first is a doubling from 10 to 20, whereas the second implies a growth of
#' 8% for each time step from 1 to 10. \cr
#' This allows us for example to calculate daily growth rates over the last x months,
#' even with missing days.
#'
#' @returns
#' A vector the same length as `time`.
#'
#' @examples
#' library(timeplyr)
#' library(lubridate)
#' library(dplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' time <- time_seq(today(), today() + weeks(3),
#'                  time_by = "3 days")
#' set.seed(99)
#' x <- sample.int(length(time))
#'
#' roll_mean(x, window = 7)
#' roll_sum(x, window = 7)
#'
#' time_roll_mean(x, window = ddays(7), time = time)
#' time_roll_sum(x, window = days(7), time = time)
#'
#' # Alternatively and more verbosely
#' x_chunks <- time_roll_window(x, window = 7, time = time)
#' x_chunks
#' vapply(x_chunks, mean, 0)
#'
#' # Interval (x - 3 x]
#' time_roll_sum(x, window = ddays(3), time = time)
#'
#' # An example with an irregular time series
#'
#' t <- today() + days(sort(sample(1:30, 20, TRUE)))
#' time_elapsed(t, days(1)) # See the irregular elapsed time
#' x <- rpois(length(t), 10)
#'
#' tibble(x, t) %>%
#'   mutate(sum = time_roll_sum(x, time = t, window = days(3))) %>%
#'   time_ggplot(t, sum)
#'
#' \donttest{
#' ### Rolling mean example with many time series
#'
#' # Sparse time with duplicates
#' index <- sort(sample(seq(now(), now() + dyears(3), by = "333 hours"),
#'                      250, TRUE))
#' x <- matrix(rnorm(length(index) * 10^3),
#'             ncol = 10^3, nrow = length(index),
#'             byrow = FALSE)
#'
#' zoo_ts <- zoo::zoo(x, order.by = index)
#'
#' # Normally you might attempt something like this
#' apply(x, 2,
#'       function(x){
#'         time_roll_mean(x, window = dmonths(1), time = index)
#'       }
#' )
#' # Unfortunately this is too slow and inefficient
#'
#'
#' # Instead we can pivot it longer and code each series as a separate group
#' tbl <- ts_as_tibble(zoo_ts)
#'
#' tbl %>%
#'   mutate(monthly_mean = time_roll_mean(value, window = dmonths(1),
#'                                        time = time, g = group))
#' }
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname time_roll
#' @export
time_roll_sum <- function(x, window = Inf,
                          time = seq_along(x),
                          # lag = 0L,
                          weights = NULL,
                          g = NULL,
                          partial = TRUE,
                          close_left_boundary = FALSE,
                          na.rm = TRUE,
                          time_type = getOption("timeplyr.time_type", "auto"),
                          roll_month = getOption("timeplyr.roll_month", "preday"),
                          roll_dst = getOption("timeplyr.roll_dst", "NA"),
                          ...){
  check_is_time_or_num(time)
  check_time_not_missing(time)
  # if (length(lag) != 1L){
  #   stop("lag must be of length 1")
  # }
  window <- time_by_get(time, time_by = window)
  time_num <- timespan_num(window)
  time_unit <- timespan_unit(window)
  time_subtract <- add_names(list(-time_num), time_unit)
  unit_time_by <- add_names(list(1), time_unit)
  window_size <- time_num
  if (length(window_size) != 1L){
    stop("time window size must be of length 1")
  }
  has_groups <- !is.null(g)
  g <- GRP2(g, return.groups = FALSE, return.order = TRUE)
  group_id <- fastplyr::group_id(g)
  if (has_groups){
    group_sizes <- GRP_group_sizes(g)
    n_groups <- GRP_n_groups(g)
    g2 <- GRP2(new_df(g = group_id, t = time), return.groups = FALSE)
  } else {
    g2 <- GRP2(time, return.groups = FALSE)
    group_sizes <- length(x)
    n_groups <- min(1L, length(x))
  }
  groups_are_sorted <- GRP_is_sorted(g2)
  group_id2 <- GRP_group_id(g2)
  if (!groups_are_sorted){
    group_order <- GRP_order(g2)
    sorted_df <- new_tbl(x = x, time = time,
                               group_id = group_id,
                               group_id2 = group_id2,
                               weights = weights) %>%
      df_row_slice(group_order)
    x <- .subset2(sorted_df, "x")
    time <- .subset2(sorted_df, "time")
    group_id <- .subset2(sorted_df, "group_id")
    group_id2 <- .subset2(sorted_df, "group_id2")
    weights <- fpluck(sorted_df, "weights")
  }
  sorted_g2 <- sorted_group_id_to_GRP(group_id2,
                                      n_groups = GRP_n_groups(g2),
                                      group_sizes = GRP_group_sizes(g2),
                                      group.starts = FALSE)
  if (has_groups){
    sorted_g <- sorted_group_id_to_GRP(group_id,
                                       n_groups = n_groups,
                                       group_sizes = group_sizes)
  } else {
    sorted_g <- NULL
  }
  time_start <- time_add2(time, time_by = time_subtract,
                          roll_month = roll_month,
                          roll_dst = roll_dst)
  time_window <- sequence(group_sizes)
  adj_window <- bin_grouped(time_start, breaks = time,
                                 gx = sorted_g,
                                 gbreaks = sorted_g,
                                 right = close_left_boundary,
                                 codes = TRUE)
  adj_window[which_na(adj_window)] <- 0L
  time_window <- cheapr::set_subtract(time_window, adj_window)
  # if (lag != 0){
  #   x <- flag2(x, n = lag, g = sorted_g)
  #   final_window <- flag2(final_window, n = lag, g = sorted_g)
  #   weights <- flag2(weights, n = lag, g = sorted_g)
  # }
  out <- frollsum3(x, n = time_window,
                   weights = weights,
                   adaptive = TRUE, align = "right",
                   na.rm = na.rm, ...)
  if (!partial){
    elapsed <- abs(
      time_elapsed(time, time_by = unit_time_by, g = sorted_g, rolling = FALSE)
    ) + 1
    if (close_left_boundary){
      is_partial <- cppdoubles::double_lte(elapsed, time_num)
    } else {
      is_partial <- cppdoubles::double_lt(elapsed, time_num)
    }
    out[which(is_partial)] <- NA_real_
  }
  # For duplicate times, we take the last value of each duplicate
  out <- glast(out, g = sorted_g2)
  if (!groups_are_sorted){
    out <- greorder2(out, g = g2)
  }
  out
}
#' @rdname time_roll
#' @export
time_roll_mean <- function(x, window = Inf,
                           time = seq_along(x),
                           # lag = 0L,
                           weights = NULL,
                           g = NULL,
                           partial = TRUE,
                           close_left_boundary = FALSE,
                           na.rm = TRUE,
                           time_type = getOption("timeplyr.time_type", "auto"),
                           roll_month = getOption("timeplyr.roll_month", "preday"),
                           roll_dst = getOption("timeplyr.roll_dst", "NA"),
                           ...){
  check_is_time_or_num(time)
  check_time_not_missing(time)
  window <- time_by_get(time, time_by = window)
  time_num <- timespan_num(window)
  time_unit <- timespan_unit(window)
  time_subtract <- add_names(list(-time_num), time_unit)
  unit_time_by <- add_names(list(1), time_unit)
  window_size <- time_num
  if (length(window_size) != 1L){
    stop("time window size must be of length 1")
  }
  has_groups <- !is.null(g)
  g <- GRP2(g, return.groups = FALSE, return.order = TRUE)
  group_id <- fastplyr::group_id(g)
  if (has_groups){
    group_sizes <- GRP_group_sizes(g)
    n_groups <- GRP_n_groups(g)
    g2 <- GRP2(new_df(g = group_id, t = time), return.groups = FALSE)
  } else {
    g2 <- GRP2(time, return.groups = FALSE)
    group_sizes <- length(x)
    n_groups <- min(1L, length(x))
  }
  groups_are_sorted <- GRP_is_sorted(g2)
  group_id2 <- GRP_group_id(g2)
  if (!groups_are_sorted){
    group_order <- GRP_order(g2)
    sorted_df <- new_tbl(x = x, time = time,
                               group_id = group_id,
                               group_id2 = group_id2,
                               weights = weights) %>%
      df_row_slice(group_order)
    x <- .subset2(sorted_df, "x")
    time <- .subset2(sorted_df, "time")
    group_id <- .subset2(sorted_df, "group_id")
    group_id2 <- .subset2(sorted_df, "group_id2")
    weights <- fpluck(sorted_df, "weights")
  }
  sorted_g2 <- sorted_group_id_to_GRP(group_id2,
                                      n_groups = GRP_n_groups(g2),
                                      group_sizes = GRP_group_sizes(g2),
                                      group.starts = FALSE)
  time_start <- time_add2(time, time_by = time_subtract,
                          roll_month = roll_month,
                          roll_dst = roll_dst)
  naive_window <- sequence(group_sizes)
  if (has_groups){
    sorted_g <- sorted_group_id_to_GRP(group_id,
                                       n_groups = n_groups,
                                       group_sizes = group_sizes)
  } else {
    sorted_g <- NULL
  }
  adj_window <- bin_grouped(time_start,
                                 breaks = time,
                                 gx = sorted_g,
                                 gbreaks = sorted_g,
                                 right = close_left_boundary,
                                 codes = TRUE)
  adj_window[which_na(adj_window)] <- 0L
  final_window <- naive_window - adj_window
  out <- frollmean3(x, n = final_window,
                    weights = weights,
                    adaptive = TRUE, align = "right",
                    na.rm = na.rm, ...)
  if (!partial){
    elapsed <- abs(
      time_elapsed(time, time_by = unit_time_by, g = sorted_g, rolling = FALSE)
    ) + 1
    if (close_left_boundary){
      is_partial <- cppdoubles::double_lte(elapsed, time_num)
    } else {
      is_partial <- cppdoubles::double_lt(elapsed, time_num)
    }
    out[which(is_partial)] <- NA_real_
  }
  # For duplicate times, we take the last mean value of each duplicate
  out <- glast(out, g = sorted_g2)
  if (!groups_are_sorted){
    out <- greorder2(out, g = g2)
  }
  out
}
#' @rdname time_roll
#' @export
time_roll_growth_rate <- function(x, window = Inf,
                                  time = seq_along(x),
                                  time_step = NULL,
                                  g = NULL,
                                  partial = TRUE,
                                  close_left_boundary = FALSE,
                                  na.rm = TRUE,
                                  time_type = getOption("timeplyr.time_type", "auto"),
                                  roll_month = getOption("timeplyr.roll_month", "preday"),
                                  roll_dst = getOption("timeplyr.roll_dst", "NA")){
  check_time_not_missing(time)
  check_is_time_or_num(time)
  window <- time_by_get(time, time_by = window)
  time_num <- timespan_num(window)
  time_unit <- timespan_unit(window)
  time_subtract <- add_names(list(-time_num), time_unit)
  unit_time_by <- add_names(list(1), time_unit)
  window_size <- time_num
  if (length(window_size) != 1L){
    stop("time window size must be of length 1")
  }
  has_groups <- !is.null(g)
  g <- GRP2(g, return.groups = FALSE, return.order = TRUE)
  group_id <- fastplyr::group_id(g)
  if (has_groups){
    group_sizes <- GRP_group_sizes(g)
    n_groups <- GRP_n_groups(g)
    g2 <- GRP2(new_df(g = group_id, t = time), return.groups = FALSE)
  } else {
    g2 <- GRP2(time, return.groups = FALSE)
    group_sizes <- length(x)
    n_groups <- min(1L, length(x))
  }
  groups_are_sorted <- GRP_is_sorted(g2)
  group_id2 <- GRP_group_id(g2)
  if (!groups_are_sorted){
    group_order <- GRP_order(g2)
    sorted_df <- new_tbl(x = x,
                         time = time,
                         group_id = group_id,
                         group_id2 = group_id2) %>%
      df_row_slice(group_order)
    x <- .subset2(sorted_df, "x")
    time <- .subset2(sorted_df, "time")
    group_id <- .subset2(sorted_df, "group_id")
    group_id2 <- .subset2(sorted_df, "group_id2")
  }
  sorted_g2 <- sorted_group_id_to_GRP(group_id2,
                                      n_groups = GRP_n_groups(g2),
                                      group_sizes = GRP_group_sizes(g2))
  if (has_groups){
    sorted_g <- sorted_group_id_to_GRP(group_id,
                                       n_groups = n_groups,
                                       group_sizes = group_sizes)
  } else {
    sorted_g <- NULL
  }
  time_start <- time_add2(time, time_by = time_subtract,
                          roll_month = roll_month,
                          roll_dst = roll_dst)
  naive_window <- sequence(group_sizes)

  adj_window <- bin_grouped(time_start,
                                 breaks = time,
                                 gx = sorted_g,
                                 gbreaks = sorted_g,
                                 right = close_left_boundary,
                                 codes = TRUE)
  adj_window[which_na(adj_window)] <- 0L
  final_window <- naive_window - adj_window
  if (is.null(time_step)){
    # Check first for gaps in time
    time_step <- time_granularity2(time)
    has_gaps <- time_has_gaps(time,
                              time_by = time_step,
                              g = sorted_g,
                              use.g.names = TRUE)
    if (sum(has_gaps) > 0){
      if (has_groups){
        group_ids <- which(has_gaps)
        if (is.null(names(has_gaps))){
          groups_with_gaps <- group_ids
          group_sub_msg <- "in group ID:"
        } else {
          groups_with_gaps <- names(has_gaps)[group_ids]
          group_sub_msg <- "in the group:"
        }
        rlang::warn(c("x" = paste("Time variable may have gaps", group_sub_msg),
                      "*" = groups_with_gaps[1L],
                      "Consider supplying the time_step argument",
                      "",
                      "For example:",
                      paste0("time_step = list(",
                             timespan_unit(time_step),
                             " = ",
                             timespan_num(time_step),
                             ")")),
                      use_cli_format = TRUE)
      } else {
        rlang::warn(c("x" = "Time variable may have gaps",
                      "Consider supplying the time_step argument",
                      "",
                    "For example:",
                    paste0("time_step = list(",
                           timespan_unit(time_step),
                           " = ",
                           timespan_num(time_step),
                           ")")),
                    use_cli_format = TRUE)
      }
    }
    lag_window <- final_window - 1L
    x_lagged <- roll_lag(x, lag_window)
    if (na.rm){
      lag_window <- data.table::frollsum(!is.na(x), n = lag_window,
                                           adaptive = partial,
                                           algo = "fast",
                                           align = "right")
    }
    out <- ( (x / x_lagged) ^ (1 / (lag_window)) )
    out[which(x == 0 & x_lagged == 0)] <- 1
  } else {
    time_step <- time_by_list(time_step)
    lag_window <- final_window - 1L
    x_lagged <- roll_lag(x, lag_window)
    time_lagged <- roll_lag(time, lag_window)
    time_differences <- time_diff(time_lagged, time,
                                  time_by = time_step,
                                  time_type = time_type)
    if (na.rm){
      time_differences <- time_differences -
        data.table::frollsum(is.na(x), n = final_window,
                             adaptive = TRUE,
                             algo = "fast",
                             align = "right")
    }
    out <- ( (x / x_lagged) ^ (1 / (time_differences)) )
    out[which(x == 0 & x_lagged == 0)] <- 1
  }
  if (!partial){
    elapsed <- time_elapsed(time, time_by = unit_time_by,
                            g = sorted_g, rolling = FALSE)
    if (close_left_boundary){
      is_partial <- cppdoubles::double_lte(abs(elapsed) + 1, time_num)
    } else {
      is_partial <- cppdoubles::double_lt(abs(elapsed) + 1, time_num)
    }
    out[which(is_partial)] <- NA_real_
  }
  out <- glast(out, g = sorted_g2)
  if (!groups_are_sorted){
    out <- collapse::greorder(out, g = g2)
  }
  out
}

#' @rdname time_roll
#' @export
time_roll_window_size <- function(time, window = Inf,
                                  g = NULL,
                                  partial = TRUE,
                                  close_left_boundary = FALSE,
                                  time_type = getOption("timeplyr.time_type", "auto"),
                                  roll_month = getOption("timeplyr.roll_month", "preday"),
                                  roll_dst = getOption("timeplyr.roll_dst", "NA")){
  check_is_time_or_num(time)
  check_time_not_missing(time)
  window <- time_by_list(window)
  check_time_by_length_is_one(window)
  time_num <- timespan_num(window)
  time_unit <- timespan_unit(window)
  time_subtract <- add_names(list(-time_num), time_unit)
  unit_time_by <- add_names(list(1), time_unit)
  g <- GRP2(g, return.groups = FALSE)
  if (!gis_sorted(time, g = g)){
    stop("time variable must be sorted and if g is supplied,
         it must be sorted also by g first")
  }
  if (is.null(g)){
    group_sizes <- length(time)
    n_groups <- min(1L, length(time))
    group_ends <- length(time)
  } else {
    group_sizes <- GRP_group_sizes(g)
    n_groups <- GRP_n_groups(g)
    group_ends <- GRP_ends(g)
  }
  start <- time_add2(time, time_by = time_subtract,
                     time_type = time_type,
                     roll_month = roll_month,
                     roll_dst = roll_dst)
  time <- time_cast(time, start)
  out <- sequence(group_sizes)
  if (time_num < 0){
    # prepend <- -Inf
    # append <- NULL
    close_left_boundary <- !close_left_boundary
  }
  if (isTRUE(time_num == 0)){
    if (close_left_boundary){
      out <- fastplyr::row_id(new_df(group_id = fastplyr::group_id(g), time = time))
    } else {
      out <- integer(length(time))
    }
  } else {
    adj_window <- bin_grouped(start,
                              breaks = time,
                              gx = g,
                              gbreaks = g,
                              right = close_left_boundary,
                              codes = TRUE)
    which_na <- which_na(adj_window)
    adj_window[which_na] <- 0L
    out <- cheapr::set_subtract(out, adj_window)
  }
  if (time_num < 0){
    rev_window <- -sequence(group_sizes, from = group_sizes, to = min(1L, length(time)), by = -1L)
    out[which_na] <- rev_window[which_na]
    which_na <- cheapr::set_change_sign(which_na)
    out[which_na] <- out[which_na] - 1L
  }
  if (!partial){
    elapsed <- time_elapsed(time, time_by = unit_time_by,
                            g = g, rolling = FALSE)
    if (close_left_boundary){
      is_partial <- cppdoubles::double_lte(abs(elapsed) + 1, time_num)
    } else {
      is_partial <- cppdoubles::double_lt(abs(elapsed) + 1, time_num)
    }
    out[which(is_partial)] <- NA_integer_
  }
  out
}
#' @rdname time_roll
#' @export
time_roll_window <- function(x, window = Inf, time = seq_along(x),
                             g = NULL,
                             partial = TRUE,
                             close_left_boundary = FALSE,
                             time_type = getOption("timeplyr.time_type", "auto"),
                             roll_month = getOption("timeplyr.roll_month", "preday"),
                             roll_dst = getOption("timeplyr.roll_dst", "NA")){
  window_widths <- time_roll_window_size(time, window = window,
                                         g = g,
                                         partial = partial,
                                         close_left_boundary = close_left_boundary,
                                         time_type = time_type,
                                         roll_month = roll_month,
                                         roll_dst = roll_dst)
  window_widths[which_na(window_widths)] <- 0L
  out <- roll_chop(x, sizes = window_widths)
  vctrs::new_list_of(out, ptype = x[0L])
}
#' @rdname time_roll
#' @export
time_roll_apply <- function(x, window = Inf, fun,
                            time = seq_along(x),
                            g = NULL,
                            partial = TRUE,
                            unlist = FALSE,
                            close_left_boundary = FALSE,
                            time_type = getOption("timeplyr.time_type", "auto"),
                            roll_month = getOption("timeplyr.roll_month", "preday"),
                            roll_dst = getOption("timeplyr.roll_dst", "NA")){
  stopifnot(is.function(fun))
  sizes <- time_roll_window_size(time,
                                 window = window,
                                 g = g,
                                 partial = partial,
                                 close_left_boundary = close_left_boundary,
                                 time_type = time_type,
                                 roll_month = roll_month,
                                 roll_dst = roll_dst)
  sizes[which_na(sizes)] <- 0L
  x_size <- length(x)
  out <- vector("list", x_size)
  for (i in seq_len(x_size)){
    out[[i]] <- fun(x[seq_len(.subset(sizes, i)) + (i - .subset(sizes, i))])
  }
  if (!partial){
    which_zero <- cheapr::val_find(sizes, 0L)
    if (length(which_zero) > 0L){
      ptype <- out[[1L]][0L]
    }
    out[which_zero] <- list(ptype)
  }
  if (is.null(g)){
    group_id <- fastplyr::group_id(time, as_qg = TRUE)
  } else {
    group_id <- fastplyr::group_id(new_df(g = group_id(g), t = time), as_qg = TRUE)
  }
  # This only works because group_id should always be sorted here
  # Which is already checked in time_roll_window_size
  out <- out[cumsum(attr(group_id, "group.sizes"))[group_id]]
  if (unlist){
    out <- unlist(out, use.names = FALSE, recursive = FALSE)
  }
  out
}

# Rolling join 2nd version (latest working version)
# time_roll_sum3 <- function(x, window = Inf,
#                           time = seq_along(x),
#                           weights = NULL,
#                           g = NULL,
#                           partial = TRUE,
#                           close_left_boundary = FALSE,
#                           na.rm = TRUE,
#                           time_type = getOption("timeplyr.time_type", "auto"),
#                           roll_month = getOption("timeplyr.roll_month", "preday"), roll_dst = getOption("timeplyr.roll_dst", "NA"),
#                           ...){
#   check_is_time_or_num(time)
#   check_time_not_missing(time)
#   window <- time_by_get(time, time_by = window)
#   time_num <- timespan_num(window)
#   time_unit <- timespan_unit(window)
#   time_subtract <- add_names(list(-time_num), time_unit)
#   unit_time_by <- add_names(list(1), time_unit)
#   window_size <- time_num
#   if (length(window_size) != 1L){
#     stop("time window size must be of length 1")
#   }
#   has_groups <- !is.null(g)
#   g <- GRP2(g, return.groups = FALSE, return.order = TRUE)
#   group_id <- group_id(g)
#   if (has_groups){
#     group_sizes <- GRP_group_sizes(g)
#     n_groups <- GRP_n_groups(g)
#     g2 <- GRP2(list(group_id, time), return.groups = FALSE)
#   } else {
#     g2 <- GRP2(time, return.groups = FALSE)
#     group_sizes <- length(x)
#     n_groups <- min(1L, length(x))
#   }
#   groups_are_sorted <- GRP_is_sorted(g2)
#   group_id2 <- GRP_group_id(g2)
#   if (!groups_are_sorted){
#     group_order <- GRP_order(g2)
#     sorted_df <- new_tbl(x = x, time = time,
#                          group_id = group_id,
#                          group_id2 = group_id2,
#                          weights = weights) %>%
#       df_row_slice(group_order)
#     x <- .subset2(sorted_df, "x")
#     time <- .subset2(sorted_df, "time")
#     group_id <- .subset2(sorted_df, "group_id")
#     group_id2 <- .subset2(sorted_df, "group_id2")
#     weights <- fpluck(sorted_df, "weights")
#   }
#   sorted_g2 <- sorted_group_id_to_GRP(group_id2,
#                                       n_groups = GRP_n_groups(g2),
#                                       group_sizes = GRP_group_sizes(g2),
#                                       group.starts = FALSE)
#   if (has_groups){
#     sorted_g <- sorted_group_id_to_GRP(group_id,
#                                        n_groups = n_groups,
#                                        group_sizes = group_sizes)
#   } else {
#     sorted_g <- NULL
#   }
#   time_start <- time_add2(time, time_by = time_subtract,
#                           roll_month = roll_month,
#                           roll_dst = roll_dst)
#   naive_window <- sequence(group_sizes)
#   dt1 <- collapse::qDT(list3(group_id = group_id,
#                              time = time))
#   dt2 <- collapse::qDT(list3(group_id = group_id,
#                              time_start = time_start))
#   data.table::setattr(dt1, "sorted", names(dt1))
#   data.table::setattr(dt2, "sorted", names(dt2))
#   if (close_left_boundary){
#     if (has_groups){
#       naive_window2 <- dt1[dt2, on = list(group_id, time < time_start),
#                            which = TRUE, mult = "last"]
#     } else {
#       naive_window2 <- dt1[dt2, on = list(time < time_start),
#                            which = TRUE, mult = "last"]
#     }
#   } else {
#     if (has_groups){
#       naive_window2 <- dt1[dt2, on = list(group_id, time <= time_start),
#                            which = TRUE, mult = "last"]
#     } else {
#       naive_window2 <- dt1[dt2, on = list(time <= time_start),
#                            which = TRUE, mult = "last"]
#     }
#   }
#   naive_window2 <- seq_along(x) - naive_window2
#   final_window <- data.table::fcoalesce(naive_window2, naive_window)
#   out <- frollsum3(x, n = final_window,
#                    weights = weights,
#                    adaptive = TRUE, align = "right",
#                    na.rm = na.rm, ...)
#   if (!partial){
#     elapsed <- time_elapsed(time, time_by = unit_time_by,
#                             g = sorted_g, rolling = FALSE)
#     if (close_left_boundary){
#       is_partial <- cppdoubles::double_lte(abs(elapsed) + 1, time_num)
#     } else {
#       is_partial <- cppdoubles::double_lt(abs(elapsed) + 1, time_num)
#     }
#     out[which(is_partial)] <- NA_real_
#   }
#   # For duplicate times, we take the last value of each duplicate
#   out <- glast(out, g = sorted_g2)
#   if (!groups_are_sorted){
#     out <- collapse::greorder(out, g = g2)
#   }
#   out
# }
