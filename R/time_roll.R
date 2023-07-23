#' Fast time-based by-group rolling sum/mean - Currently experimental
#'
#' @description An efficient method for a rolling sum/mean for many groups with
#' respect to a date or datetime time index. \cr
#' It is always aligned "right".
#'
#' @param x Numeric vector.
#' @param window Window size, default is `length(x)`.
#' Must be one of the following:
#' * string, e.g `window = "day"` or `window = "2 weeks"`
#' * lubridate duration or period object, e.g. `days(1)` or `ddays(1)`.
#' * named list of length one, e.g. `list("days" = 7)`.
#' * Numeric vector, e.g. `window = 7`.
#' @param time (Optional) time index. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`,
#' `yearmon`, or `yearqtr` vector.
#' If this is left `NULL`, a regular rolling sum/mean will be calculated.
#' @param partial Should calculations be done using partial windows?
#' Default is \code{TRUE}.
#' @param weights Importance weights. Must be the same length as x.
#' Currently, no normalisation of weights occurs.
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame.
#' @param close_left_boundary Should the left boundary be closed?
#' For example, if you specify `window = "day"` and
#' `time = c(today(), today() + 1)`, \cr
#' a value of `FALSE` would result in the window vector `c(1, 1)` whereas
#' a value of `TRUE` would result in the window vector `c(1, 2)`.
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
#' @param ... Additional arguments passed to `data.table::frollmean` and
#' `data.table::frollsum`.
#' @details
#' It is much faster if your data are already sorted such that
#' `!is.unsorted(order(g, x))` is `TRUE`.
#' @examples
#' library(timeplyr)
#' library(lubridate)
#' library(dplyr)
#'
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
#' purrr::map_dbl(x_chunks, mean)
#'
#' # Interval [x - 3 x]
#' time_roll_sum(x, window = ddays(3), time = time,
#'               close_left_boundary = TRUE)
#' # Interval (x - 3 x]
#' time_roll_sum(x, window = ddays(3), time = time,
#'               close_left_boundary = FALSE)
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
#' \dontrun{
#' ### Rolling mean example with many time series
#'
#' # Sparse time with duplicates
#' index <- sort(sample(seq(now(), now() + dyears(3), by = "333 hours"),
#'                      250, TRUE))
#' x <- matrix(rnorm(length(index) * 10^5),
#'             ncol = 10^5, nrow = length(index),
#'             byrow = FALSE)
#'
#' zoo_ts <- zoo::zoo(x, order.by = index)
#'
#' # Say you started with data like zoo_ts
#' # Normally you might attempt something like this
#' # apply(zoo_ts, 2,
#' #       function(x){
#' #         slider::slide_index_mean(x, i = index, before = dmonths(1))
#' #       }
#' # )
#' # Unfortunately this is too slow and inefficient
#'
#'
#' # Instead we can pivot it longer and code each series as a separate group
#' tbl <- ts_as_tibble(zoo_ts)
#'
#' tbl %>%
#'   mutate(monthly_mean = time_roll_mean(value, window = dmonths(1),
#'                                 time = time, g = group))
#' }
#' @rdname time_roll
#' @export
time_roll_sum <- function(x, window,
                          time = NULL,
                          weights = NULL,
                          g = NULL,
                          partial = TRUE,
                          close_left_boundary = FALSE,
                          na.rm = TRUE,
                          time_type = c("auto", "duration", "period"),
                          roll_month = "preday", roll_dst = "pre",
                          ...){
  if (is.null(time)){
    return(roll_sum(x, window = window,
                    g = g, weights = weights,
                    partial = partial,
                    na.rm = na.rm, ...))
  }
  check_index_not_missing(time)
  window <- time_by_get(time, time_by = window)
  time_num <- time_by_num(window)
  time_unit <- time_by_unit(window)
  time_subtract <- setnames(list(-time_num), time_unit)
  window_size <- time_num
  if (length(window_size) != 1L){
    stop("time window size must be of length 1")
  }
  has_groups <- !is.null(g)
  check_is_time_or_num(time)
  g <- GRP2(g, return.groups = FALSE, return.order = FALSE)
  group_id <- group_id(g)
  if (has_groups){
    group_sizes <- GRP_group_sizes(g)
    n_groups <- GRP_n_groups(g)
    g2 <- GRP2(list(group_id, time), return.groups = FALSE)
  } else {
    g2 <- GRP2(time, return.groups = FALSE)
    group_sizes <- length(x)
    group_id <- collapse::alloc(0L, length(x))
    n_groups <- min(1L, length(x))
  }
  groups_are_sorted <- GRP_is_sorted(g2)
  group_id2 <- GRP_group_id(g2)
  if (!groups_are_sorted){
    group_order <- GRP_order(g2)
    x <- x[group_order]
    time <- time[group_order]
    group_id <- group_id[group_order]
    group_id2 <- group_id2[group_order]
  }
  time_start <- time_add2(time, time_by = time_subtract,
                          roll_month = roll_month,
                          roll_dst = roll_dst)
  naive_window <- sequence(group_sizes)
  dt1 <- collapse::qDT(list(group_id = group_id,
                            time = time))
  dt2 <- collapse::qDT(list(group_id = group_id,
                            time_start = time_start))
  data.table::setattr(dt1, "sorted", c("group_id", "time"))
  data.table::setattr(dt2, "sorted", c("group_id", "time_start"))
  if (close_left_boundary){
    naive_window2 <- dt1[dt2, on = list(group_id, time < time_start),
                         which = TRUE, mult = "last"]
  } else {
    naive_window2 <- dt1[dt2, on = list(group_id, time <= time_start),
                         which = TRUE, mult = "last"]
  }
  naive_window2 <- seq_along(group_id) - naive_window2
  final_window <- data.table::fcoalesce(naive_window2, naive_window)
  out <- frollsum3(x, n = final_window,
                   weights = weights,
                   adaptive = TRUE, align = "right",
                   na.rm = na.rm, ...)
  if (!partial){
    elapsed <- time_elapsed(time, time_by = window, g = group_id,
                            rolling = FALSE)
    out[double_lt(elapsed, 1)] <- NA_real_

  }
  # For duplicate times, we take the last mean value of each duplicate
  out <- glast(out, g = group_id2)
  if (!groups_are_sorted){
    out <- collapse::greorder(out, g = g2)
  }
  out
}
#' @rdname time_roll
#' @export
time_roll_mean <- function(x, window,
                           time = NULL,
                           weights = NULL,
                           g = NULL,
                           partial = TRUE,
                           close_left_boundary = FALSE,
                           na.rm = TRUE,
                           time_type = c("auto", "duration", "period"),
                           roll_month = "preday", roll_dst = "pre",
                           ...){
  if (is.null(time)){
    return(roll_sum(x, window = window,
                    g = g, weights = weights,
                    partial = partial,
                    na.rm = na.rm, ...))
  }
  check_index_not_missing(time)
  window <- time_by_get(time, time_by = window)
  time_num <- time_by_num(window)
  time_unit <- time_by_unit(window)
  time_subtract <- setnames(list(-time_num), time_unit)
  window_size <- time_num
  if (length(window_size) != 1L){
    stop("time window size must be of length 1")
  }
  has_groups <- !is.null(g)
  check_is_time_or_num(time)
  g <- GRP2(g, return.groups = FALSE, return.order = FALSE)
  group_id <- group_id(g)
  if (has_groups){
    group_sizes <- GRP_group_sizes(g)
    n_groups <- GRP_n_groups(g)
    g2 <- GRP2(list(group_id, time), return.groups = FALSE)
  } else {
    g2 <- GRP2(time, return.groups = FALSE)
    group_sizes <- length(x)
    group_id <- collapse::alloc(0L, length(x))
    n_groups <- min(1L, length(x))
  }
  groups_are_sorted <- GRP_is_sorted(g2)
  group_id2 <- GRP_group_id(g2)
  if (!groups_are_sorted){
    group_order <- GRP_order(g2)
    x <- x[group_order]
    time <- time[group_order]
    group_id <- group_id[group_order]
    group_id2 <- group_id2[group_order]
  }
  time_start <- time_add2(time, time_by = time_subtract,
                          roll_month = roll_month,
                          roll_dst = roll_dst)
  naive_window <- sequence(group_sizes)
  dt1 <- collapse::qDT(list(group_id = group_id,
                            time = time))
  dt2 <- collapse::qDT(list(group_id = group_id,
                            time_start = time_start))
  data.table::setattr(dt1, "sorted", c("group_id", "time"))
  data.table::setattr(dt2, "sorted", c("group_id", "time_start"))
  if (close_left_boundary){
    naive_window2 <- dt1[dt2, on = list(group_id, time < time_start),
                         which = TRUE, mult = "last"]
  } else {
    naive_window2 <- dt1[dt2, on = list(group_id, time <= time_start),
                         which = TRUE, mult = "last"]
  }
  naive_window2 <- seq_along(group_id) - naive_window2
  final_window <- data.table::fcoalesce(naive_window2, naive_window)
  out <- frollmean3(x, n = final_window,
                    weights = weights,
                    adaptive = TRUE, align = "right",
                    na.rm = na.rm, ...)
  if (!partial){
    elapsed <- time_elapsed(time, time_by = window, g = group_id,
                            rolling = FALSE)
    out[double_lt(elapsed, 1)] <- NA_real_

  }
  # For duplicate times, we take the last mean value of each duplicate
  out <- glast(out, g = group_id2)
  if (!groups_are_sorted){
    out <- collapse::greorder(out, g = g2)
  }
  out
}
# time_roll_window <- function(x, window, time,
#                              partial = TRUE,
#                              close_left_boundary = FALSE,
#                              time_type = c("auto", "duration", "period"),
#                              roll_month = "preday", roll_dst = "pre"){
#   window <- time_by_list(window)
#   time_num <- time_by_num(window)
#   time_unit <- time_by_unit(window)
#   time_subtract <- setnames(list(-time_num), time_unit)
#   start <- time_add2(time, time_by = time_subtract,
#                      time_type = time_type,
#                      roll_month = roll_month,
#                      roll_dst = roll_dst)
#   time <- time_cast(time, start)
#   window_width <- seq_along(x) -
#     findInterval(start, time, left.open = close_left_boundary)
#   if (!partial){
#     elapsed <- time_elapsed(time, time_by = window, rolling = FALSE)
#     window_width[double_lt(elapsed, 1)] <- 0L
#   }
#   out <- roll_chop(x, sizes = window_width)
#   vctrs::new_list_of(out, ptype = x[0L])
# }
# time_roll_window_size <- function(x, window,
#                                   g = NULL,
#                                   partial = TRUE,
#                                   close_left_boundary = FALSE,
#                                   time_type = c("auto", "duration", "period"),
#                                   roll_month = "preday", roll_dst = "pre"){
#   window <- time_by_list(window)
#   time_num <- time_by_num(window)
#   time_unit <- time_by_unit(window)
#   time_subtract <- setnames(list(-time_num), time_unit)
#   g <- GRP2(g, return.groups = FALSE)
#   if (!gis_sorted(x, g = g)){
#     stop("x must be sorted and if g is supplied,
#          it must be sorted also by g first")
#   }
#   if (is.null(g)){
#     group_sizes <- length(x)
#     group_id <- collapse::alloc(1L, length(x))
#   } else {
#     group_sizes <- GRP_group_sizes(g)
#     group_id <- GRP_group_id(g)
#   }
#   start <- time_add2(x, time_by = time_subtract,
#                      time_type = time_type,
#                      roll_month = roll_month,
#                      roll_dst = roll_dst)
#   x <- time_cast(x, start)
#   naive_window <- sequence(group_sizes)
#   dt1 <- collapse::qDT(list(group_id = group_id,
#                             time = x))
#   dt2 <- collapse::qDT(list(group_id = group_id,
#                             start = start))
#   data.table::setattr(dt1, "sorted", c("group_id", "time"))
#   data.table::setattr(dt2, "sorted", c("group_id", "start"))
#   if (close_left_boundary){
#     naive_window2 <- dt1[dt2, on = .(group_id, time < start),
#                          which = TRUE, mult = "last"]
#   } else {
#     naive_window2 <- dt1[dt2, on = .(group_id, time <= start),
#                          which = TRUE, mult = "last"]
#   }
#   naive_window2 <- seq_along(group_id) - naive_window2
#   out <- data.table::fcoalesce(naive_window2, naive_window)
#   if (!partial){
#     elapsed <- time_elapsed(x, time_by = window, rolling = FALSE)
#     out[double_lt(elapsed, 1)] <- NA_integer_
#   }
#   out
# }
time_roll_sum3 <- function(x, window,
                          time = NULL,
                          weights = NULL,
                          g = NULL,
                          partial = TRUE,
                          close_left_boundary = FALSE,
                          na.rm = TRUE,
                          time_type = c("auto", "duration", "period"),
                          roll_month = "preday", roll_dst = "pre",
                          ...){
  if (is.null(time)){
    return(roll_sum(x, window = window,
                    g = g, weights = weights,
                    partial = partial,
                    na.rm = na.rm, ...))
  }
  if (anyNA(time)){
    stop("time index must not contain NA values")
  }
  window <- time_by_get(time, time_by = window)
  time_num <- time_by_num(window)
  time_unit <- time_by_unit(window)
  time_subtract <- setnames(list(-time_num), time_unit)
  window_size <- time_num
  if (length(window_size) != 1L){
    stop("time window size must be of length 1")
  }
  has_groups <- !is.null(g)
  check_is_time_or_num(time)
  g <- GRP2(g, return.groups = FALSE, return.order = FALSE)
  group_id <- group_id(g)
  if (has_groups){
    group_sizes <- GRP_group_sizes(g)
    n_groups <- GRP_n_groups(g)
    g2 <- GRP2(list(group_id, time), return.groups = FALSE)
  } else {
    g2 <- GRP2(time, return.groups = FALSE)
    group_sizes <- length(x)
    n_groups <- min(1L, length(x))
  }
  groups_are_sorted <- GRP_is_sorted(g2)
  group_id2 <- GRP_group_id(g2)
  if (!groups_are_sorted){
    group_order <- GRP_order(g2)
    x <- x[group_order]
    time <- time[group_order]
    group_id <- group_id[group_order]
    group_id2 <- group_id2[group_order]
  }
  time_start <- time_add2(time, time_by = time_subtract,
                          roll_month = roll_month,
                          roll_dst = roll_dst)
  naive_window <- sequence(group_sizes)
  if (has_groups){
    time_start_list <- collapse::gsplit(time_as_number(time_start), g = g)
    time_list <- collapse::gsplit(time_as_number(time), g = g)
    adj_window <- vector("list", length(time_list))
    for (i in seq_along(adj_window)){
      adj_window[[i]] <- .bincode(.subset2(time_start_list, i),
                                  .subset2(time_list, i),
                                  right = close_left_boundary,
                                  include.lowest = FALSE)
    }
    adj_window <- unlist(adj_window, recursive = FALSE, use.names = FALSE)
    adj_window[is.na(adj_window)] <- 0L
  } else {
    adj_window <- findInterval(time_start, time, left.open = close_left_boundary)
  }
  final_window <- naive_window - adj_window
  out <- frollsum3(x, n = final_window,
                    weights = weights,
                    adaptive = TRUE, align = "right",
                    na.rm = na.rm, ...)
  if (!partial){
    elapsed <- time_elapsed(time, time_by = window, g = group_id,
                            rolling = FALSE)
    out[double_lt(elapsed, 1)] <- NA_real_

  }
  # For duplicate times, we take the last mean value of each duplicate
  out <- glast(out, g = group_id2)
  if (!groups_are_sorted){
    out <- collapse::greorder(out, g = g2)
  }
  out
}
# Working alternative
# time_roll_sum2 <- function(x, window,
#                           time = NULL,
#                           weights = NULL,
#                           g = NULL,
#                           partial = TRUE,
#                           close_left_boundary = FALSE,
#                           na.rm = TRUE,
#                           time_type = c("auto", "duration", "period"),
#                           roll_month = "preday", roll_dst = "pre",
#                           ...){
#   if (is.null(time)){
#     return(roll_sum(x, window = window,
#                     g = g, weights = weights,
#                     partial = partial,
#                     na.rm = na.rm, ...))
#   }
#   if (anyNA(time)){
#     stop("time index must not contain NA values")
#   }
#   window <- time_by_get(time, time_by = window)
#   time_num <- time_by_num(window)
#   time_unit <- time_by_unit(window)
#   time_subtract <- setnames(list(-time_num), time_unit)
#   window_size <- time_num
#   if (length(window_size) != 1L){
#     stop("time window size must be of length 1")
#   }
#   has_groups <- !is.null(g)
#   check_is_time_or_num(time)
#   g <- GRP2(g, return.groups = FALSE, return.order = FALSE)
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
#     x <- x[group_order]
#     time <- time[group_order]
#     group_id <- group_id[group_order]
#     group_id2 <- group_id2[group_order]
#   }
#   time_start <- time_add2(time, time_by = time_subtract,
#                           roll_month = roll_month,
#                           roll_dst = roll_dst)
#   naive_window <- sequence(group_sizes)
#   if (has_groups){
#     dt <- data.table::data.table(time_start = time_as_number(time_start),
#                                  time = time_as_number(time),
#                                  group_id = group_id)
#     data.table::setattr(dt, "sorted", "group_id")
#     dt[, ("adj_window") := .bincode(get("time_start"), get("time"),
#                                     right = close_left_boundary,
#                                     include.lowest = FALSE),
#        by = "group_id"]
#     adj_window <- dt[["adj_window"]]
#     adj_window[is.na(adj_window)] <- 0L
#   } else {
#     adj_window <- findInterval(time_start, time, left.open = close_left_boundary)
#     # adj_window <- .bincode(as.double(unclass(time_start)),
#     #                        as.double(unclass(time)),
#     #                        right = close_left_boundary,
#     #                        include.lowest = FALSE)
#     # adj_window[is.na(adj_window)] <- 0L
#   }
#   final_window <- naive_window - adj_window
#   out <- frollsum3(x, n = final_window,
#                     weights = weights,
#                     adaptive = TRUE, align = "right",
#                     na.rm = na.rm, ...)
#   if (!partial){
#     elapsed <- time_elapsed(time, time_by = window, g = group_id,
#                             rolling = FALSE)
#     out[double_lt(elapsed, 1)] <- NA_real_
#
#   }
#   # For duplicate times, we take the last mean value of each duplicate
#   out <- glast(out, g = group_id2)
#   if (!groups_are_sorted){
#     out <- collapse::greorder(out, g = g2)
#   }
#   out
# }
# Working alternative
# time_roll_mean2 <- function(x, window,
#                            time = NULL, g = NULL,
#                            weights = NULL,
#                            partial = TRUE,
#                            close_left_boundary = FALSE,
#                            na.rm = TRUE,
#                            time_type = c("auto", "duration", "period"),
#                            roll_month = "preday", roll_dst = "pre",
#                            ...){
#   if (is.null(time)){
#     return(roll_mean(x, window = window,
#                      g = g, weights = weights,
#                      partial = partial,
#                      na.rm = na.rm, ...))
#   }
#   if (anyNA(time)){
#     stop("time index must not contain NA values")
#   }
#   window <- time_by_get(time, time_by = window)
#   time_num <- time_by_num(window)
#   time_unit <- time_by_unit(window)
#   time_subtract <- setnames(list(-time_num), time_unit)
#   window_size <- time_num
#   if (length(window_size) != 1L){
#     stop("time window size must be of length 1")
#   }
#   has_groups <- !is.null(g)
#   check_is_time_or_num(time)
#   g <- GRP2(g, return.groups = FALSE, return.order = FALSE)
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
#     x <- x[group_order]
#     time <- time[group_order]
#     group_id <- group_id[group_order]
#     group_id2 <- group_id2[group_order]
#   }
#   time_start <- time_add2(time, time_by = time_subtract,
#                           roll_month = roll_month,
#                           roll_dst = roll_dst)
#   naive_window <- sequence(group_sizes)
#   if (has_groups){
#     dt <- data.table::data.table(time_start = time_as_number(time_start),
#                                  time = time_as_number(time),
#                                  group_id = group_id)
#     data.table::setattr(dt, "sorted", "group_id")
#     # dt[, ("adj_window") := findInterval(get("time_start"), get("time"),
#     #                                     left.open = close_left_boundary),
#     #    by = "group_id"]
#     dt[, ("adj_window") := .bincode(get("time_start"), get("time"),
#                                     right = close_left_boundary,
#                                     include.lowest = FALSE),
#        by = "group_id"]
#     adj_window <- dt[["adj_window"]]
#     adj_window[is.na(adj_window)] <- 0L
#   } else {
#     adj_window <- findInterval(time_start, time, left.open = close_left_boundary)
#     # adj_window <- .bincode(as.double(unclass(time_start)),
#     #                        as.double(unclass(time)),
#     #                        right = close_left_boundary,
#     #                        include.lowest = FALSE)
#     # adj_window[is.na(adj_window)] <- 0L
#   }
#   final_window <- naive_window - adj_window
#   out <- frollmean3(x, n = final_window,
#                     weights = weights,
#                     adaptive = TRUE, align = "right",
#                     na.rm = na.rm, ...)
#   if (!partial){
#     elapsed <- time_elapsed(time, time_by = window, g = group_id,
#                             rolling = FALSE)
#     out[double_lt(elapsed, 1)] <- NA_real_
#
#   }
#   # For duplicate times, we take the last mean value of each duplicate
#   out <- glast(out, g = group_id2)
#   if (!groups_are_sorted){
#     out <- collapse::greorder(out, g = g2)
#   }
#   out
# }
#' @rdname time_roll
#' @export
time_roll_window <- function(x, window, time,
                             partial = TRUE,
                             close_left_boundary = FALSE,
                             time_type = c("auto", "duration", "period"),
                             roll_month = "preday", roll_dst = "pre"){
  check_index_not_missing(time)
  window <- time_by_list(window)
  time_num <- time_by_num(window)
  time_unit <- time_by_unit(window)
  time_subtract <- setnames(list(-time_num), time_unit)
  start <- time_add2(time, time_by = time_subtract,
                     time_type = time_type,
                     roll_month = roll_month,
                     roll_dst = roll_dst)
  time <- time_cast(time, start)
  window_width <- seq_along(x) -
    findInterval(start, time, left.open = close_left_boundary)
  if (!partial){
    elapsed <- time_elapsed(time, time_by = window, rolling = FALSE)
    window_width[double_lt(elapsed, 1)] <- 0L
  }
  out <- roll_chop(x, sizes = window_width)
  vctrs::new_list_of(out, ptype = x[0L])
}
#' @rdname time_roll
#' @export
time_roll_window_size <- function(x, window,
                                  partial = TRUE,
                                  close_left_boundary = FALSE,
                                  time_type = c("auto", "duration", "period"),
                                  roll_month = "preday", roll_dst = "pre"){
  check_index_not_missing(x)
  window <- time_by_list(window)
  time_num <- time_by_num(window)
  time_unit <- time_by_unit(window)
  time_subtract <- setnames(list(-time_num), time_unit)
  start <- time_add2(x, time_by = time_subtract,
                     time_type = time_type,
                     roll_month = roll_month,
                     roll_dst = roll_dst)
  x <- time_cast(x, start)
  window_width <- seq_along(x) -
    findInterval(start, x, left.open = close_left_boundary)
  window_width
}
