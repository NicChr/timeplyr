#' Generate a unique identifier for a regular time sequence with gaps
#'
#' @description A unique identifier is created every time a specified amount of
#' time has passed, or in the case of regular sequences, when there is a gap
#' in time.
#'
#' @param x Date, datetime or numeric vector.
#' @param time_by Time unit. \cr
#' This signifies the granularity of the time data with which to measure gaps
#' in the sequence.
#' If your data is daily for example, supply `time_by = "days"`.
#' If weekly, supply `time_by = "week"`.
#' Must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param threshold Threshold such that when the time elapsed
#' exceeds this, the sequence ID is incremented by 1.
#' For example, if `time_by = "days"` and `threshold = 2`,
#' then when 2 days have passed, a new ID is created.
#' Furthermore, `threshold` generally need not be supplied as \cr
#' `time_by = "3 days"` & `threshold = 1` \cr
#' is identical to \cr
#' `time_by = "days"` & `threshold = 3`. \cr
#' @param g Object used for grouping x.
#' This can for example be a vector or data frame.
#' `g` is passed directly to `collapse::GRP()`.
#' @param na_skip Should `NA` values be skipped? Default is `TRUE`.
#' @param rolling When this is `FALSE`, a new ID is created every time
#' a cumulative amount of time has passed. Once that amount of time has passed,
#' a new ID is created, the clock "resets" and we start counting from that point.
#' @param switch_on_boundary When an exact amount of time
#' (specified in `time_by`) has passed, should there an increment in ID?
#' The default is `FALSE`. For example, if `time_by = "days"` and
#' `switch_on_boundary = FALSE`, `>` 1 day must have passed, otherwise
#' `>=` 1 day must have passed.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(lubridate)
#' # Weekly sequence, with 2 gaps in between
#' x <- time_seq(today(), length.out = 10, time_by = "week")
#' x <- x[-c(3, 7)]
#' # A new ID when more than a week has passed since the last time point
#' time_seq_id(x, time_by = "week")
#' # A new ID when >= 2 weeks has passed since the last time point
#' time_seq_id(x, time_by = "weeks", threshold = 2, switch_on_boundary = TRUE)
#' # A new ID when at least 4 cumulative weeks have passed
#' time_seq_id(x, time_by = "4 weeks",
#'             switch_on_boundary = TRUE, rolling = FALSE)
#' # A new ID when more than 4 cumulative weeks have passed
#' time_seq_id(x, time_by = "4 weeks",
#'             switch_on_boundary = FALSE, rolling = FALSE)
#' @export
time_seq_id <- function(x, time_by = NULL, threshold = 1,
                        g = NULL, na_skip = TRUE,
                        rolling = TRUE, switch_on_boundary = FALSE,
                        time_type = c("auto", "duration", "period")){
  if (!is.null(g)){
    g <- GRP2(g)
    group_start_locs <- GRP_starts(g)
  } else {
    group_start_locs <- min(1L, length(x))
  }
  time_by <- time_by_get(x, time_by = time_by, is_sorted = FALSE)
  time_num <- time_by_num(time_by)
  # Elapsed time
  telapsed <- time_elapsed(x, time_by = time_by, g = g,
                           time_type = time_type, rolling = rolling,
                           na_skip = na_skip, fill = 0)
  # Check x is in ascending order
  check_time_elapsed_order(telapsed)
  tol <- sqrt(.Machine$double.eps)
  # x > y
  # over_threshold <- (telapsed - threshold) > tol
  # x > y OR x == y
  # over_threshold <- over_threshold | abs(telapsed - threshold) < tol
  if (rolling){
    if (switch_on_boundary){
      over_threshold <- (telapsed - threshold) >= (0 - tol)
    } else {
      over_threshold <- (telapsed - threshold) > (tol)
    }
  } else {
    if (!is.null(g)){
      dt <- data.table::data.table(x = telapsed, group_id = GRP_group_id(g))
      over_threshold <- dt[, ("over") :=
                             roll_time_threshold(get("x"),
                                                 threshold = threshold,
                                                 switch_on_boundary = switch_on_boundary,
                                                 tol = tol),
                           by = "group_id"][["over"]]
    } else {
      over_threshold <- roll_time_threshold(telapsed,
                                            threshold = threshold,
                                            switch_on_boundary = switch_on_boundary,
                                            tol = tol)
    }
  }
  # Make sure first ID is always 1
  setv(over_threshold, group_start_locs, 0L, vind1 = TRUE)
  collapse::fcumsum(over_threshold, g = g, na.rm = na_skip) + 1L
}
# time_seq_id <- function(x, time_by = NULL,
#                         g = NULL, na_skip = TRUE,
#                         rolling = TRUE, switch_on_boundary = FALSE,
#                         time_type = c("auto", "duration", "period")){
#   if (!is.null(g)){
#     g <- GRP2(g)
#     group_start_locs <- GRP_starts(g)
#   } else {
#     group_start_locs <- min(1L, length(x))
#   }
#   threshold <- 1
#   time_by <- time_by_get(x, time_by = time_by, is_sorted = FALSE)
#   time_num <- time_by_num(time_by)
#   telapsed <- time_elapsed(x, time_by = time_by, g = g,
#                            time_type = time_type, rolling = rolling,
#                            na_skip = na_skip, fill = 0)
#   check_time_elapsed_order(telapsed)
#   tol <- sqrt(.Machine$double.eps)
#   if (rolling){
#     # if (switch_on_boundary){
#     #   # over_threshold <- (telapsed - threshold + tol) >= tol
#     #   # over_threshold <- (telapsed - threshold + tol) >= 0
#     #   over_threshold <- (telapsed - threshold) >= (-tol)
#     # } else {
#     #   over_threshold <- (telapsed - threshold) > tol
#     # }
#     over_threshold <- (telapsed - threshold) > tol
#     if (switch_on_boundary){
#       over_threshold <- over_threshold | abs(telapsed - threshold) < tol
#     }
#   } else {
#     if (!is.null(g)){
#       dt <- data.table::data.table(x = telapsed, group_id = GRP_group_id(g))
#       # IN PROGRESS..
#       # By doing the below, we force a new threshold to be exceeded at the
#       # first non-NA value of each group, thus not needed to do a grouped
#       # calculation...
#       # This should only work if data is sorted by groups...
#       # setorderv2(dt, cols = "group_id")
#       # temp_group <- collapse::group(dt[["group_id"]], starts = TRUE)
#       # cumsum_not_na <- collapse::fcumsum(!is.na(dt[["x"]]), g = temp_group)
#       # i_replace <- which(cumsum_not_na == 1L) - 1L
#       # i_replace <- i_replace[i_replace > 1L]
#       # # i_replace <- which(cumsum_not_na == 0L &  # First NA values
#       # #                      cumsum_not_na != collapse::flag(cumsum_not_na, n = -1L)) # Next value not NA
#       # data.table::set(dt,
#       #                 j = "over2",
#       #                 value = dt[["x"]])
#       # data.table::set(dt,
#       #                 i = i_replace,
#       #                 j = "over2",
#       #                 value = time_num + 1)
#       # data.table::set(dt,
#       #                 j = "over2",
#       #                 value = roll_time_threshold(dt[["over2"]],
#       #                                             switch_on_boundary =
#       #                                               switch_on_boundary))
#       # # data.table::set(dt,
#       # #                 i = i_replace,
#       # #                 # i = fnmiss(dt[["x"]], g = temp_group) +
#       # #                 #   attr(temp_group, "starts"),
#       # #                 j = "x",
#       # #                 value = time_num + 1)
#       # data.table::set(dt, j = "over",
#       #                 value = roll_time_threshold(dt[["x"]],
#       #                                             switch_on_boundary =
#       #                                               switch_on_boundary))
#       # data.table::set(dt,
#       #                 i = i_replace,
#       #                 j = "over2",
#       #                 value = dt[["over"]][i_replace])
#       # over_threshold <- collapse::greorder(dt[["over2"]], g = g)
#       # The below is preferable but slow with many groups
#       over_threshold <- dt[, ("over") :=
#                              roll_time_threshold(get("x"),
#                                                  threshold = threshold,
#                                                  switch_on_boundary = switch_on_boundary),
#                            by = "group_id"][["over"]]
#     } else {
#       over_threshold <- roll_time_threshold(telapsed,
#                                             threshold = threshold,
#                                             switch_on_boundary = switch_on_boundary)
#     }
#   }
#   # Make sure first ID is always 1
#   over_threshold[group_start_locs] <- 0L
#   collapse::fcumsum(over_threshold, g = g, na.rm = na_skip) + 1L
# }
# time_seq_id <- function(x, time_by = NULL,
#                         g = NULL, time_type = c("auto", "duration", "period"),
#                         na_skip = TRUE){
#   if (!is.null(g)){
#     g <- GRP2(g)
#   }
#   telapsed <- time_elapsed(x, time_by = time_by, g = g,
#                            time_type = time_type, rolling = TRUE,
#                            na_skip = na_skip, fill = 0)
#   check_time_elapsed_order(telapsed)
#   tol <- sqrt(.Machine$double.eps)
#   out <- collapse::fcumsum((telapsed - 1) > tol,
#                            g = g, na.rm = na_skip) + 1L
#   out
# }
# time_seq_id <- function(x, time_by = NULL,
#                         g = NULL, time_type = c("auto", "duration", "period"),
#                         na_skip = TRUE){
#   if (!is.null(g)){
#     g <- GRP2(g)
#   }
#   telapsed <- time_elapsed(x, time_by = time_by, g = g,
#                            time_type = time_type, rolling = TRUE,
#                            na_skip = na_skip, fill = 0)
#   # if (!rolling){
#   #   telapsed <- fdiff2(telapsed, g = g, fill = 0)
#   # }
#   # if (na_skip){
#   #   is_na <- is.na(telapsed)
#   #   telapsed[is_na] <- 0
#   #   telapsed <- fdiff2(telapsed, g = g, fill = 0)
#   #   telapsed[is_na] <- NA
#   # } else {
#   #   telapsed <- fdiff2(telapsed, g = g, fill = 0)
#   # }
#   # if (!is.null(time_by)){
#   #   check_time_elapsed_regular(telapsed)
#   # }
#   check_time_elapsed_order(telapsed)
#   tol <- sqrt(.Machine$double.eps)
#   out <- collapse::fcumsum((telapsed - 1) > tol,
#                            g = g, na.rm = na_skip) + 1L
#   # if (na_skip){
#   #   na_telapsed <- is.na(telapsed)
#   #   has_nas <- any(na_telapsed)
#   # }
#   # if (na_skip && has_nas){
#   #   out <- logical(length(telapsed))
#   #   out[!na_telapsed] <- (telapsed[!na_telapsed] - 1) > tol
#   #   out <- collapse::fcumsum(out, g = g, na.rm = na_skip) + 1L
#   # } else {
#   #   out <- collapse::fcumsum((telapsed - 1) > tol,
#   #                            g = g, na.rm = na_skip) + 1L
#   # }
#   out
# }
# time_seq_id <- function(x, time_by = NULL,
#                         g = NULL, time_type = c("auto", "duration", "period"),
#                         na_skip = FALSE){
#   if (!is.null(g)){
#     g <- GRP2(g)
#   }
#   telapsed <- time_elapsed(x, time_by = time_by, g = g,
#                            time_type = time_type, rolling = TRUE,
#                            na_skip = na_skip, fill = 0)
#   if (!is.null(time_by)){
#     check_time_elapsed_regular(telapsed)
#   }
#   check_time_elapsed_order(telapsed)
#   if (na_skip){
#     out <- logical(length(telapsed))
#     na_telapsed <- is.na(telapsed)
#     out[!na_telapsed] <- telapsed[!na_telapsed] > 1
#     out <- collapse::fcumsum(out, g = g, na.rm = na_skip) + 1L
#   } else {
#     out <- collapse::fcumsum(telapsed > 1, g = g, na.rm = na_skip) + 1L
#   }
#   out
# }
