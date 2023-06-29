#' Generate a unique identifier for a regular time sequence with gaps
#'
#' @description A unique identifier is created every time there is a gap in the
#' regular time sequence.
#'
#' @param x Date, datetime or numeric vector.
#' @param time_by Time unit. \cr
#' Must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param g Object used for grouping x.
#' This can for example be a vector or data frame.
#' `g` is passed directly to `collapse::GRP()`.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @param na_skip Should `NA` values be skipped? Default is `TRUE`.
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(lubridate)
#' # 3 Monthly sequences, with a month gap in between each
#' x <- time_seq_v2(rep(11, 3), # 3 sequences of lengths 11 each
#'                  from = today() + years(0:2),
#'                  time_by = "month")
#' time_seq_id(x, time_by = "month")
#' time_gaps(x, time_by = "month") # Time gaps
#' @export
time_seq_id <- function(x, time_by = NULL,
                        g = NULL, time_type = c("auto", "duration", "period"),
                        na_skip = TRUE){
  if (!is.null(g)){
    g <- GRP2(g)
  }
  telapsed <- time_elapsed(x, time_by = time_by, g = g,
                           time_type = time_type, rolling = FALSE,
                           na_skip = na_skip)
  telapsed <- fdiff2(telapsed, g = g, fill = 0)
  if (!is.null(time_by)){
    check_time_elapsed_regular(telapsed)
  }
  check_time_elapsed_order(telapsed)
  tol <- sqrt(.Machine$double.eps)
  if (na_skip){
    na_telapsed <- is.na(telapsed)
    has_nas <- any(na_telapsed)
  }
  if (na_skip && has_nas){
    out <- logical(length(telapsed))
    out[!na_telapsed] <- (telapsed[!na_telapsed] - 1) > tol
    out <- collapse::fcumsum(out, g = g, na.rm = na_skip) + 1L
  } else {
    out <- collapse::fcumsum((telapsed - 1) > tol,
                             g = g, na.rm = na_skip) + 1L
  }
  out
}
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
