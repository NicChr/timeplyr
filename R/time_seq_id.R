#' Generate a unique identifier for a regular time sequence with gaps
#'
#' @description
#' A unique identifier is created every time a specified amount of
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
#'
#' @returns
#' An integer vector of `length(x)`.
#'
#' @details
#' `time_seq_id()` Assumes `x` is regular and in
#' ascending or descending order.
#' To check this condition formally, use `time_is_regular()`.
#'
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(lubridate)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
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
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
time_seq_id <- function(x, time_by = NULL, threshold = 1,
                        g = NULL, na_skip = TRUE,
                        rolling = TRUE, switch_on_boundary = FALSE,
                        time_type = c("auto", "duration", "period")){
  check_is_time_or_num(x)
  g <- GRP2(g)
  time_by <- time_by_get(x, time_by = time_by, is_sorted = FALSE)
  time_num <- time_by_num(time_by)
  # Elapsed time
  telapsed <- time_elapsed(x, time_by = time_by, g = g,
                           time_type = time_type, rolling = rolling,
                           na_skip = na_skip, fill = -Inf)
  if (rolling){
    if (switch_on_boundary){
      over_threshold <- double_gte(telapsed, threshold)
    } else {
      over_threshold <- double_gt(telapsed, threshold)
    }
  } else {
    dt <- data.table::data.table(x = telapsed, group_id = GRP_group_id(g))
    group_id_col <- names(dt)[names(dt) == "group_id"]
    over_threshold <- dt[, ("over") :=
                           roll_time_threshold(get("x"), threshold = threshold,
                                               switch_on_boundary = switch_on_boundary),
                         by = group_id_col][["over"]]
  }
  collapse::fcumsum(over_threshold, g = g, na.rm = na_skip) + 1L
}
