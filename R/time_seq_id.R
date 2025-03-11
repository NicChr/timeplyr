#' Generate a unique identifier for a regular time sequence with gaps
#'
#' @description
#' A unique identifier is created every time a specified amount of
#' time has passed, or in the case of regular sequences, when there is a gap
#' in time.
#'
#' @param x Time vector. \cr
#' E.g. a `Date`, `POSIXt`, `numeric` or any time-based vector.
#' @param timespan [timespan].
#' @param threshold Threshold such that when the time elapsed
#' exceeds this, the sequence ID is incremented by 1.
#' For example, if `timespan = "days"` and `threshold = 2`,
#' then when 2 days have passed, a new ID is created.
#' Furthermore, `threshold` generally need not be supplied as \cr
#' `timespan = "3 days"` & `threshold = 1` \cr
#' is identical to \cr
#' `timespan = "days"` & `threshold = 3`. \cr
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
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # Weekly sequence, with 2 gaps in between
#' x <- time_seq(today(), length.out = 10, time = "week")
#' x <- x[-c(3, 7)]
#' # A new ID when more than a week has passed since the last time point
#' time_seq_id(x)
#' # A new ID when >= 2 weeks has passed since the last time point
#' time_seq_id(x, threshold = 2, switch_on_boundary = TRUE)
#' # A new ID when at least 4 cumulative weeks have passed
#' time_seq_id(x, timespan = "4 weeks",
#'             switch_on_boundary = TRUE, rolling = FALSE)
#' # A new ID when more than 4 cumulative weeks have passed
#' time_seq_id(x, timespan = "4 weeks",
#'             switch_on_boundary = FALSE, rolling = FALSE)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
time_seq_id <- function(x, timespan = granularity(x),
                        threshold = 1, g = NULL, na_skip = TRUE,
                        rolling = TRUE, switch_on_boundary = FALSE){
  check_is_time_or_num(x)
  g <- GRP2(g)
  timespan <- timespan(timespan)
  time_num <- timespan_num(timespan)
  if (is_whole_number(threshold)){
    threshold <- as.integer(threshold)
  }
  min_threshold <- min(threshold)
  if (is_integerable(min_threshold - 1)){
    fill <- -abs(min_threshold) - 1L
  } else {
    fill <- -Inf
  }
  # Elapsed time
  telapsed <- time_elapsed(x, timespan, g = g,
                           rolling = rolling, na_skip = na_skip,
                           fill = fill)
  if (rolling){
    if (switch_on_boundary){
      over_threshold <- cppdoubles::double_gte(telapsed, threshold)
    } else {
      over_threshold <- cppdoubles::double_gt(telapsed, threshold)
    }
  } else {
    dt <- new_dt(x = telapsed, group_id = fastplyr::group_id(g), .copy = FALSE)
    group_id_col <- names(dt)[names(dt) == "group_id"]
    dt[, ("over") :=
         lapply(
           .SD, function(x)
             cpp_roll_time_threshold(x, threshold = threshold, switch_on_boundary = switch_on_boundary)
         ),
       by = group_id_col,
       .SDcols = "x"]
    over_threshold <- dt[["over"]]
  }
  collapse::fcumsum(over_threshold, g = g, na.rm = na_skip) + 1L
}
