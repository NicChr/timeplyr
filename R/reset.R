#' Reset 'timeplyr' options
#'
#' @returns
#' Resets the timeplyr global options (prefixed with `"timeplyr."`): \cr
#' time_type, roll_month, roll_dst, interval_style,
#' interval_sub_formatter and use_intervals.
#'
#' @description
#' One can set global options to be used in timeplyr. These options include:
#' * time_type - Controls whether to use periods, durations or to decide automatically.
#' * roll_month - Controls how to roll forward or backward impossible calendar days.
#' * roll_dst - Controls how to roll forward or backward impossible date-times.
#' * interval_style - Controls how `time_interval` objects are formatted.
#' * interval_sub_formatter - A function to format the start and end times of a `time_interval`.
#' * use_intervals - Controls whether `time_intervals` are
#' returned whenever dates or date-times are aggregated. If this is `FALSE`
#' the start time (or left-hand side) is always returned.
#'
#'
#' @examples
#' library(timeplyr)
#' options(timeplyr.interval_style = "start")
#' getOption("timeplyr.interval_style")
#' reset_timeplyr_options()
#' getOption("timeplyr.interval_style")
#'
#' @export
reset_timeplyr_options <- function(){
  # options(.initial_options)
  options("timeplyr.time_type" = "auto",
          "timeplyr.roll_month" = "preday",
          "timeplyr.roll_dst" = "boundary",
          "timeplyr.interval_style" = "full",
          "timeplyr.interval_sub_formatter" = identity,
          "timeplyr.use_intervals" = TRUE)
}
