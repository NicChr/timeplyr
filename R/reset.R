#' Reset 'timeplyr' options
#'
#' @returns
#' Resets the timeplyr global options (prefixed with `"timeplyr."`): \cr
#' time_type, roll_month, roll_dst, interval_style, and
#' interval_sub_formatter.
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
          "timeplyr.use_intervals" = FALSE)
}
