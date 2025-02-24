#' Reset 'timeplyr' options
#'
#' @returns
#' Resets the timeplyr global options (prefixed with `"timeplyr."`): \cr
#' roll_month & roll_dst.
#'
#' @export
reset_timeplyr_options <- function(){
  options(timeplyr.roll_month = "postday", timeplyr.roll_dst = c("NA", "xfirst"))
}
