#' Reset 'timeplyr' options
#'
#' @returns
#' Resets the timeplyr global options (prefixed with `"timeplyr."`): \cr
#' roll_month & roll_dst.
#'
#' @export
reset_timeplyr_options <- function(){
  # options(.initial_options)
  options("timeplyr.roll_month" = "preday",
          "timeplyr.roll_dst" = "boundary")
}
