.onAttach <- function(...){
  options(
    timeplyr.roll_month = getOption("timeplyr.roll_month", "xlast"),
    timeplyr.roll_dst = getOption("timeplyr.roll_dst", c("NA", "xfirst"))
  )
}
.onUnload <- function(libname, pkgname){
  options(
    timeplyr.roll_month = NULL,
    timeplyr.roll_dst = NULL
  )
}
