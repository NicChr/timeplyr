#' Additional ggplot2 scales
#'
#' @description
#' Additional scales and transforms for use with year_months and year_quarters
#' in ggplot2.
#'
#' @param ... Arguments passed to `scale_x_continuous` and `scale_y_continuous`.
#'
#' @returns
#' A ggplot2 scale or transform.
#'
#' @rdname scales
#' @export
transform_year_month <- function(){
  scales::new_transform("year_month", as.numeric, new_year_month)
}
#' @rdname scales
#' @export
transform_year_quarter <- function(){
  scales::new_transform("year_quarter", as.numeric, new_year_quarter)
}
#' @rdname scales
#' @export
scale_x_year_month <- function(...){
  ggplot2::ggproto(NULL, ggplot2::scale_x_continuous(...),
                   trans = transform_year_month())
}
#' @rdname scales
#' @export
scale_x_year_quarter <- function(...){
  ggplot2::ggproto(NULL, ggplot2::scale_x_continuous(...),
                   trans = transform_year_quarter())
}
#' @rdname scales
#' @export
scale_y_year_month <- function(...){
  ggplot2::ggproto(NULL, ggplot2::scale_y_continuous(...),
                   trans = transform_year_month())
}
#' @rdname scales
#' @export
scale_y_year_quarter <- function(...){
  ggplot2::ggproto(NULL, ggplot2::scale_y_continuous(...),
                   trans = transform_year_quarter())
}
# transform_time_interval <- function(width, endpoints_inverse, ...){
#   scales::new_transform("time_interval", transform = \(x) cheapr::attrs_clear(unclass(x)),
#                         inverse = \(x){
#                           inv <- endpoints_inverse(x)
#                           time_interval(inv, width)
#                         },
#                         ...)
# }
# Not sure how to make a time_interval ggplot scale without having
# the user supply the interval width
# The scale_ function needs to access the time_interval directly
# scale_x_time_interval_date <- function(width, ...){
#   ggplot2::ggproto(NULL, ggplot2::scale_x_continuous(...),
#                    trans = transform_time_interval(width, endpoints_inverse = .Date))
# }
# scale_x_time_interval_datetime <- function(width, ...){
#   ggplot2::ggproto(NULL, ggplot2::scale_x_continuous(...),
#                    trans = transform_time_interval(
#                      width, endpoints_inverse = .POSIXct
#                      ))
# }
