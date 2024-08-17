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
  scales::new_transform("year_month", unclass, new_year_month)
}
#' @rdname scales
#' @export
transform_year_quarter <- function(){
  scales::new_transform("year_quarter", as.numeric, new_year_quarter)
}
# transform_year_month <- function(){
#   scales::new_transform("year_month", as.Date, year_month)
# }
# transform_year_month <- function(){
#   scales::new_transform("year_month", decimal_year_month, year_month_decimal)
# }
# transform_year_quarter <- function(){
#   scales::new_transform("year_quarter", as.Date, year_quarter)
# }
# scale_x_year_month <- function(...){
#   ggplot2::ggproto("ScaleYearMonth", ggplot2::scale_x_continuous(...),
#                    trans = transform_year_month())
# }
# }
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
# scale_x_yearmon <- function(...){
#   ggplot2::ggproto(NULL, ggplot2::scale_x_continuous(...),
#                    labels = as_yearmon)
# }
# scale_y_yearmon <- function(...){
#   ggplot2::ggproto(NULL, ggplot2::scale_y_continuous(...),
#                    labels = as_yearmon)
# }
# scale_x_year_quarter <- function(...){
#   ggplot2::ggproto("ScaleYearQuarter", ggplot2::scale_x_continuous(...),
#                    trans = transform_year_quarter())
# }
# scale_x_date2 <- function(...){
#   ggplot2::ggproto("ScaleDate2", ggplot2::scale_x_date(...), get_breaks = ggplot_time_breaks)
# }

