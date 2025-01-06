#' Time differences by any time unit
#'
#' @description
#' The time difference between 2 date or date-time vectors.
#'
#' @param x Start date or datetime.
#' @param y End date or datetime.
#' @param timespan A [timespan] used to divide the difference.
#'
#' @returns
#' A numeric vector recycled to the length of `max(length(x), length(y))`.
#'
#' @examples
#' library(timeplyr)
#' library(lubridate)
#' time_diff(today(), today() + days(10), "days")
#' time_diff(today(), today() + days((0:3) * 7), weeks(1))
#' time_diff(today(), today() + days(100), timespan("days", 1:100))
#' time_diff(1, 1 + 0:100, 3)
#'
#' @export
time_diff <- function(x, y, timespan = 1L){
  span <- timespan(timespan)
  units <- timespan_unit(span)
  num <- timespan_num(span)

  if (!timespan_has_unit(span)){
    set_time_cast(y, x)
    if (!is.numeric(y)){
      y <- unclass(y)
    }
    if (!is.numeric(x)){
      x <- unclass(x)
    }
    out <- divide(y - x, num)
  } else {
    x <- as_datetime2(x)
    y <- as_datetime2(y)
    if (!is_duration_unit(units)){
      # Use distinct start/end pairs (intervals)
      # Instead of all of them because it's usually more efficient
      interval_tbl <- new_df(x = x, y = y, num = num, .recycle = TRUE)
      interval_groups <- collapse::group(interval_tbl, starts = TRUE, group.sizes = TRUE)
      starts <- attr(interval_groups, "starts")
      sizes <- attr(interval_groups, "group.sizes")
      n_groups <- attr(interval_groups, "N.groups")
      # If distinct pairs results in a 2x reduction in data size, then we do that
      distinct_pairs <- isTRUE((df_nrow(interval_tbl) %/% n_groups) >= 2L)
      if ( distinct_pairs ){
        interval_tbl <- df_row_slice(interval_tbl, starts)
      }
      x <- interval_tbl$x
      y <- interval_tbl$y
      num <- interval_tbl$num

      unit <- period_unit(units)(abs(num))
      out <- sign(num) * divide_interval_by_period2(x, y, unit)
      out[which(num == 0 & x > y)] <- -Inf
      out[which(num == 0 & x < y)] <- Inf
      # Expand them back to original length
      if (distinct_pairs){
        out <- out[interval_groups]
      }
    } else {
      x <- unclass(x)
      y <- unclass(y)
      by <- unit_to_seconds(span)
      out <- (y - x) / by
    }
  }
  strip_attrs(out)
}
