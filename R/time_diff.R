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

  if (units %in% c("days", "weeks") &&
      is_date(x) &&
      is_date(y) &&
      is_whole_number(num)){
    if (units == "weeks"){
      num <- num * 7L
    }
    out <- divide(unclass(y) - unclass(x), num)
  } else if (!timespan_has_unit(span)){
    set_time_cast(y, x)
    out <- divide(unclass(y) - unclass(x), num)
  } else {
    x <- as_datetime2(x)
    y <- as_datetime2(y)
    if (!is_duration_unit(units)){
      # Use distinct start/end pairs (intervals)
      # Instead of all of them because it's usually more efficient
      interval_tbl <- cheapr::new_df(x = x, y = y, num = num, .recycle = TRUE)
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

      span <- new_timespan(units, abs(num))
      out <- sign(num) * divide_interval_by_period(x, y, span)
      zero <- num == 0
      out[which(zero & x > y)] <- -Inf
      out[which(zero & x < y)] <- Inf
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
# diff_months <- function(x, y, fractional = FALSE){
#   # start <- unclass(as.POSIXlt(x))
#   # end <- unclass(as.POSIXlt(y))
#   # sy <- start[["year"]]
#   # sm <- start[["mon"]]
#   # ey <- end[["year"]]
#   # em <- end[["mon"]]
#   # smd <- start[["mday"]]
#   # emd <- end[["mday"]]
#
#   start <- data.table::as.IDate(x)
#   end <- data.table::as.IDate(y)
#   sy <- data.table::year(start)
#   sm <- data.table::month(start)
#   ey <- data.table::year(end)
#   em <- data.table::month(end)
#   smd <- data.table::mday(start)
#   emd <- data.table::mday(end)
#
#   out <- 12L * (ey - sy) + (em - sm)
#
#   # Adjust for full months
#   pos <- cheapr::val_find(out > 0L, TRUE)
#   neg <- cheapr::val_find(out < 0L, TRUE)
#   out[pos] <- out[pos] - (emd < smd)[pos]
#   out[neg] <- out[neg] + (emd > smd)[neg]
#   # out[pos] <- out[pos] - (emd[pos] < smd[pos])
#   # out[neg] <- out[neg] + (emd[neg] > smd[neg])
#   out
# }
# diff_months <- function(x, y, fractional = FALSE){
#
#   start <- data.table::as.IDate(x)
#   end <- data.table::as.IDate(y)
#   sy <- data.table::year(start)
#   sm <- data.table::month(start)
#   ey <- data.table::year(end)
#   em <- data.table::month(end)
#   smd <- data.table::mday(start)
#   emd <- data.table::mday(end)
#
#   out <- 12L * (ey - sy) + (em - sm)
#
#   sign <- cheapr::int_sign(out)
#
#   # Adjust for full months
#   pos <- cheapr::val_find(sign, 1L)
#   neg <- cheapr::val_find(sign, -1L)
#   out[pos] <- out[pos] - (emd < smd)[pos]
#   out[neg] <- out[neg] + (emd > smd)[neg]
#
#   # Fractional through the month
#   if (fractional){
#     start_dt <- as_datetime2(x)
#     end_dt <- as_datetime2(y)
#     int_end1 <- C_time_add(start_dt, list(month = out), "preday", "NA")
#     int_end2 <- C_time_add(int_end1, list(month = 1), "preday", "NA")
#     fraction <- strip_attrs((unclass(end_dt) - unclass(int_end1)) / (unclass(int_end2) - unclass(int_end1)))
#     out <- out + fraction
#   }
#
#   out
# }


diff_months <- function(x, y, fractional = FALSE){

  start <- data.table::as.IDate(x)
  end <- data.table::as.IDate(y)
  sy <- data.table::year(start)
  sm <- data.table::month(start)
  ey <- data.table::year(end)
  em <- data.table::month(end)
  smd <- data.table::mday(start)
  emd <- data.table::mday(end)

  out <- (12L * (ey - sy)) + (em - sm)

  sign <- cheapr::int_sign(out)

  # Adjust for full months
  pos <- cheapr::val_find(sign, 1L)
  neg <- cheapr::val_find(sign, -1L)
  out[pos] <- out[pos] - (emd < smd)[pos]
  out[neg] <- out[neg] + (emd > smd)[neg]

  # Fractional through the month
  if (fractional){
    zero <- cheapr::val_find(sign, 0L)
    start_dt <- as_datetime2(x)
    end_dt <- as_datetime2(y)
    int_end1 <- C_time_add(start_dt, list(month = out), "preday", "NA")
    int_end2 <- int_end1 # Initialise
    int_end2[pos] <- C_time_add(int_end1[pos], list(month = 1), "preday", "NA")
    int_end2[neg] <- C_time_add(int_end1[neg], list(month = -1), "preday", "NA")
    int_end2[zero] <- C_time_add(int_end1[zero], list(month = 1), "preday", "NA")
    fraction <- strip_attrs(
      (unclass(end_dt) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
    )
    out <- out + fraction
  }
  out
}

diff_years <- function(x, y, fractional = FALSE){

  start <- data.table::as.IDate(x)
  end <- data.table::as.IDate(y)
  sy <- data.table::year(start)
  ey <- data.table::year(end)
  syd <- data.table::yday(start)
  eyd <- data.table::yday(end)

  out <- ey - sy

  sign <- cheapr::int_sign(out)

  # Adjust for full months
  pos <- cheapr::val_find(sign, 1L)
  neg <- cheapr::val_find(sign, -1L)
  out[pos] <- out[pos] - (eyd < syd)[pos]
  out[neg] <- out[neg] + (eyd > syd)[neg]

  # Fractional through the year
  if (fractional){
    zero <- cheapr::val_find(sign, 0L)
    start_dt <- as_datetime2(x)
    end_dt <- as_datetime2(y)
    int_end1 <- C_time_add(start_dt, list(year = out), "preday", "NA")
    int_end2 <- int_end1 # Initialise
    int_end2[pos] <- C_time_add(int_end1[pos], list(year = 1), "preday", "NA")
    int_end2[neg] <- C_time_add(int_end1[neg], list(year = -1), "preday", "NA")
    int_end2[zero] <- C_time_add(int_end1[zero], list(year = 1), "preday", "NA")
    fraction <- strip_attrs(
      (unclass(end_dt) - unclass(int_end1)) / abs(unclass(int_end2) - unclass(int_end1))
    )
    out <- out + fraction
  }

  out
}
# diff_years <- function(x, y, fractional = FALSE){
#
#   start <- data.table::as.IDate(x)
#   end <- data.table::as.IDate(y)
#   sy <- data.table::year(start)
#   ey <- data.table::year(end)
#   syd <- data.table::yday(start)
#   eyd <- data.table::yday(end)
#
#   out <- ey - sy
#
#   sign <- cheapr::int_sign(out)
#
#   # Adjust for full months
#   pos <- cheapr::val_find(sign, 1L)
#   neg <- cheapr::val_find(sign, -1L)
#   out[pos] <- out[pos] - (eyd < syd)[pos]
#   out[neg] <- out[neg] + (eyd > syd)[neg]
#
#   # Fractional through the year
#   if (fractional){
#     start_dt <- as_datetime2(x)
#     end_dt <- as_datetime2(y)
#     int_end1 <- C_time_add(start_dt, list(year = out), "preday", "NA")
#     int_end2 <- C_time_add(int_end1, list(year = 1), "preday", "NA")
#     fraction <- strip_attrs((unclass(end_dt) - unclass(int_end1)) / (unclass(int_end2) - unclass(int_end1)))
#     out <- out + fraction
#   }
#
#   out
# }

