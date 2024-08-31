#' Accurate and efficient age calculation
#'
#' @description Correct calculation of ages in years using lubridate periods.
#' Leap year calculations work as well.
#'
#' @param start Start date/datetime, typically date of birth.
#' @param end End date/datetime. Default is current date/datetime.
#'
#' @returns
#' Integer vector of age in years or months.
#'
#' @rdname age_years
#' @export
age_years <- function(start, end = if (is_date(start)) Sys.Date() else Sys.time()){
  check_is_time(start)
  check_is_time(end)
  interval_tbl <- new_df(start = start, end = end, .recycle = TRUE)
  interval_groups <- group2(interval_tbl)
  starts <- attr(interval_groups, "starts")
  sizes <- attr(interval_groups, "group.sizes")
  n_groups <- attr(interval_groups, "N.groups")
  # If distinct pairs results in a 2x reduction in data size, then we do that
  distinct_pairs <- isTRUE((df_nrow(interval_tbl) %/% n_groups) >= 2L)
  if (distinct_pairs){
    interval_tbl <- df_row_slice(interval_tbl, starts)
  }
  start <- interval_tbl$start
  end <- interval_tbl$end
  per <- int_to_per(start, end)
  secs <- per[["second"]]
  mins <- per[["minute"]]
  hours <- per[["hour"]]
  days <- per[["day"]]
  months <- per[["month"]]
  years <- per[["year"]]
  per_as_secs <- secs + 60 * mins + 60 * 60 * hours + 60 * 60 * 24 *
    days + 60 * 60 * 24 * 365.25/12 * months + 60 * 60 *
    24 * 365.25 * years
  out <- as.integer(per_as_secs / 31557600)
  if (distinct_pairs){
    out <- out[interval_groups]
  }
  out
}
#' @rdname age_years
#' @export
age_months <- function(start, end = if (is_date(start)) Sys.Date() else Sys.time()){
  check_is_time(start)
  check_is_time(end)
  interval_tbl <- new_df(start = start, end = end, .recycle = TRUE)
  interval_groups <- group2(interval_tbl)
  starts <- attr(interval_groups, "starts")
  sizes <- attr(interval_groups, "group.sizes")
  n_groups <- attr(interval_groups, "N.groups")
  # If distinct pairs results in a 2x reduction in data size, then we do that
  distinct_pairs <- isTRUE((df_nrow(interval_tbl) %/% n_groups) >= 2L)
  if (distinct_pairs){
    interval_tbl <- df_row_slice(interval_tbl, starts)
  }
  start <- interval_tbl$start
  end <- interval_tbl$end

  per <- int_to_per(start, end)
  sec <- per[["second"]]
  min <- per[["minute"]]
  hour <- per[["hour"]]
  day <- per[["day"]]
  month <- per[["month"]]
  year <- per[["year"]]
  per_as_secs <- sec + 60 * min + 60 * 60 * hour + 60 * 60 * 24 *
    day + 60 * 60 * 24 * 365.25/12 * month + 60 * 60 *
    24 * 365.25 * year
  months <- per_as_secs / 2629800
  if (all_integerable(months)){
    out <- as.integer(months)
  } else {
    out <- trunc(months)
  }
  if (distinct_pairs){
    out <- out[interval_groups]
  }
  out
}
