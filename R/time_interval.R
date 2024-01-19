#' S3-based Time Intervals (Currently very experimental and so subject to change)
#'
#' @description
#' Inspired by both 'lubridate' and 'ivs', `time_interval` is a 'vctrs' style
#' class for right-open intervals that contain a vector of start dates and end dates.
#'
#' @returns
#' An object of class `time_interval`. \cr
#' `is_time_interval` returns a logical of length 1. \cr
#' `interval_start` returns the start times. \cr
#' `interval_end` returns the end times. \cr
#' `interval_count` returns a data frame of unique intervals and their counts. \cr
#'
#' @details
#' In the near-future, all time aggregated variables will utilise these intervals.
#' One can control the appearance of the intervals through the "timeplyr.interval_style" option.
#' For example:
#'
#' `options(timeplyr.interval_style = "full")` - Full interval format.
#' `options(timeplyr.interval_style = "start")` - Start time of the interval.
#' `options(timeplyr.interval_style = "end")` - end time of the interval.
#'
#' Representing time using intervals is natural because when one talks about a day or an hour,
#' they are implicitly referring to an interval of time. Even a unit as small as a second
#' is just an interval and therefore base R objects like Dates and POSIXcts are
#' also intervals.
#'
#' @param x A 'time_interval'.
#' @param start Start time. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`, `yearmon`, `yearqtr`,
#' `year_month` or `year_quarter`.
#' @param end End time. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`, `yearmon`, `yearqtr`,
#' `year_month` or `year_quarter`.
#'
#' @seealso [interval_start]
#'
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(lubridate)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' x <- 1:10
#' int <- time_interval(x, 100)
#' options(timeplyr.interval_style = "full")
#' int
#'
#' # Displaying the start or end values of the intervals
#' format(int, "start")
#' format(int, "end")
#'
#' month_start <- floor_date(today(), unit = "months")
#' month_int <- time_interval(month_start, month_start + months(1))
#' month_int
#' # Custom format function for start and end dates
#' format(month_int, interval_sub_formatter =
#'          function(x) format(x, format = "%Y/%B"))
#' format(month_int, interval_style = "start",
#'        interval_sub_formatter = function(x) format(x, format = "%Y/%B"))
#'
#' # Advanced formatting
#'
#' # As shown above, we can specify formatting functions for the dates
#' # in our intervals
#' # Sometimes it's useful to set a default function
#'
#' options(timeplyr.interval_sub_formatter =
#'           function(x) format(x, format = "%b %Y"))
#' month_int
#'
#' # Divide an interval into different time units
#' time_interval(today(), today() + years(0:10)) / "years"
#' time_interval(today(), today() + dyears(0:10)) / ddays(365.25)
#' time_interval(today(), today() + years(0:10)) / "months"
#' time_interval(today(), today() + years(0:10)) / "weeks"
#' time_interval(today(), today() + years(0:10)) / "7 days"
#' time_interval(today(), today() + years(0:10)) / "24 hours"
#' time_interval(today(), today() + years(0:10)) / "minutes"
#' time_interval(today(), today() + years(0:10)) / "seconds"
#' time_interval(today(), today() + years(0:10)) / "milliseconds"
#'
#' # Cutting Sepal Length into blocks of width 1
#' int <- time_aggregate(iris$Sepal.Length, time_by = 1)
#' int %>%
#'   interval_count()
#' reset_timeplyr_options()
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname time_interval
#' @export
time_interval <- function(start = integer(), end = integer()){
  out <- time_interval_list(start, end)
  class(out) <- c("time_interval", "vctrs_rcrd", "vctrs_vctr")
  invisible(vctrs::field) # Just in case accessing namespace is necessary
  out
  # vctrs::new_rcrd(out, class = "time_interval")
}
#' @rdname time_interval
#' @export
is_time_interval <- function(x){
  inherits(x, "time_interval")
}
time_interval_list <- function(start, end){
  set_time_cast(start, end)
  if (typeof(start) == "double" && (
    collapse::anyv(start, Inf) ||
    collapse::anyv(start, -Inf)
  )){
    stop("start must be finite")
  }
  recycle_args(start = start, end = end)
}
# Like time_interval() but no checks or recycling
new_time_interval <- function(start, end){
  out <- list(start = start, end = end)
  class(out) <- c("time_interval", "vctrs_rcrd", "vctrs_vctr")
  out
}
as.data.frame.time_interval <- function(x, ...){
  out <- unclass(x)
  attr(out, "row.names") <- .set_row_names(length(out[[1L]]))
  class(out) <- "data.frame"
  out
  # new_df(start = interval[[1L]],
  #        end = interval[[2L]])
  # new_df(start = interval_start(x),
  #        end = interval_end(x))
}
as.list.time_interval <- function(x, ...){
  unclass(x)
}
`+.time_interval` <- function(e1, e2){
  start <- interval_start(e1)
  end <- interval_end(e1)
  start <- time_add2(start, e2)
  end <- time_add2(end, e2)
  new_time_interval(start, end)
}
`-.time_interval` <- function(e1, e2){
  time_by <- time_by_list(e2)
  time_by <- add_names(
    list(-time_by_num(time_by)
    ),
    time_by_unit(time_by)
  )
  start <- interval_start(e1)
  end <- interval_end(e1)
  start <- time_add2(start, time_by)
  end <- time_add2(end, time_by)
  new_time_interval(start, end)
}
`/.time_interval` <- function(e1, e2){
  time_diff(interval_start(e1), interval_end(e1), time_by = e2)
}
`[.time_interval` <- function(x, i){
  out <- collapse::ss(unclass(x), i = i)
  class(out) <- class(x)
  out
}
# unique.time_interval <- function(x, sort = FALSE, method = "auto",
#                                  decreasing = FALSE, na.last = TRUE, ...){
#   cl <- class(x)
#   out <- as.list(collapse::funique(as.data.frame(x), sort = sort, method = method,
#                                    decreasing = decreasing, na.last = na.last))
#   class(out) <- cl
#   out
# }

# funique.time_interval <- unique.time_interval

# General collapse::funique method for vctrs rcrds
funique.vctrs_rcrd <- function(x, sort = FALSE, method = "auto",
                                 decreasing = FALSE, na.last = TRUE, ...){
  cl <- class(x)
  out <- collapse::funique(unclass(x), sort = sort, method = method,
                                   decreasing = decreasing, na.last = na.last)
  class(out) <- cl
  out
}

unique.time_interval <- funique.vctrs_rcrd

xtfrm.time_interval <- function(x){
  group_id(x, order = TRUE)
}
sort.time_interval <- function(x, ...){
  o <- radixorderv2(unclass(x), ...)
  x[o]
}
duplicated.time_interval <- function(x, ...){
  collapse::fduplicated(unclass(x), ...)
}

as.character.time_interval <- function(x,
                                       interval_style = getOption("timeplyr.interval_style", "full"),
                                       interval_sub_formatter = getOption("timeplyr.interval_sub_formatter", identity),
                                       ...){
  if (is.null(interval_style)){
    int_fmt <- "full"
  } else {
    int_fmt <- rlang::arg_match0(interval_style, c("full", "start", "end"))
  }
  start <- interval_start(x)
  end <- interval_end(x)
  if (!is.null(interval_sub_formatter)){
    start <- interval_sub_formatter(start)
    end <- interval_sub_formatter(end)
  } else {
    start <- as.character(start)
    end <- as.character(end)
  }
  which_na <- cpp_which(is.na(x))
  if (int_fmt == "full"){
    out <- paste0("[", start, ", ", end, ")")
    which_closed <- cpp_which(start == end)
    out[which_closed] <- paste0("[", start[which_closed], ", ", end[which_closed], "]")
    # which_left_open <- cpp_which(start > end)
    # out[which_left_open] <- paste0("(", start[which_left_open], "--", end[which_left_open], "]")
    out[which_na] <- NA_character_
    out
  } else if (int_fmt == "start"){
    start[which_na] <- NA_character_
    start
  } else {
    end[which_na] <- NA_character_
    end
  }
}
format.time_interval <- function(x,
                                 interval_style = getOption("timeplyr.interval_style", "full"),
                                 interval_sub_formatter = getOption("timeplyr.interval_sub_formatter", identity),
                                 ...){
  format(as.character(x,
                      interval_style = interval_style,
                      interval_sub_formatter = interval_sub_formatter), ...)
}
vec_ptype_abbr.time_interval <- function(x, ...) "tm_intv"
vec_ptype_full.time_interval <- function(x, ...) "time_interval"

print.time_interval <- function(x, max = NULL, ...){
  out <- x
  N <- length(out)
  if (is.null(max)){
    max <- getOption("max.print", 9999L)
  }
  max <- min(max, N)
  if (max < N){
    i <- seq_len(max)
    out <- out[i]
    additional_msg <- paste(" [ reached 'max' / getOption(\"max.print\") -- omitted",
                            N - max, "entries ]\n")
  } else {
    additional_msg <- character()
  }
  vctrs::obj_print_header(x)
  vctrs::obj_print_data(out)
  cat(additional_msg)
  invisible(x)
}
as.Date.time_interval <- function(x, ...){
  as.Date(interval_start(x), ...)
}
as.POSIXct.time_interval <- function(x, ...){
  as.POSIXct(interval_start(x), ...)
}
as.POSIXlt.time_interval <- function(x, ...){
  as.POSIXlt(interval_start(x), ...)
}

