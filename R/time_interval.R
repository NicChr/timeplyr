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
#' int <- time_aggregate(iris$Sepal.Length, time_by = 1, as_interval = TRUE)
#' int %>%
#'   interval_count()
#' reset_timeplyr_options()
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#' }
#' @rdname time_interval
#' @export
time_interval <- function(start = integer(), width = time_resolution(start)){

  timespan <- timespan(width)

  if (length(timespan) > 1){
    cli::cli_abort("{.arg num} must be a length-1 timespan")
  }

  if (isTRUE(any(timespan_num(timespan) <= 0, na.rm = TRUE))){
    cli::cli_abort("{.arg width} must be a positive-valued timespan")
  }

  # Basically if width is unitless (e.g. width = 10)
  # Then we use the unit of the time resolution
  if (!timespan_has_unit(timespan)){
    resolution <- time_resolution(start)
    timespan[["unit"]] <- timespan_unit(resolution)
  }
  new_time_interval(start, timespan)
}
#' @rdname time_interval
#' @export
is_time_interval <- function(x){
  inherits(x, "time_interval")
}
# Like time_interval() but no checks or recycling
new_time_interval <- function(start, timespan){
  out <- start
  attr(out, "timespan") <- timespan
  class(out) <- c("time_interval", oldClass(start))
  out
}
#' @rdname time_interval
#' @export
`[.time_interval` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  span <- interval_width(x)
  class(x) <- NULL
  val <- NextMethod("[")
  class(val) <- cl
  attr(val, "timespan") <- span
  val
}
#' @rdname time_interval
#' @export
`rep.time_interval` <- function(x, ...){
  cl <- oldClass(x)
  span <- interval_width(x)
  class(x) <- NULL
  val <- NextMethod("rep")
  class(val) <- cl
  attr(val, "timespan") <- span
  val
}
#' @rdname time_interval
#' @export
`rep_len.time_interval` <- function(x, ...){
  cl <- oldClass(x)
  span <- interval_width(x)
  class(x) <- NULL
  val <- NextMethod("rep_len")
  class(val) <- cl
  attr(val, "timespan") <- span
  val
}

#' @export
`+.time_interval` <- function(e1, e2){
  start <- interval_start(e1)
  width <- interval_width(e1)
  new_time_interval(time_add(start, timespan(e2)), width)
}
#' @export
`-.time_interval` <- function(e1, e2){
  start <- interval_start(e1)
  width <- interval_width(e1)
  new_time_interval(time_subtract(start, timespan(e2)), width)
}
#' @export
format.time_interval <- function(x, ...){
  start <- interval_start(x)
  end <- interval_end(x)
  out <- stringr::str_c("[", start, ", ", end, ")")
  names(out) <- names(x)
  out
}
#' @export
as.character.time_interval <- function(x, ...){
  start <- interval_start(x)
  end <- interval_end(x)
  stringr::str_c("[", start, ", ", end, ")")
}
#' @export
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
  # start <- interval_start(out)
  # end <- interval_end(out)
  timespan <- interval_width(x)
  unit <- timespan_unit(timespan)
  num <- timespan_num(timespan)

  # intv <- stringr::str_c("[", start, ", ", end, ")")
  if (timespan_has_unit(timespan)){
    cat(
      "<time_interval> /",
      "Width:",
      paste0(unit, " (", num, ")\n")
    )
  } else {
    cat(
      "<time_interval> /",
      "Width:", num, "\n"
    )
  }
  if (length(start) == 0){
    print(paste0("time_interval(width = ",
                 "timespan(", ifelse(is.na(unit), "NULL", unit), ", ", num, ")", "))"),
          quote = FALSE)
  } else {
    print(format(out), max = max, quote = FALSE, ...)
    # print(intv, quote = FALSE)
  }
  cat(additional_msg)
  invisible(x)
}
