#' S3-based Time Intervals (Currently very experimental and so subject to change)
#'
#' @description
#' Inspired by both 'lubridate' and 'ivs',
#' `time_interval` objects are lightweight S3 objects of a fixed width.
#' This enables fast and flexible representation of time data
#' such as months, weeks, and more.
#' They are all left closed, right open intervals.
#'
#' @param x A [time_interval].
#' @param start Start time. \cr
#' E.g a `Date`, `POSIXt`, `numeric` and more.
#' @param width Interval width supplied as a [timespan].
#' By default this is the [resolution] of a time vector so for example,
#' a date's resolution is exactly 1 day, therefore
#' `time_interval(Sys.Date())` simply represents today's date
#' as an interval.
#'
#' @details
#' Currently because of limitations with the S3/S4 system,
#' one can't use time intervals directly with lubridate periods.
#' To navigate around this, `timeplyr::timespan()` can be used.
#' e.g. instead of `interval / weeks(3)`, use `interval / timespan(weeks(3))`
#' or even `interval / "3 weeks"`. where `interval` is a `time_interval`.
#'
#' To perform interval algebra it is advised to use the 'ivs' package.
#' To convert a `time_interval` into an `ivs_iv`, use
#' `ivs::iv(interval_start(x), interval_end(x))`.
#'
#' @returns
#' An object of class `time_interval`. \cr
#' `is_time_interval` returns a logical of length 1. \cr
#' `interval_start` returns the start times. \cr
#' `interval_end` returns the end times. \cr
#' `interval_width` returns the width of the interval as a [timespan]. \cr
#' `interval_count` returns a data frame of unique intervals and their counts. \cr
#' `interval_range` returns a the range of the interval. \cr
#' `new_time_interval` is a bare-bones version of `time_interval()` that
#' performs no checks.
#'
#' @seealso [interval_start]
#'
#' @examples
#' library(dplyr)
#' library(timeplyr)
#' library(lubridate)
#' x <- 1:10
#' int <- time_interval(x, 100)
#' int
#'
#' month_start <- floor_date(today(), unit = "months")
#' month_int <- time_interval(month_start, "month")
#' month_int
#'
#' interval_start(month_int)
#' interval_end(month_int)
#'
#' # Divide an interval into different time units
#' time_interval(today(), years(10)) / timespan("year")
#'
#' # Cutting Sepal Length into blocks of width 1
#' int <- time_cut_width(iris$Sepal.Length, 1)
#' interval_count(int)
#'
#' @rdname time_interval
#' @export
time_interval <- function(start = integer(), width = resolution(start)){

  timespan <- timespan(width)

  if (length(timespan) > 1){
    cli::cli_abort("{.arg num} must be a length-1 timespan")
  }

  if (isTRUE(any(timespan_num(timespan) <= 0, na.rm = TRUE))){
    cli::cli_abort("{.arg width} must be a positive-valued timespan")
  }

  # Basically if width is unit-less (e.g. width = 10)
  # Then we use the unit of the time resolution
  if (!timespan_has_unit(timespan)){
    resolution <- resolution(start)
    attr(timespan, "unit") <- timespan_unit(resolution)
  }

  if (inherits(start, "POSIXlt")){
    start <- as.POSIXct(start)
  }

  new_time_interval(start, timespan)
}
#' @rdname time_interval
#' @export
is_time_interval <- function(x){
  inherits(x, "time_interval")
}
check_is_time_interval <- function(x){
  if (!is_time_interval(x)){
    cli::cli_abort("{.arg x} must be a {.cls time_interval}")
  }
}

check_valid_time_interval <- function(x){
  check_is_time_interval(x)
  if (is.null(interval_width(x))){
    cli::cli_abort("{.arg x} must have a timespan")
  }
}

#' @rdname time_interval
#' @export
new_time_interval <- function(start, width){
  cheapr::attrs_add(
    start,
    timespan = width,
    class = "time_interval",
    old_class = oldClass(start)
  )
}
#' @export
`[.time_interval` <- function(x, ...){
  out <- x
  class(out) <- attr(out, "old_class")
  out <- cheapr::sset(out, ...)
  new_time_interval(out, attr(x, "timespan"))
}
#' @export
c.time_interval <- function(..., recursive = FALSE, use.names = TRUE){
  dots <- list(...)
  if (!use.names){
    dots <- unname(dots)
  }
  int <- dots[[1L]]
  span <- interval_width(int)
  span_unit <- timespan_unit(span)
  span_num <- timespan_num(span)
  for (i in seq_along(dots)){
    dot <- dots[[i]]
    if (!is_time_interval(dot)){
      cli::cli_abort("Cannot combine {.cls time_interval} with {.cls {class(dot)}}")
    }
    class(dots[[i]]) <- attr(dot, "old_class")
    if (!identical(span_unit, timespan_unit(attr(dot, "timespan"))) ||
        span_num != timespan_num(attr(dot, "timespan"))){
      cli::cli_abort(c(
        " " = "{.cls time_interval} {i} with width {interval_width(dot)}",
        "must match the width of",
        " " = "{.cls time_interval} { i - 1 } with width {span}"))
    }
  }
  new_time_interval(cheapr::cheapr_c(.args = dots), span)
}
#' @export
unique.time_interval <- function(x, incomparables = FALSE, ...){
  new_time_interval(
    collapse::funique(interval_start(x), ...),
    interval_width(x)
  )
}
#' @export
rep.time_interval <- function(x, ...){
  new_time_interval(
    rep(interval_start(x), ...),
    interval_width(x)
  )
}
#' @exportS3Method base::rep_len
rep_len.time_interval <- function(x, length.out){
  new_time_interval(
    cheapr::cheapr_rep_len(interval_start(x), length.out),
    interval_width(x)
  )
}


#' @export
format.time_interval <- function(x, ...){
  start <- interval_start(x)
  width <- interval_width(x)
  if (timespan_has_unit(width)){
    width_abbr <- timespan_abbr(width, short = TRUE)
    out <- stringr::str_c("[", start, ", +", width_abbr, ")")
  } else {
    end <- interval_end(x)
    out <- stringr::str_c("[", start, ", ", end, ")")
  }
  names(out) <- names(x)
  out
}
#' @export
as.character.time_interval <- function(x, ...){
  start <- interval_start(x)
  width <- interval_width(x)
  if (timespan_has_unit(width)){
    width_abbr <- timespan_abbr(width, short = TRUE)
    out <- stringr::str_c("[", start, ", +", width_abbr, ")")
  } else {
    end <- interval_end(x)
    out <- stringr::str_c("[", start, ", ", end, ")")
  }
  out
}
#' @export
as.Date.time_interval <- function(x, ...){
  out <- as.Date(interval_start(x))
  class(out) <- "Date"
  out
}
#' @export
as.POSIXct.time_interval <- function(x, tz = "", ...){
  out <- as.POSIXct(interval_start(x), tz = tz, ...)
  class(out) <- c("POSIXct", "POSIXt")
  out
}
#' @export
as.POSIXlt.time_interval <- function(x, tz = "", ...){
  out <- as.POSIXlt(interval_start(x), tz = tz, ...)
  class(out) <- c("POSIXlt", "POSIXt")
  out
}
#' @importFrom lubridate tz
#' @export
tz.time_interval <- function(x){
  lubridate::tz(interval_start(x))
}
#' @export
print.time_interval <- function(x, max = NULL, ...){
  check_valid_time_interval(x)
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
  timespan <- interval_width(x)
  unit <- timespan_unit(timespan)
  num <- timespan_num(timespan)

  if (N == 0){
    cli::cli_text(
      "A ", "{.cls time_interval} of length 0
      and width
      {cli::col_red(timespan_abbr(timespan, short = TRUE))}"
    )
  } else {
    cli::cli_text(intv_span_abbr_cli(x))
    print(format(out), max = max, quote = FALSE, ...)
  }
  cat(additional_msg)
  invisible(x)
}

intv_span_abbr <- function(x){
  check_is_time_interval(x)

  width <- interval_width(x)
  unit <- timespan_unit(width)
  num <- timespan_num(width)

  paste0(
    "<time_interval> [width:",
    timespan_abbr(width, short = TRUE), "]"
  )
}
intv_span_abbr_cli <- function(x){
  check_is_time_interval(x)

  width <- interval_width(x)
  unit <- timespan_unit(width)
  num <- timespan_num(width)

  paste0(
    "<time_interval> [width:",
    cli::col_red(timespan_abbr(width, short = TRUE)), "]"
  )
}

#' @exportS3Method pillar::pillar_shaft
pillar_shaft.time_interval <- function(x, ...) {
  out <- as.character(x)
  pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @rdname time_interval
#' @export
interval_start <- function(x){
  UseMethod("interval_start")
}
#' @export
interval_start.time_interval <- function(x){
  cheapr::attrs_add(
    x, timespan = NULL, old_class = NULL, class = attr(x, "old_class")
  )
}
#' @export
interval_start.Interval <- function(x){
  attr(x, "start", TRUE)
}
#' @rdname time_interval
#' @export
interval_end <- function(x){
  UseMethod("interval_end")
}
#' @export
interval_end.time_interval <- function(x){
  time_add(interval_start(x), interval_width(x))

}
#' @export
interval_end.Interval <- function(x){
  interval_start(x) + cheapr::attrs_clear(unclass(x))
}
#' @rdname time_interval
#' @export
interval_width <- function(x){
  UseMethod("interval_width")
}
#' @export
interval_width.time_interval <- function(x){
  attr(x, "timespan")
}
#' @rdname time_interval
#' @export
interval_count <- function(x){
  UseMethod("interval_count")
}
#' @export
interval_count.time_interval <- function(x){
  out <- cheapr::counts(x, sort = TRUE)
  names(out) <- c("interval", "n")
  fastplyr::as_tbl(out)
}
#' @rdname time_interval
#' @export
interval_range <- function(x){
  UseMethod("interval_range")
}
#' @export
interval_range.time_interval <- function(x){
  rng <- collapse::frange(x, na.rm = TRUE)
  c(interval_start(rng[1]), interval_end(rng[2]))
}
