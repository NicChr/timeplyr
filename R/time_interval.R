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
#' int %>%
#'   as_tbl() |>
#'   count(value)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#' }
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

  # Basically if width is unitless (e.g. width = 10)
  # Then we use the unit of the time resolution
  if (!timespan_has_unit(timespan)){
    resolution <- resolution(start)
    timespan[["unit"]] <- timespan_unit(resolution)
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
# Like time_interval() but no checks or recycling
new_time_interval <- function(start, timespan){
  out <- start
  attr(out, "timespan") <- timespan
  class(out) <- c("time_interval", oldClass(start))
  # class(out) <- c(paste0(timespan_abbr(timespan), "_intv"),
  #                 "time_interval", oldClass(start))
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
c.time_interval <- function(...){
  dots <- list(...)
  int <- dots[[1L]]
  cl <- oldClass(int)
  span <- interval_width(int)
  for (i in seq_along(dots)){
    dot <- dots[[i]]
    if (!is_time_interval(dot)){
      cli::cli_abort("Cannot combine {.cls time_interval} with {.cls {class(dot)}}")
    }
    dots[[i]] <- rm_intv_class(dot)
    if (!identical(span, interval_width(dot))){
     cli::cli_abort(c(
     " " = "{.cls time_interval} {i} with width {interval_width(dot)}",
     "must match the width of",
     " " = "{.cls time_interval} { i - 1 } with width {span}"))
    }
  }
  out <- do.call(c, dots, envir = parent.frame())
  class(out) <- cl
  attr(out, "timespan") <- span
  out
}
#' @rdname time_interval
#' @export
unique.time_interval <- function(x, incomparables = FALSE, ...){
  new_time_interval(
    unique(rm_intv_class(x), incomparables = incomparables, ...),
    interval_width(x)
  )
}
#' @rdname time_interval
#' @export
rep.time_interval <- function(x, ...){
  new_time_interval(
    rep(rm_intv_class(x), ...),
    interval_width(x)
  )
}
#' @rdname time_interval
#' @export
rep_len.time_interval <- function(x, ...){
  new_time_interval(
    rep_len(rm_intv_class(x), ...),
    interval_width(x)
  )
}

#' @export
Ops.time_interval <- function(e1, e2){
  switch(.Generic,
         `+` = {
           start <- interval_start(e1)
           width <- interval_width(e1)
           span <- timespan(e2)
           new_time_interval(time_add(start, span), width)
         },
         `-` = {
           start <- interval_start(e1)
           width <- interval_width(e1)
           span <- timespan(e2)
           new_time_interval(time_subtract(start, span), width)
         },
         `/` = {
           start <- interval_start(e1)
           span <- timespan(e2)
           end <- interval_end(e1)
           time_diff(start, end, time_by = span)
           }, NextMethod(.Generic))
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
  timespan <- interval_width(x)
  unit <- timespan_unit(timespan)
  num <- timespan_num(timespan)

  cat(intv_span_abbr(x), sep = "\n")
  if (N == 0){
    unit <- cheapr::na_replace(unit, "NULL")
    if (timespan_has_unit(timespan)){
      unit <- paste0("'", unit, "'")
    }
    print(paste0("time_interval(width = ",
                 "timespan(", unit, ", ", num, ")", "))"),
          quote = FALSE)
  } else {
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

  # paste(
  #   "<time_interval> /",
  #   "Width:", timespan_abbr(width)
  # )
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.time_interval <- function(x, ...) {
  out <- as.character(x)
  pillar::new_pillar_shaft_simple(out, align = "left")
}
