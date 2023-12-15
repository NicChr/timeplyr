#' S3-based Time Intervals (Currently very experimental and so subject to change)
#'
#' @description
#' Inspired by both 'lubridate' and 'ivs', `time_interval` is an S3 class for
#' right-open intervals that contain a vector of start dates and end dates.
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
#' @examples
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
#' format(month_int, interval_sub_formatter = function(x) format(x, "%Y/%B"))
#' format(month_int, interval_style = "start", interval_sub_formatter = function(x) format(x, "%Y/%B"))
#'
#' # Advanced formatting
#'
#' # As shown above, we can specify formatting functions for the dates
#' # in our intervals
#' # Sometimes it's useful to set a default function
#'
#' options(timeplyr.interval_sub_formatter = function(x) format(x, "%b %Y"))
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
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @rdname time_interval
#' @export
time_interval <- function(start = integer(), end = integer()){
  interval <- time_interval_list(start, end)
  out <- interval[["start"]]
  end <- interval[["end"]]
  structure(out,
            end = end,
            class = c("time_interval", oldClass(out)))
}
#' @rdname time_interval
#' @export
is_time_interval <- function(x){
  inherits(x, "time_interval")
}
time_interval_list <- function(start, end){
  set_time_cast(start, end)
  if (typeof(start) == "double" && (
    collapse::anyv(unclass(start), Inf) ||
    collapse::anyv(unclass(start), -Inf)
  )){
    stop("start must be finite")
  }
  # if (!identical(class(start), class(end))){
  #   stop(paste0("Incompatible classes\n start: <",
  #               paste(class(start), collapse = " "), ">",
  #               "\n end: <", paste(class(end), collapse = " "), ">"))
  # }
  recycle_args(start = start, end = end)
}
#' @rdname time_interval
#' @export
time_interval_start <- function(x){
  UseMethod("time_interval_start")
}
#' @export
time_interval_start.time_interval <- function(x){
  out <- x
  class(out) <- setdiff2(oldClass(out), "time_interval")
  attr(out, "end") <- NULL
  out
}
#' @export
time_interval_start.Interval <- function(x){
  attr(x, "start", TRUE)
}
#' @rdname time_interval
#' @export
time_interval_end <- function(x){
  UseMethod("time_interval_end")
}
#' @export
time_interval_end.time_interval <- function(x){
  attr(x, "end", TRUE)
}
#' @export
time_interval_end.Interval <- function(x){
  time_interval_start(x) + strip_attrs(unclass(x))
}
as.data.frame.time_interval <- function(x, ...){
  new_df(start = time_interval_start(x),
         end = time_interval_end(x))
}
as.list.time_interval <- function(x, ...){
  list(start = time_interval_start(x),
       end = time_interval_end(x))
}
`+.time_interval` <- function(e1, e2){
  start <- time_interval_start(e1)
  end <- time_interval_end(e1)
  start <- time_add2(start, e2)
  end <- time_add2(end, e2)
  time_interval(start, end)
}
`-.time_interval` <- function(e1, e2){
  time_by <- time_by_list(e2)
  time_by <- add_names(
    list(-time_by_num(time_by)
    ),
    time_by_unit(time_by)
  )
  start <- time_interval_start(e1)
  end <- time_interval_end(e1)
  start <- time_add2(start, time_by)
  end <- time_add2(end, time_by)
  time_interval(start, end)
}
`/.time_interval` <- function(e1, e2){
  interval <- as.list(e1)
  time_diff(interval[[1L]], interval[[2L]], time_by = e2)
}
unique.time_interval <- function(x, ...){
  interval <- collapse::funique(as.data.frame(x))
  time_interval(interval[[1L]], interval[[2L]])
}
duplicated.time_interval <- function(x, ...){
  collapse::fduplicated(as.list(x))
}
sort.time_interval <- function(x, ...){
  interval <- as.data.frame(x)
  x[radixorderv2(interval, ...)]
}
c.time_interval <- function(...){
  dots <- list(...)
  end <- vector("list", length(dots))
  end_attrs <- attributes(attr(dots[[1L]], "end", TRUE))
  end_class <- oldClass(attr(dots[[1L]], "end", TRUE))
  for (i in seq_along(dots)){
    end[[i]] <- attr(.subset2(dots, i), "end", TRUE)
  }
  start <- unlist(dots, recursive = FALSE, use.names = FALSE)
  end <- unlist(end, recursive = FALSE, use.names = FALSE)
  attributes(start) <- end_attrs
  attributes(end) <- end_attrs
  time_interval(start, end)
  # structure(start, end = end, class = c("time_interval", end_class))
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
  start <- time_interval_start(x)
  end <- time_interval_end(x)
  # This is important for when slicing a data frame with time_interval objects
  if (length(end) > length(x)){
    end <- end[seq_along(x)]
  }
  if (!is.null(interval_sub_formatter)){
    start <- do.call(interval_sub_formatter, list(start))
    end <- do.call(interval_sub_formatter, list(end))
  } else {
    start <- as.character(start)
    end <- as.character(end)
  }
  if (int_fmt == "full"){
    out <- paste0("[", start, "--", end, ")")
    which_closed <- cpp_which(start == end)
    out[which_closed] <- paste0("[", start[which_closed], "--", end[which_closed], "]")
    # which_left_open <- cpp_which(start > end)
    # out[which_left_open] <- paste0("(", start[which_left_open], "--", end[which_left_open], "]")
    out
  } else if (int_fmt == "start"){
    start
  } else {
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
print.time_interval <- function(x, max = NULL, ...){
  out <- x
  end <- time_interval_end(out)
  N <- length(out)
  if (is.null(max)){
    max <- getOption("max.print", 9999L)
  }
  max <- min(max, N)
  if (max < N){
    i <- seq_len(max)
    out <- out[i]
    end <- end[i]
    additional_msg <- paste(" [ reached 'max' / getOption(\"max.print\") -- omitted",
                            N - max, "entries ]\n")
  } else {
    additional_msg <- character()
  }
  print(as.character(out), max = max + 1, ...)
  cat(additional_msg)
  invisible(x)
}
`[.time_interval` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  end <- `[`(attr(x, "end", TRUE), ..., drop = drop)
  class(x) <- NULL
  val <- NextMethod("[")
  attr(val, "end") <- end
  class(val) <- cl
  val
}
#' @export
`[<-.time_interval` <- function(x, i, value){
  start <- time_interval_start(x)
  end <- time_interval_end(x)
  replace <- value
  if (is_time_interval(value)){
    start[i] <- time_interval_start(value)
    end[i] <- time_interval_end(value)
  } else {
    start[i] <- value
    end[i] <- value
  }
  out <- time_interval(start, end)
  out
}
`[[.time_interval` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  end <- `[[`(attr(x, "end", TRUE), ..., drop = drop)
  class(x) <- NULL
  val <- NextMethod("[[")
  attr(val, "end") <- end
  class(val) <- cl
  val
}
type_sum.time_interval <- function(x) {
  "time_intrv"
}
rep_len.time_interval <- function(x, length.out){
  x[rep_len(seq_along(x), length.out = length.out)]
}
rep.int.time_interval <- function(x, ...){
  x[rep.int(seq_along(x), ...)]
}
rep.time_interval <- function(x, ...){
  x[rep(seq_along(x), ...)]
}
as.Date.time_interval <- function(x, ...){
  class(x) <- setdiff2(oldClass(x), "time_interval")
  as.Date(x, ...)
}
as.POSIXct.time_interval <- function(x, ...){
  class(x) <- setdiff2(oldClass(x), "time_interval")
  as.POSIXct(x, ...)
}
as.POSIXlt.time_interval <- function(x, ...){
  class(x) <- setdiff2(oldClass(x), "time_interval")
  as.POSIXlt(x, ...)
}
is.na.time_interval <- function(x){
  is.na(unclass(x)) & is.na(time_interval_end(x))
}
# new_pillar_shaft.time_interval <- function(x, ...) {
#   x <- format(x)
#   pillar::new_pillar_shaft_simple(x, width = 10, align = "left")
# }
pillar_shaft.time_interval <- function(x, ...) {
  out <- format(x)
  # out[cpp_which(is.na(x[["start"]]) | is.na(x[["end"]]))] <- time_interval(NA, NA)
  pillar::new_pillar_shaft_simple(out, align = "right")
}

