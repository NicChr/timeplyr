#' S3-based Time Intervals (Currently very experimental and so subject to change)
#'
#' @description
#' Inspired by both 'lubridate' and 'ivs', `time_interval` is an S3 class for
#' right-open intervals that contain a vector of start dates and end dates.
#'
#' @details
#' In the near-future, all time aggregated variables will utilise these intervals.
#' One can control the appearance of the intervals through the "timeplyr.interval_format" option.
#' For example:
#'
#' `options(timeplyr.interval_format = "full")` - Full interval format.
#' `options(timeplyr.interval_format = "start")` - Start time of the interval.
#' `options(timeplyr.interval_format = "end")` - end time of the interval.
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
#' options(timeplyr.interval_format = "full")
#' int
#' format(int, "start")
#' format(int, "end")
#'
#' time_interval(today(), today() + years(0:10)) / "years"
#' time_interval(today(), today() + dyears(0:10)) / ddays(365.25)
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
# as_time_interval <- function(x, time_by = NULL){
#   if (is.null(time_by)){
#     granularity <- time_granularity2(x)
#   } else {
#     granularity <- time_by_list(time_by)
#   }
#   time_interval(x, time_add2(x, granularity))
# }
time_interval_list <- function(start, end){
  set_time_cast(start, end)
  # if (!identical(class(start), class(end))){
  #   stop(paste0("Incompatible classes\n start: <",
  #               paste(class(start), collapse = " "), ">",
  #               "\n end: <", paste(class(end), collapse = " "), ">"))
  # }
  recycle_args(start = start, end = end)
}

time_interval_start <- function(x){
  class(x) <- setdiff2(oldClass(x), "time_interval")
  attr(x, "end") <- NULL
  x
}
time_interval_end <- function(x){
  attr(x, "end", TRUE)
}
as.data.frame.time_interval <- function(x, ...){
  out <- x
  class(out) <- setdiff2(oldClass(x), "time_interval")
  end <- attr(out, "end")
  attr(out, "end") <- NULL
  new_df(start = out, end = end)
}
as.list.time_interval <- function(x, ...){
  end <- attr(x, "end", TRUE)
  class(x) <- setdiff2(oldClass(x), "time_interval")
  attr(x, "end") <- NULL
  list(start = x, end = end)
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
  time_interval(interval$start, interval$end)
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
  structure(start, end = end, class = c("time_interval", end_class))
}
as.character.time_interval <- function(x,
                                       # interval_sub_fmt = getOption("timeplyr.interval_sub_format", "full"),
                                       interval_format = getOption("timeplyr.interval_format", "full"),
                                       ...){
  start <- NextMethod("as.character", x)
  end <- attr(x, "end")
  # This is important for when slicing a data frame with time_interval objects
  if (length(end) > length(x)){
    end <- end[seq_along(x)]
  }
  # if (!is.null(itnerval_sub_fmt)){
  #   start <- format(start, interval_sub_fmt) # TO DO
  # }
  int_fmt <- rlang::arg_match0(interval_format, c("full", "start", "end"))
  if (int_fmt == "full"){
    paste0("[", start, "--", end, ")")
  } else if (int_fmt == "start"){
    start
  } else {
    end
  }
}
# as.character.time_interval <- function(x, ...){
#   class(x) <- setdiff2(oldClass(x), "time_interval")
#   end <- attr(x, "end")
#   paste0("[", x, "--", end, ")")
# }
format.time_interval <- function(x, interval_format = getOption("timeplyr.interval_format", "full"), ...){
  format(as.character(x, interval_format = interval_format), ...)
}
print.time_interval <- function(x, max = NULL, ...){
  out <- x
  end <- attr(out, "end")
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
  if (!is_time_interval(value)){
    stop("Replacement must be a time interval")
  }
  start <- time_interval_start(x)
  end <- time_interval_end(x)
  start[i] <- time_interval_start(value)
  end[i] <- time_interval_end(value)
  time_interval(start, end)
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
  as.Date(x)
}
as.POSIXct.time_interval <- function(x, ...){
  class(x) <- setdiff2(oldClass(x), "time_interval")
  as.POSIXct(x)
}
as.POSIXlt.time_interval <- function(x, ...){
  class(x) <- setdiff2(oldClass(x), "time_interval")
  as.POSIXlt(x)
}

# new_pillar_shaft.time_interval <- function(x, ...) {
#   x <- format(x)
#   pillar::new_pillar_shaft_simple(x, width = 10, align = "left")
# }
# pillar_shaft.time_interval <- function(x, ...) {
#   out <- format(x)
#   # out[cpp_which(is.na(x[["start"]]) | is.na(x[["end"]]))] <- time_interval(NA, NA)
#   new_pillar_shaft(out, align = "right")
# }
# time_interval <- function(start, end){
#   out <- time_interval_list(start, end)
#   attributes(out) <- list(class = c("time_interval", "data.frame"),
#                           row.names = c(NA_integer_, -length(out[[1L]])),
#                           names = c("start", "end"))
#   out
# }
#
# `+.time_interval` <- function(e1, e2){
#   e1[["start"]] <- time_add2(e1[["start"]], e2)
#   e1[["end"]] <- time_add2(e1[["end"]], e2)
#   e1
# }
#
# `-.time_interval` <- function(e1, e2){
#   time_by <- time_by_list(e2)
#   time_by <- add_names(
#     list(-time_by_num(time_by)
#     ),
#     time_by_unit(time_by)
#   )
#   e1[["start"]] <- time_add2(e1[["start"]], time_by)
#   e1[["end"]] <- time_add2(e1[["end"]], e2)
#   e1
# }
#
# `/.time_interval` <- function(e1, e2){
#   time_diff(e1[["start"]], e1[["end"]], time_by = e2)
# }
#
# as.character.time_interval <- function(x, ...){
#   start <- x[["start"]]
#   end <- x[["end"]]
#   paste0("[", start, "--", end, ")")
# }
#
# format.time_interval <- function(x, ...){
#   format(as.character(x), ...)
# }
#
# print.time_interval <- function(x, max = NULL, ...){
#   N <- df_nrow(x)
#   out <- list_to_data_frame(unclass(x))
#   if (is.null(max)){
#     max <- getOption("max.print", 9999L)
#   }
#   max <- min(max, N)
#   if (max < N){
#     out <- fslice(out, seq_len(max))
#     additional_msg <- paste(" [ reached 'max' / getOption(\"max.print\") -- omitted",
#                             N - max, "entries ]\n")
#   } else {
#     additional_msg <- character()
#   }
#   out <- time_interval(out[[1L]], out[[2L]])
#   print(as.character(out), max = max + 1, ...)
#   cat(additional_msg)
#   invisible(x)
# }
#
# `[.time_interval` <- function(x, ..., drop = TRUE){
#   df_row_slice(x, ...)
# }
#
# `[<-.time_interval` <- function(x, i, value){
#   x[["start"]][i] <- value[["start"]]
#   x[["end"]][i] <- value[["end"]]
#   x
# }
#
# c.time_interval <- function(...){
#   vctrs::vec_rbind(...)
# }
#
# length.time_interval <- function(x, i, value){
#   df_nrow(x)
# }
