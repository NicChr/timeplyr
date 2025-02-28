check_is_timespan <- function(x){
  if (!is_timespan(x)){
    cli::cli_abort(c(
      "{.var x} must be a timespan",
      "x" = "You've suplied a {.cls {class(x)}} vector"
    ))
  }
}
check_valid_timespan <- function(x){
  check_is_timespan(x)
  if (timespan_has_unit(x) &&
      !timespan_unit(x) %in% .duration_units){
    cli::cli_abort(
      c("timespan unit must be one of:",
        paste(.duration_units, collapse = ", "),
        "not '{timespan_unit(x)}'")
    )
  }
}
timespan_has_unit <- function(x){
  !is.na(timespan_unit(x))
}

#' Timespans
#'
#' @param units A unit of time, e.g.
#' `"days"`, `"3 weeks"`, `lubridate::weeks(3)`, or just a numeric vector.
#' @param num Number of units. E.g. `units = "days"` and `num = 3` produces
#' a timespan width of 3 days.
#' @param x A [timespan].
#' @param ... Further arguments passed onto methods.
#'
#' @returns
#' A [timespan] object.
#'
#' @details
#' `timespan()` can be used to create objects of class 'timespan' which are
#' used widely in timeplyr.
#'
#' `new_timespan()` is a bare-bones version that
#' does no checking or string parsing and is
#' intended for fast timespan creation.
#'
#' `timespan_unit()` is a helper that extracts the unit of time of the timespan.
#'
#' `timespan_num()` is a helper that extracts the number of units of time.
#'
#' @examples
#' library(timeplyr)
#'
#' timespan("week")
#' timespan("day")
#' timespan("decade")
#'
#' # Multiple units of time
#'
#' timespan("10 weeks")
#' timespan("1.5 hours")
#'
#' # These are all equivalent
#' timespan(NULL, 3);timespan(3);timespan(NA_character_, 3)
#'
#' @export
timespan <- function(units, num = 1L, ...){
  if (inherits(units, "timespan") && length(num) == 1 && num == 1){
    return(units)
  } else {
    UseMethod("timespan")
  }
}
#' @rdname timespan
#' @export
new_timespan <- function(units, num = 1L){
  out <- num
  attr(out, "unit") <- units
  class(out) <- "timespan"
  out
}
#' @export
timespan.NULL <- function(units, num = 1L, ...){
  new_timespan(NA_character_, numeric())
}
#' @export
timespan.logical <- function(units, num = 1L, ...){
  timespan(as.integer(units), num, ...)
}
#' @export
timespan.numeric <- function(units, num = 1L, ...){
  new_timespan(NA_character_, units * num)
}
#' @export
timespan.Duration <- function(units, num = 1L, ...){
  new_timespan("seconds", as.double(unclass(units)) * num)
}
#' @export
timespan.Period <- function(units, num = 1L, ...){
  out <- attributes(unclass(units))
  seconds <- lubridate::second(units)
  out[["second"]] <- seconds
  sum_rng <- lapply(out, function(x) sum(abs(collapse::frange(x, na.rm = TRUE))))
  keep <- vapply(sum_rng, function(x) isTRUE(cppdoubles::double_gt(x, 0)), FALSE)
  if (sum(keep) > 1L){
    cli::cli_abort("Multiple period units are currently not supported")
  }
  if (sum(keep) == 0){
    period <- out["second"]
  } else {
    period <- out[keep]
  }
  unit <- paste0(names(period), "s")
  num <- period[[1L]] * num
  new_timespan(unit, num)
}

#' @export
timespan.character <- function(units, num = 1L, ...){
  if (length(units) != 1){
    cli::cli_abort("{.arg units} must be of length 1")
  }
  if (is.na(units)){
    return(new_timespan(NA_character_, num))
  }
  unit <- unit_match(units)
  # If that doesn't work finally try parsing
  if (is.na(unit)){
    parse_info <- unit_parse(units)
    unit <- parse_info[["unit"]]
    num <- parse_info[["num"]] * num
  } else {
    scale <- 1L
    # If the unit is something exotic,
    # The num needs to be scaled correctly
    if (unit %in% .extra_time_units){
      exotic_info <- convert_exotic_units(unit)
      scale <- exotic_info[["scale"]]
      unit <- exotic_info[["unit"]]
    }
    unit <- unit
    num <- num * scale
  }
  new_timespan(unit, num)
}
#' @export
timespan.timespan <- function(units, num = 1L, ...){
  out <- units * num
  if (!is_timespan(out)){
    out <- new_timespan(timespan_unit(units), out)
  }
  out
}

#' @export
format.timespan <- function(x, short = TRUE, ...){
  timespan_abbr(x, short = short)
}
#' @export
as.character.timespan <- function(x, short = TRUE, ...){
  timespan_abbr(x, short = short)
}

#' @export
print.timespan <- function(x, ...){
  check_valid_timespan(x)
  unit <- timespan_unit(x)
  num <- timespan_num(x)
  cat(paste0("<", "Timespan:", cheapr::na_rm(unit), ">\n"))
  print(num)
  invisible(x)
}
#' @rdname timespan
#' @export
is_timespan <- function(x){
  inherits(x, "timespan")
}
#' @rdname timespan
#' @export
timespan_unit <- function(x){
  attr(x, "unit", TRUE)
}
#' @rdname timespan
#' @export
timespan_num <- function(x){
  out <- unclass(x)
  attr(out, "unit") <- NULL
  out
}

timespan_abbr <- function(x, short = FALSE){

  x <- timespan(x)

  if (short){
    abbrs <- c(
      picoseconds = "ps",
      nanoseconds = "ns",
      microseconds = "\u03Bcs",
      milliseconds = "ms",
      seconds = "s",
      minutes = "m",
      hours = "h",
      days  = "D",
      weeks = "W",
      months = "M",
      years = "Y"
    )
    sep <- ""
  } else {
    abbrs <- c(
      picoseconds = "picosecs",
      nanoseconds = "nanosecs",
      microseconds = "microsecs",
      milliseconds = "millisecs",
      seconds = "secs",
      minutes = "mins",
      hours = "hours",
      days  = "days",
      weeks = "weeks",
      months = "months",
      years = "years"
    )
    sep <- " "
  }

  units <- names(abbrs)

  unit <- timespan_unit(x)
  num <- timespan_num(x)
  if (timespan_has_unit(x)){
    abbr <- unname(abbrs)[match(unit, units)]
    if (!short && num == 1){
      num <- ""
      abbr <- plural_unit_to_single(abbr)
      sep <- ""
    }
  } else {
    abbr <- ""
    sep <- ""
  }

  paste(num, abbr, sep = sep)

}

#' @export
`[.timespan` <- function(x, ...){
  new_timespan(timespan_unit(x), NextMethod("["))
}
#' @export
c.timespan <- function(..., recursive = FALSE, use.names = TRUE){
  dots <- list(...)
  if (!use.names){
    dots <- unname(dots)
  }
  span <- dots[[1L]]
  span_unit <- timespan_unit(span)
  span_num <- timespan_num(span)
  for (i in seq_along(dots)){
    dot <- dots[[i]]
    if (!is_timespan(dot)){
      cli::cli_abort("Cannot combine {.cls timespan} with {.cls {class(dot)}}")
    }
    if (!identical(span_unit, timespan_unit(dot))){
      cli::cli_abort(
        "Cannot combine {.cls timespan} of unit '{timespan_unit(dot)}'
        with {.cls timespan} of unit '{span_unit}'"
      )
    }
    dots[[i]] <- timespan_num(dot)
  }
  out <- do.call(c, dots, envir = parent.frame())
  new_timespan(span_unit, out)
}
#' @export
unique.timespan <- function(x, incomparables = FALSE, ...){
  new_timespan(
    timespan_unit(x),
    unique(timespan_num(x), incomparables = incomparables, ...)
  )
}
#' @export
rep.timespan <- function(x, ...){
  new_timespan(timespan_unit(x), NextMethod("rep"))
}
#' @exportS3Method base::rep_len
rep_len.timespan <- function(x, length.out){
  new_timespan(timespan_unit(x), NextMethod("rep_len"))
}

#' @exportS3Method vctrs::vec_proxy
vec_proxy.timespan <- function(x, ...){
  unclass(x)
}
#' @exportS3Method vctrs::vec_restore
vec_restore.timespan <- function(x, to, ...){
  new_timespan(timespan_unit(to), x)
}
#' @exportS3Method pillar::pillar_shaft
pillar_shaft.timespan <- function(x, ...) {
  out <- timespan_num(x)
  pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.timespan <- function(x, ...){
  paste0("Timespan:", cheapr::na_rm(timespan_unit(x)))
}

#' @export
Ops.timespan <- function(e1, e2){
  out <- NextMethod(.Generic)
  switch(
    .Generic,
    `+` =,
    `*` =,
    `/` =,
    `^` =,
    `%%` =,
    `%/%` = {
      attributes(out) <- attributes(if (inherits(e1, "timespan")) e1 else e2)
      out
    },
    out
  )
}
