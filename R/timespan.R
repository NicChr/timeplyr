
new_timespan <- function(units, num = 1L){
 out <- list("unit" = units, num = num)
 class(out) <- "timespan"
 out
}
is_timespan <- function(x){
  inherits(x, "timespan")
}
check_timespan <- function(x){
  if (!is_timespan(x)){
    cli::cli_abort(c(
      "{.var x} must be a timespan",
      "x" = "You've suplied a {.cls {class(x)}} vector"
    ))
  }
}
timespan_unit <- function(x){
  x[["unit"]]
}
timespan_num <- function(x){
  x[["num"]]
}
timespan_has_unit <- function(x){
  !is.na(timespan_unit(x))
}
#' @export
timespan <- function(units, num = 1L, ...){
  UseMethod("timespan")
}
#' @export
timespan.NULL <- function(units, num = 1L, ...){
  new_timespan("unit" = NA_character_, "num" = num)
}
#' @export
timespan.logical <- function(units, num = 1L, ...){
  timespan(as.integer(units), num, ...)
}
#' @export
timespan.numeric <- function(units, num = 1L, ...){
  new_timespan("unit" = NA_character_, "num" = units * num)
}
#' @export
timespan.Duration <- function(units, num = 1L, ...){
  time_unit_info <- time_unit_info(units)
  unit <- paste0(names(time_unit_info), "s")
  num <- time_unit_info[[1L]] * num
  new_timespan(unit, num)
}
#' @export
timespan.Period <- function(units, num = 1L, ...){
  time_unit_info <- time_unit_info(units)
  if (length(time_unit_info) > 1L){
    stop("Multiple period units are currently not supported.")
  }
  unit <- paste0(names(time_unit_info), "s")
  num <- time_unit_info[[1L]] * num
  new_timespan(unit, num)
}

#' @export
timespan.character <- function(units, num = 1L, ...){

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
  units[["num"]] <- units[["num"]] * num
  units
}

#' @export
print.timespan <- function(x, ...){
  unit <- timespan_unit(x)
  num <- timespan_num(x)
  if (is.null(unit) || is.na(unit) || unit == "numeric" || !nzchar(unit)){
    unit <- ""
  }
  # cat(paste0("<", "Timespan:", unit, ">\n", num))
  # cat(paste0("<", "Timespan:", unit, ">"), num, sep = "\n")
  # cat(paste0("<", "Timespan:", unit, ">"), num, sep = " ")
  cat(paste0("<", "Timespan:", unit, ">\n"))
  print(num)
  invisible(x)
}
#' @export
length.timespan <- function(x){
  length(.subset2(x, "num"))
}


period_timespan <- function(years = 0L,
                            months = 0L,
                            weeks = 0L,
                            days = 0L,
                            hours = 0L,
                            minutes = 0L,
                            seconds = 0L){

  ### Find which args are missing

  units <- rev(.period_units)
  is_missing <- logical(length(units))
  names(is_missing) <- units
  is_missing[1] <- missing(years)
  is_missing[2] <- missing(months)
  is_missing[3] <- missing(weeks)
  is_missing[4] <- missing(days)
  is_missing[5] <- missing(hours)
  is_missing[6] <- missing(minutes)
  is_missing[7] <- missing(seconds)

  ### Arguments

  args <- list(
    years = years,
    months = months,
    weeks = weeks,
    days = days,
    hours = hours,
    minutes = minutes,
    seconds = seconds
  )

  ### Stop if there are any decimals

  for (i in seq_len(length(args) - 1L)){
    if (!is_whole_number(args[[i]])){
      stop(paste(names(args)[i], "must be a vector of whole numbers"))
    }
  }

  ### Recycled size

  period_lengths <- cheapr::lengths_(args)
  max_length <- max(period_lengths)

  ### Template output

  out <- new_list(length(units), default = integer())
  names(out) <- units

  ### Recycle

  if (all(period_lengths > 0L)){
    out[!is_missing] <- do.call(cheapr::recycle, args[!is_missing])

    ### Fill the vectors not specified by the user with an integer of zeroes
    if (any(is_missing)){
      fill <- integer(max_length)
      out[is_missing] <- new_list(sum(is_missing), default = fill)
    }
  }
  do.call(new_period_timespan, out)
}
new_period_timespan <- function(years = 0L,
                                months = 0L,
                                weeks = 0L,
                                days = 0L,
                                hours = 0L,
                                minutes = 0L,
                                seconds = 0L){
  out <- list(
    years = years,
    months = months,
    weeks = weeks,
    days = days,
    hours = hours,
    minutes = minutes,
    seconds = seconds
  )
  # class(out) <- c("period", "timespan", "vctrs_rcrd", "vctrs_vctr")
  out
}
# print.timespan <- function(x, max = NULL, ...){
#   out <- x
#   N <- length(out)
#   if (is.null(max)){
#     max <- getOption("max.print", 9999L)
#   }
#   max <- min(max, N)
#   if (max < N){
#     i <- seq_len(max)
#     out <- out[i]
#     additional_msg <- paste(" [ reached 'max' / getOption(\"max.print\") -- omitted",
#                             N - max, "entries ]\n")
#   } else {
#     additional_msg <- character()
#   }
#   vctrs::obj_print_header(x)
#   vctrs::obj_print_data(out)
#   cat(additional_msg)
#   invisible(x)
# }
# format.timespan <- function(x, ...){
#   periods <- unclass(x)
#   abbrs <- c("y", "m", "w", "d", "H", "M", "S")
#   time_units <- c("years", "months", "weeks", "days",
#                   "hours", "minutes", "seconds")
#   units <- names(periods)
#   abbrs <- abbrs[match(units, time_units)]
#   ranges <- lapply(
#     periods, function(x) collapse::frange(x, na.rm = TRUE)
#   )
#   keep <- logical(length(periods))
#   fmts <- character(length(periods))
#
#   # We only print periods that have at least 1 non-zero element
#   # We're also deciding whether to use an integer or floating point print format
#
#   # time_period() already checked that all values are whole numbers
#   # But they might be larger than 32-bit integers..
#
#   for (i in seq_along(keep)){
#     keep[i] <- sum(abs(ranges[[i]])) > 0
#     is_whole_num <- is_whole_number(periods[[i]])
#     can_be_int <- isTRUE(is_integerable(max(abs(ranges[[i]]))))
#     if (!is_whole_num){
#       fmts[i] <- "%.3g"
#     } else if (can_be_int){
#       fmts[i] <- "%d"
#     } else {
#       fmts[i] <- "%.0f"
#     }
#   }
#
#   # Keep only elements that have >=1 non-zero values
#   keep <- which(keep)
#   time_list <- periods[keep]
#   fmts <- fmts[keep]
#
#   # sprintf() formats
#   sprint_fmt <- paste(paste0(fmts, abbrs[match(names(time_list), units)]), collapse = " ")
#
#   if (length(time_list) == 0){
#     out <- sprintf("%.0fS", periods[["seconds"]])
#   } else {
#     out <- do.call(sprintf, c(list(sprint_fmt), time_list))
#   }
#   out
# }

# is_timespan <- function(x){
#   inherits(x, "timespan")
# }
