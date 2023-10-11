#' Guess time unit and extract basic information.
#'
#' @description This is a simple R function to convert time units to a
#' common unit, with number and scale. \cr
#' See `.time_units` for a list of accepted
#' time units.
#'
#' @param x This can be 1 of 4 options:
#' * A string, e.g. "7 days"
#' * Lubridate duration or period object, e.g. `days(1)` or `ddays(1)`.
#' * A list, e.g. list("days" = 7)
#' * A number, e.g. 5
#'
#' @returns
#' A list of length 3, including the unit, number and scale.
#'
#' @examples
#' library(timeplyr)
#' \dontshow{
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' # Single units
#' unit_guess("days")
#' unit_guess("hours")
#'
#' # Multi-units
#' unit_guess("7 days")
#' unit_guess("0.5 hours")
#'
#' # Negative units
#' unit_guess("-7 days")
#' unit_guess("-.12 days")
#'
#' # Exotic units
#' unit_guess("fortnights")
#' unit_guess("decades")
#' .extra_time_units
#'
#' # list input is accepted
#' unit_guess(list("months" = 12))
#' # With a list, a vector of numbers is accepted
#' unit_guess(list("months" = 1:10))
#' unit_guess(list("days" = -10:10 %% 7))
#'
#' # Numbers also accepted
#' unit_guess(100)
#' @export
unit_guess <- function(x){
  if (inherits(x, c("Duration", "Period"))){
    time_unit_info <- time_unit_info(x)
    if (length(time_unit_info) > 1L){
      stop("Multiple period units are currently not supported.")
    }
    unit <- paste0(names(time_unit_info), "s")
    num <- .subset2(time_unit_info, 1L)
    out <- list("unit" = unit,
                "num" = num,
                "scale" = 1L)
    # If numeric then just return this..
  } else if (is.numeric(x)){
    out <- list("unit" = "numeric",
                "num" = x,
                "scale" = 1L)
  } else if (is.list(x)){
    # If it's a list, string match but no parse
    out <- unit_list_match(x)
  } else {
    # Try matching first as it's faster
    unit <- unit_match(x)
    # If that doesn't work finally try parsing
    if (is.na(unit)){
      out <- unit_parse(x)
    } else {
      num <- 1
      scale <- 1L
      # If the unit is something exotic,
      # The num needs to be scaled correctly
      if (unit %in% .extra_time_units){
        exotic_info <- convert_exotic_units(unit)
        scale <- .subset2(exotic_info, "scale")
        unit <- .subset2(exotic_info, "unit")
      }
      # num <- num * scale
      out <- list("unit" = unit,
                  "num" = num,
                  "scale" = scale)
    }
  }
  out
}
