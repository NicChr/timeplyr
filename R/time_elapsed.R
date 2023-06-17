#' Fast grouped time elapsed
#'
#' @description Simple function to calculate how much time has passed
#' on a rolling or fixed basis. \cr
#' It is extremely efficient for even large numbers of groups, especially
#' if your data is sorted by those groups.
#'
#' @param x Date, datetime or numeric vector.
#' @param time_by Time units used to calculate episode flags.
#' If `time_by` is `NULL` then a heuristic will try and estimate the highest
#' order time unit associated with the time variable.
#' If specified, then by must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param g Object to be used for grouping `x`, passed onto `collapse::GRP()`.
#' @param time_type Time type, either "auto", "duration" or "period".
#' With larger data, it is recommended to use `time_type = "duration"` for
#' speed and efficiency.
#' @param rolling If `TRUE` then lagged time differences are calculated,
#' essentially like `diff()`. \cr
#' If `FALSE` (the default) then time differences compared to the index (first) time
#' are calculated.
#' @param fill Value with which to fill the first time elapsed value when
#' `rolling = TRUE`.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' x <- time_seq(today(), length.out = 25, by = "3 days")
#' time_elapsed(x)
#' time_elapsed(x, rolling = TRUE, time_by = "day")
#'
#' # Grouped example
#' set.seed(99)
#' g <- sample(letters[1:3], length(x), TRUE)
#'
#' time_elapsed(x, time_by = "day", g = g)
#' # Equivalently (not as efficiently) using dplyr
#' tibble(x = x, g = g) %>%
#'   group_by(g) %>%
#'   mutate(elapsed = time_elapsed(x, "day")) %>%
#'   pull(elapsed)
#' @export
time_elapsed <- function(x, time_by = NULL, g = NULL,
                         time_type = c("auto", "duration", "period"),
                         rolling = FALSE, fill = 0){
  time_by <- time_by_get(x, by = time_by, is_sorted = FALSE)
  if (is.null(g)){
    gstarts <- min(1L, length(x))
  } else {
    # We need to sort by groups to use lags
    g <- GRP2(g, sort = TRUE, return.groups = TRUE, return.order = TRUE)
    gstarts <- GRP_starts(g)
  }
  if (rolling){
    if (!is.null(g)){
      gsorted <- GRP_is_sorted(g)
      # If not sorted, sort and re-group
      if (!gsorted){
        # We need to sort by groups to use lags
        gorder <- GRP_order(g)
        g <- g[["group.id"]][gorder]
        g <- collapse::GRP(g, return.groups = FALSE, return.order = FALSE)
        gstarts <- GRP_starts(g)
        # Sort x by groups, but keep within-group order
        x <- x[gorder]
      }
    }
    x_lag <- collapse::flag(x, n = min(1L, length(x)), g = g)
    out <- time_diff(x_lag, x, by = time_by, type = time_type)
    if (!is.na(fill)){
      setv(out, gstarts, fill, vind1 = TRUE)
    }
    # Sort back to original order
    if (!is.null(g) && !gsorted){
      out <- out[collapse::radixorderv(gorder)]
    }
  } else {
    # Index time
    x_lag <- gfirst(x, g = g, na.rm = FALSE)
    out <- time_diff(x_lag, x, by = time_by, type = time_type)
    # The first value should probably be 0, even if x[1] is NA
    setv(out, gstarts, 0, vind1 = TRUE)
  }
  out
}
