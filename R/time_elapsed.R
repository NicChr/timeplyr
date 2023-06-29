#' Fast grouped time elapsed
#'
#' @description Simple function to calculate how much time has passed
#' on a rolling or fixed basis. \cr
#' It is extremely efficient for even large numbers of groups, especially
#' if your data is sorted by those groups.
#'
#' @param x Date, datetime or numeric vector.
#' @param time_by Must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param g Object to be used for grouping `x`, passed onto `collapse::GRP()`.
#' @param time_type Time type, either "auto", "duration" or "period".
#' With larger data, it is recommended to use `time_type = "duration"` for
#' speed and efficiency.
#' @param rolling If `TRUE` (the default) then lagged
#' time differences are calculated, essentially like `diff()`. \cr
#' If `FALSE` then time differences compared to the index (first) time
#' are calculated.
#' @param fill Value with which to fill the first time elapsed value when
#' `rolling = TRUE`.
#' @param na_skip Should `NA` values be skipped? Default is `TRUE`.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' x <- time_seq(today(), length.out = 25, time_by = "3 days")
#' time_elapsed(x)
#' time_elapsed(x, rolling = FALSE, time_by = "day")
#'
#' # Grouped example
#' set.seed(99)
#' # ~ 100k groups, 1m rows
#' x <- sample(time_seq_v2(20, today(), "day"), 10^6, TRUE)
#' g <- sample.int(10^5, 10^6, TRUE)
#'
#' time_elapsed(x, time_by = "day", g = g)
#' # Equivalently (not as efficiently) using dplyr
#' \dontrun{
#' tibble(x = x, g = g) %>%
#'   group_by(g) %>%
#'   mutate(elapsed = time_elapsed(x, "day")) %>%
#'   pull(elapsed)
#' }
#' @export
time_elapsed <- function(x, time_by = NULL, g = NULL,
                         time_type = c("auto", "duration", "period"),
                         rolling = TRUE, fill = 0,
                         na_skip = TRUE){
  time_by <- time_by_get(x, time_by = time_by, is_sorted = FALSE)
  if (time_by_length(time_by) > 1){
    stop("Please supply only one numeric value in time_by")
  }
  if (is.null(g)){
    gstarts <- min(1L, length(x))
  } else {
    # We need to sort by groups to use lags
    g <- GRP2(g, sort = TRUE, return.groups = TRUE, return.order = TRUE)
    gstarts <- GRP_starts(g)
  }
  if (rolling){
    sort_nas <- na_skip && sum(is.na(x)) > 0
    if (!is.null(g)){
      gsorted <- GRP_is_sorted(g)
      # If not sorted, sort and re-group
      if (!gsorted){
        # We need to sort by groups to use lags
        group_order <- GRP_order(g)
        g <- GRP_group_id(g)[group_order]
        g <- collapse::GRP(g, return.groups = TRUE, return.order = FALSE)
        gstarts <- GRP_starts(g)
        # Sort x by groups, but keep within-group order
        x <- x[group_order]
      }
      if (sort_nas){
        na_order <- gorder(is.na(x), g = GRP_group_id(g))
        na_sorted <- isTRUE(attr(na_order, "sorted"))
        if (!na_sorted){
          g <- GRP_group_id(g)[na_order]
          g <- collapse::GRP(g, return.groups = FALSE, return.order = FALSE)
          x <- x[na_order]
        }
      } else {
        na_sorted <- TRUE
      }
    } else {
      gsorted <- TRUE
      if (sort_nas){
        na_order <- gorder(is.na(x))
        na_sorted <- isTRUE(attr(na_order, "sorted"))
        if (!na_sorted){
          x <- x[na_order]
        }
      } else {
        na_sorted <- TRUE
      }
    }
    x_lag <- flag2(x, n = min(1L, length(x)), g = g)
    out <- time_diff(x_lag, x, time_by = time_by, time_type = time_type)
    # Sort back to original order
    if (!na_sorted){
      out <- out[gorder(na_order)]
    }
    if (!is.na(fill)){
      setv(out, gstarts, fill, vind1 = TRUE)
    }
    if (!gsorted){
      out <- out[gorder(group_order)]
    }
  } else {
    # Index time
    x_lag <- gfirst(x, g = g, na.rm = na_skip)
    out <- time_diff(x_lag, x, time_by = time_by, time_type = time_type)
  }
  out
}
