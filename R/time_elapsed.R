#' Fast grouped time elapsed
#'
#' @description Calculate how much time has passed
#' on a rolling or fixed cumulative basis. \cr
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
#' time differences are calculated on a rolling basis,
#' essentially like `diff()`. \cr
#' If `FALSE` then time differences compared to the index (first) time
#' are calculated.
#' @param fill When `rolling = TRUE`, this is the value that fills
#' the first elapsed time. The default is `NA`.
#' @param na_skip Should `NA` values be skipped? Default is `TRUE`.
#'
#' @details
#' `time_elapsed()` is quite efficient when there are many groups,
#' especially if your data is sorted in order of those groups.
#' In the case that `g` is supplied, it is most efficient when your data is
#' sorted by `g` .
#' When `na_skip` is `TRUE` and `rolling` is also `TRUE`, `NA` values are simply
#' skipped and hence the time differences between the current value and the
#' previous non-NA value are calculated. For example,
#' `c(3, 4, 6, NA, NA, 9)` becomes `c(NA, 1, 2, NA, NA, 3)`. \cr
#' When `na_skip` is `TRUE` and `rolling` is `FALSE`, time differences between
#' the current value and the first non-NA value of the series are calculated.
#' For example,
#' `c(NA, NA, 3, 4, 6, NA, 8)` becomes `c(NA, NA, 0, 1, 3, NA, 5)`.
#'
#' `roll_time_diff` is a more bare-bones ungrouped version of
#' `time_elapsed` which operates like base R's `diff` function and allows
#' lagged time differences.
#'
#' @returns A numeric vector the same length as `x`.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' \dontshow{
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
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
                         rolling = TRUE, fill = NA,
                         na_skip = TRUE){
  check_is_time_or_num(x)
  time_by <- time_by_get(x, time_by = time_by)
  check_time_by_length_is_one(time_by)
  if (!is.na(fill) && length(fill) > 1){
    stop("fill must be a single number")
  }
  g <- GRP2(g, sort = TRUE, return.groups = TRUE, return.order = TRUE)
  if (rolling){
    sorted_group_info <- sort_data_by_GRP(x, g = g, sorted_group_starts = TRUE)
    sorted_g <- sorted_group_info[["sorted_GRP"]]
    x <- sorted_group_info[["x"]]
    if (sorted_group_info[["has_groups"]]){
      group_starts <- GRP_starts(sorted_g)
    } else {
      group_starts <- min(length(x), 1L)
    }
    if (na_skip){
      x_filled <- roll_na_fill(x, g = sorted_g)
      x_lag <- flag2(x_filled, g = sorted_g)
      group_starts <- group_starts +
        fnmiss(x_lag, g = sorted_g, use.g.names = FALSE) - 1L
    } else {
      x_lag <- flag2(x, g = sorted_g)
    }
    out <- time_diff(x_lag, x, time_by = time_by, time_type = time_type)
    if (!is.na(fill)){
      out[group_starts] <- fill
    }
    if (!sorted_group_info[["sorted"]]){
      out <- collapse::greorder(out, g = g)
    }
  } else {
    # Index time
    first_time <- gfirst(x, g = g, na.rm = na_skip)
    out <- time_diff(first_time, x, time_by = time_by, time_type = time_type)
  }
  out
}
