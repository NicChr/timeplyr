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
                         rolling = TRUE, fill = NA,
                         na_skip = TRUE){
  time_by <- time_by_get(x, time_by = time_by, is_sorted = FALSE)
  if (time_by_length(time_by) > 1){
    stop("Please supply only one numeric value in time_by")
  }
  if (!is.na(fill) && length(fill) > 1){
    stop("fill must be a single number")
  }
  has_groups <- !is.null(g)
  if (!has_groups){
    group_starts <- min(1L, length(x))
  } else {
    # We need to sort by groups to use lags
    g <- GRP2(g, sort = TRUE, return.groups = TRUE, return.order = TRUE)
    group_starts <- GRP_starts(g)
  }
  if (rolling){
    if (has_groups){
      groups_are_sorted <- GRP_is_sorted(g)
      # If not sorted, sort and re-group
      if (!groups_are_sorted){
        # We need to sort by groups to use lags
        group_order <- GRP_order(g)
        g2 <- GRP_group_id(g)[group_order]
        g2 <- collapse::GRP(g2)
        group_starts <- GRP_starts(g2)
        # Sort x by groups, but keep within-group order
        x <- x[group_order]
      } else {
        g2 <- g
      }
    } else {
      g2 <- NULL
      groups_are_sorted <- TRUE
    }
    if (na_skip){
      x_filled <- roll_na_fill(x, g = g2)
      x_lag <- flag2(x_filled, g = g2)
      # Because we're skipping NAs, the group
      # starts are shifted by the number
      # of initial NA values of each group
      # This is only important for the fill arg
      # group_starts <- group_starts +
      #   fnmiss(x_filled, g = g2, use.g.names = FALSE, na.rm = FALSE)
      # group_starts <- pmin(group_starts, fn(x, g = g2))
      group_starts <- group_starts +
        fnmiss(x_lag, g = g2, use.g.names = FALSE, na.rm = FALSE) - 1L
    } else {
      x_lag <- flag2(x, g = g2)
    }
    out <- time_diff(x_lag, x, time_by = time_by, time_type = time_type)
    if (!is.na(fill)){
      out[group_starts] <- fill
    }
    # Sort back to original order
    if (!groups_are_sorted){
      out <- collapse::greorder(out, g = g)
    }
  } else {
    # Index time
    first_time <- gfirst(x, g = g, na.rm = na_skip)
    out <- time_diff(first_time, x, time_by = time_by, time_type = time_type)
    # Alternate type of fill..
    # if (!is.na(fill)){
    #   if (na_skip){
    #     group_starts <- group_starts +
    #       fsum(collapse::fcumsum(!is.na(x), g = g) == 0, g = g)
    #   }
    #   out[group_starts] <- 0
    #   out <- out + fill
    # }
  }
  out
}
# time_elapsed <- function(x, time_by = NULL, g = NULL,
#                          time_type = c("auto", "duration", "period"),
#                          rolling = TRUE, fill = 0,
#                          na_skip = TRUE){
#   time_by <- time_by_get(x, time_by = time_by, is_sorted = FALSE)
#   if (time_by_length(time_by) > 1){
#     stop("Please supply only one numeric value in time_by")
#   }
#   if (is.null(g)){
#     gstarts <- min(1L, length(x))
#   } else {
#     # We need to sort by groups to use lags
#     g <- GRP2(g, sort = TRUE, return.groups = TRUE, return.order = TRUE)
#     gstarts <- GRP_starts(g)
#   }
#
#   # Calculation
#   if (rolling){
#     is_na <- is.na(x)
#     if (any(is_na)){
#       is_not_na <- !is_na
#       out <- rep_len(NA_real_, length(x))
#       x_not_na <- x[is_not_na]
#       g2 <- GRP_group_id(g)[is_not_na]
#       x_lag <- gfirst(x_not_na, g = g2, na.rm = FALSE)
#       out[is_not_na] <- time_diff(x_lag, x_not_na, time_by = time_by,
#                                   time_type = time_type)
#       out[is_not_na] <- fdiff2(out[is_not_na], g = g2)
#       # Replace values that should have NA with NA
#       if (!na_skip){
#         out[is.na(flag2(out, g = g))] <- NA
#       }
#     } else {
#       x_lag <- gfirst(x, g = g, na.rm = FALSE)
#       out <- time_diff(x_lag, x, time_by = time_by, time_type = time_type)
#       out <- fdiff2(out, g = g)
#     }
#     if (!is.na(fill)){
#       setv(out, gstarts, fill, vind1 = TRUE)
#     }
#   } else {
#     x_lag <- gfirst(x, g = g, na.rm = na_skip)
#     out <- time_diff(x_lag, x, time_by = time_by, time_type = time_type)
#   }
#   out
# }
# time_elapsed <- function(x, time_by = NULL, g = NULL,
#                          time_type = c("auto", "duration", "period"),
#                          rolling = TRUE, fill = 0,
#                          na_skip = TRUE){
#   time_by <- time_by_get(x, time_by = time_by, is_sorted = FALSE)
#   if (time_by_length(time_by) > 1){
#     stop("Please supply only one numeric value in time_by")
#   }
#   if (is.null(g)){
#     gstarts <- min(1L, length(x))
#   } else {
#     # We need to sort by groups to use lags
#     g <- GRP2(g, sort = TRUE, return.groups = TRUE, return.order = TRUE)
#     gstarts <- GRP_starts(g)
#   }
#
#   # Calculation
#   if (rolling){
#     if (na_skip){
#       is_na <- is.na(x)
#     }
#     if (na_skip && any(is_na)){
#       is_not_na <- !is_na
#       out <- rep_len(NA_real_, length(x))
#       x_not_na <- x[is_not_na]
#       g <- GRP_group_id(g)[is_not_na]
#       x_lag <- gfirst(x_not_na, g = g, na.rm = FALSE)
#       out[is_not_na] <- time_diff(x_lag, x_not_na, time_by = time_by,
#                                   time_type = time_type)
#       out[is_not_na] <- fdiff2(out[is_not_na], g = g)
#     } else {
#       x_lag <- gfirst(x, g = g, na.rm = FALSE)
#       out <- time_diff(x_lag, x, time_by = time_by, time_type = time_type)
#       out <- fdiff2(out, g = g)
#     }
#     if (!is.na(fill)){
#       setv(out, gstarts, fill, vind1 = TRUE)
#     }
#   } else {
#     x_lag <- gfirst(x, g = g, na.rm = na_skip)
#     out <- time_diff(x_lag, x, time_by = time_by, time_type = time_type)
#   }
#   out
# }
# time_elapsed <- function(x, time_by = NULL, g = NULL,
#                           time_type = c("auto", "duration", "period"),
#                           rolling = TRUE, fill = 0,
#                           na_skip = TRUE){
#   time_by <- time_by_get(x, time_by = time_by, is_sorted = FALSE)
#   if (time_by_length(time_by) > 1){
#     stop("Please supply only one numeric value in time_by")
#   }
#   if (is.null(g)){
#     gstarts <- min(1L, length(x))
#   } else {
#     # We need to sort by groups to use lags
#     g <- GRP2(g, sort = TRUE, return.groups = TRUE, return.order = TRUE)
#     gstarts <- GRP_starts(g)
#   }
#   # Index time
#   x_lag <- gfirst(x, g = g, na.rm = na_skip || rolling)
#   out <- time_diff(x_lag, x, time_by = time_by, time_type = time_type)
#   if (rolling){
#     out <- fdiff2(out, g = g)
#     if (!is.na(fill)){
#       setv(out, gstarts, fill, vind1 = TRUE)
#     }
#   }
#   out
# }
# Old inefficient method.
# time_elapsed <- function(x, time_by = NULL, g = NULL,
#                          time_type = c("auto", "duration", "period"),
#                          rolling = TRUE, fill = 0,
#                          na_skip = FALSE){
#   time_by <- time_by_get(x, time_by = time_by, is_sorted = FALSE)
#   if (time_by_length(time_by) > 1){
#     stop("Please supply only one numeric value in time_by")
#   }
#   if (is.null(g)){
#     gstarts <- min(1L, length(x))
#   } else {
#     # We need to sort by groups to use lags
#     g <- GRP2(g, sort = TRUE, return.groups = TRUE, return.order = TRUE)
#     gstarts <- GRP_starts(g)
#   }
#   if (rolling){
#     sort_nas <- na_skip && sum(is.na(x)) > 0
#     if (!is.null(g)){
#       gsorted <- GRP_is_sorted(g)
#       # If not sorted, sort and re-group
#       if (!gsorted){
#         # We need to sort by groups to use lags
#         group_order <- GRP_order(g)
#         g <- GRP_group_id(g)[group_order]
#         g <- collapse::GRP(g, return.groups = TRUE, return.order = FALSE)
#         gstarts <- GRP_starts(g)
#         # Sort x by groups, but keep within-group order
#         x <- x[group_order]
#       }
#       if (sort_nas){
#         na_order <- gorder(is.na(x), g = GRP_group_id(g))
#         na_sorted <- isTRUE(attr(na_order, "sorted"))
#         if (!na_sorted){
#           g <- GRP_group_id(g)[na_order]
#           g <- collapse::GRP(g, return.groups = FALSE, return.order = FALSE)
#           x <- x[na_order]
#         }
#       } else {
#         na_sorted <- TRUE
#       }
#     } else {
#       gsorted <- TRUE
#       if (sort_nas){
#         na_order <- gorder(is.na(x))
#         na_sorted <- isTRUE(attr(na_order, "sorted"))
#         if (!na_sorted){
#           x <- x[na_order]
#         }
#       } else {
#         na_sorted <- TRUE
#       }
#     }
#     x_lag <- flag2(x, n = min(1L, length(x)), g = g)
#     out <- time_diff(x_lag, x, time_by = time_by, time_type = time_type)
#     # Sort back to original order
#     if (!na_sorted){
#       out <- out[gorder(na_order)]
#     }
#     if (!is.na(fill)){
#       setv(out, gstarts, fill, vind1 = TRUE)
#     }
#     if (!gsorted){
#       out <- out[gorder(group_order)]
#     }
#   } else {
#     # Index time
#     x_lag <- gfirst(x, g = g, na.rm = na_skip)
#     out <- time_diff(x_lag, x, time_by = time_by, time_type = time_type)
#   }
#   out
# }
