#' Is time a regular sequence? (Experimental)
#'
#' @description
#' This function is a fast way to check if a time vector
#' is a regular sequence, possibly for many groups.
#' Regular in this context means that the lagged time differences are a
#' whole multiple of the specified time unit. \cr
#' This means `x` can be a regular sequence with or without gaps in time.
#'
#' @param x Time vector. \cr
#' E.g. a `Date`, `POSIXt`, `numeric` or any time-based vector.
#' @param timespan [timespan].
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame. \cr
#' Note that when `g` is supplied the output is a logical with length
#' matching the number of unique groups.
#' @param use.g.names Should the result include group names?
#' Default is `TRUE`.
#' @param na.rm Should `NA` values be removed before calculation?
#' Default is `TRUE`.
#' @param allow_gaps Should gaps be allowed? Default is `FALSE`.
#' @param allow_dups Should duplicates be allowed? Default is `FALSE`.
#'
#' @returns
#' A logical vector the same length as the number of supplied groups.
#'
#' @examples
#' library(timeplyr)
#' library(lubridate)
#' library(dplyr)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 1L)
#' collapse::set_collapse(nthreads = 1L)
#' }
#' x <- 1:5
#' y <- c(1, 1, 2, 3, 5)
#'
#' # No duplicates or gaps allowed by default
#' time_is_regular(x)
#' time_is_regular(y)
#'
#' increment <- 1
#'
#' # duplicates and gaps allowed
#' time_is_regular(x, increment, allow_dups = TRUE, allow_gaps = TRUE)
#' time_is_regular(y, increment, allow_dups = TRUE, allow_gaps = TRUE)
#'
#' # No gaps allowed
#' time_is_regular(x, increment, allow_dups = TRUE, allow_gaps = FALSE)
#' time_is_regular(y, increment, allow_dups = TRUE, allow_gaps = FALSE)
#'
#' # Grouped
#' eu_stock <- ts_as_tbl(EuStockMarkets)
#' eu_stock <- eu_stock |>
#'   mutate(date = as_date(
#'     date_decimal(time)
#'   ))
#'
#' time_is_regular(eu_stock$date, g = eu_stock$group, timespan = 1,
#'                 allow_gaps = TRUE)
#' # This makes sense as no trading occurs on weekends and holidays
#' time_is_regular(eu_stock$date, g = eu_stock$group,
#'                 timespan = 1,
#'                 allow_gaps = FALSE)
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
#' @export
time_is_regular <- function(x, timespan = granularity(x),
                            g = NULL, use.g.names = TRUE,
                            na.rm = TRUE,
                            allow_gaps = FALSE,
                            allow_dups = FALSE){
  check_is_time_or_num(x)
  if (length(x) == 0L){
    return(TRUE)
  }
  x <- unname(x)
  g <- GRP2(g, return.groups = use.g.names)
  check_data_GRP_size(x, g)
  if (!is.null(g)){
    n_groups <- GRP_n_groups(g)
  } else {
    n_groups <- min(1L, length(x))
  }
  timespan <- timespan(timespan)
  telapsed <- time_elapsed(x, timespan, g = g,
                           rolling = FALSE,
                           na_skip = na.rm,
                           fill = 0)
  if (na.rm){
    roll_time_diff <- roll_diff(roll_na_fill(telapsed, g = g), fill = 0, g = g)
  } else {
    roll_time_diff <- roll_diff(telapsed, fill = 0, g = g)
  }
  # out <- vapply(collapse::gsplit(telapsed, g = g),
  #               function(x) is_whole_number(x, na.rm = na.rm),
  #               logical(1))
  # double_equal(telapsed, round(telapsed))
  is_whole_num <- are_whole_numbers(telapsed)
  if (na.rm){
    is_whole_num[cheapr::na_find(is_whole_num)] <- TRUE
  }
  n_whole_num <- collapse::fsum(is_whole_num, g = g, use.g.names = FALSE,
                                na.rm = na.rm, fill = FALSE)
  out <- n_whole_num == fn(telapsed, g = g, use.g.names = FALSE)
  is_increasing <- cppdoubles::double_gte(
    collapse::fmin(roll_time_diff, g = g, na.rm = na.rm, use.g.names = FALSE), 0
  )
  out <- out & is_increasing
  if (!allow_gaps){
    has_gaps <- cppdoubles::double_gt(
      collapse::fmax(roll_time_diff, g = g, na.rm = na.rm, use.g.names = FALSE), 1
    )
    out <- out & !has_gaps
  }
  if (!allow_dups){
    gduplicated <- gduplicated(x, g = g)
    if (na.rm){
      gduplicated <- gduplicated & !is.na(x)
    }
    has_dups <- gany(gduplicated, g = g)
    out <- out & !has_dups
  }
  if (use.g.names){
    names(out) <- GRP_names(g)
  }
  out
}
# Ungrouped version.
# time_is_reg <- function(x, time_by = NULL,
#                         na.rm = TRUE,
#                         time_type = getOption("timeplyr.time_type", "auto"),
#                         allow_gaps = TRUE,
#                         allow_dups = TRUE){
#   check_is_time_or_num(x)
#   if (length(x) == 0L){
#     return(TRUE)
#   }
#   time_by <- time_by_get(x, time_by = time_by)
#   telapsed <- time_elapsed(x, time_by = time_by,
#                            time_type = time_type, rolling = FALSE,
#                            na_skip = na.rm,
#                            fill = 0)
#   if (is.null(time_by)){
#     out <- TRUE
#   } else {
#     out <- is_whole_number(telapsed, na.rm = na.rm)
#     # Check that the sequence is increasing/decreasing
#     if (na.rm){
#       is_increasing <- is_sorted(telapsed[!is.na(telapsed)])
#     } else {
#       is_increasing <- !is.unsorted(telapsed)
#     }
#     # is_increasing <- diff_is_increasing(roll_time_diff)
#     out <- out && is_increasing
#   }
#   if (!allow_gaps){
#     # roll_time_diff <- fdiff2(telapsed, fill = 0)
#     # has_gaps <- diff_has_gaps(roll_time_diff)
#     has_gaps <- time_has_gaps(x, time_by = time_by,
#                               na.rm = na.rm, time_type = time_type,
#                               check_time_regular = FALSE)
#     out <- out && !has_gaps
#   }
#   if (!allow_dups){
#     is_dup <- collapse::fduplicated(x)
#     if (na.rm){
#       is_dup <- is_dup & !is.na(x)
#     }
#     has_dups <- any(is_dup)
#     out <- out & !has_dups
#   }
#   out
# }
# check_time_elapsed_regular <- function(x){
#     unique_elapsed <- collapse::funique(x)
#     is_regular <- is_whole_number(unique_elapsed, na.rm = TRUE)
#     if (!is_regular){
#       stop("x is not regular given the chosen time unit")
#     }
# }
# check_time_elapsed_order <- function(x){
#   if (isTRUE(collapse::fmin(x, na.rm = TRUE) < 0)){
#     stop("x must be in ascending or descending order")
#   }
# }
