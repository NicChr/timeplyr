#' Check for missing dates or datetimes
#'
#' @description `time_gaps()` checks for missing gaps in time for any
#' regular date or datetime sequence.
#'
#' @param x A date, datetime or numeric vector.
#' @param time_by Time unit. \cr
#' Must be one of the three:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param time_type Time type, either "auto", "duration" or "period".
#' With larger data, it is recommended to use `time_type = "duration"` for
#' speed and efficiency.
#' @param check_regular Should a check be done to see whether x is a regular
#' sequence given a specified `time_by` unit?
#' Default is `TRUE`.
#' The check only occurs when `time_by` is supplied.
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
#'
#' @details If you know your data is regular and you know the time unit,
#' it is more efficient to set `check_regular = FALSE` and supply the time unit
#' in `time_by`.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' library(nycflights13)
#'
#' missing_dates(flights$time_hour)
#' time_has_gaps(flights$time_hour)
#' time_num_gaps(flights$time_hour)
#' time_gaps(flights$time_hour)
#'
#' # Number of missing hours by origin and dest
#' flights %>%
#'   group_by(origin, dest) %>%
#'   summarise(n_missing = time_num_gaps(time_hour, "hours"))
#' @rdname time_gaps
#' @export
time_gaps <- function(x, time_by = NULL,
                      time_type = c("auto", "duration", "period"),
                      check_regular = TRUE, na.rm = TRUE){
  if (!na.rm && sum(is.na(x)) > 0){
    vctrs::vec_init(x, n = 1L)
  } else {
    time_seq <- time_expandv(x, time_by = time_by,
                             time_type = time_type)
    if (check_regular){
      check_time_regular(x, time_seq, time_by)
    }
    time_seq[!time_seq %in% x]
  }
}

#' @rdname time_gaps
#' @export
time_num_gaps <- function(x, time_by = NULL,
                          time_type = c("auto", "duration", "period"),
                          check_regular = TRUE,
                          na.rm = TRUE){
  stopifnot(is_time_or_num(x))
  if (length(x) <= 1L){
    return(0L)
  }
  n_unique <- n_unique(x, na.rm = na.rm)
  if (n_unique == 1){
    return(0L)
  }
  time_type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
  tby <- time_by_get(x, time_by = time_by)
  x_rng <- collapse::frange(x, na.rm = na.rm)
  # range_diff <- time_diff(x_rng[1L],
  #                         x_rng[2L],
  #                         time_by = tby,
  #                         time_type = time_type)
  if (check_regular && !is.null(time_by)){
    full_seq <- time_seq(x_rng[1L],
                         x_rng[2L],
                         time_by = tby,
                         time_type = time_type)
    full_seq_size <- length(full_seq)
    check_time_regular(x, full_seq, time_by)
  } else {
    full_seq_size <- time_seq_sizes(x_rng[1L],
                                    x_rng[2L],
                                    time_by = tby,
                                    time_type = time_type)
  }
  full_seq_size - n_unique
}
#' @rdname time_gaps
#' @export
time_has_gaps <- function(x, time_by = NULL,
                          time_type = c("auto", "duration", "period"),
                          check_regular = TRUE, na.rm = TRUE){
  time_num_gaps(x, time_by = time_by,
                time_type = time_type,
                check_regular = check_regular, na.rm = na.rm) > 0
}
check_time_regular <- function(x, seq, time_by){
  if (!is.null(time_by)){
    if (length(setdiff(x, seq) > 0L)){
      stop("x is not regular given the chosen time unit")
    }
  }
}
