#' Gaps in a regular time sequence
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
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame.
#' @param use.g.names Should the result include group names?
#' Default is `TRUE`.
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
#' @param check_time_regular Should the time vector be
#' checked to see if it is regular (with or without gaps)?
#' Default is `FALSE`.
#'
#' @details When `check_time_regular` is TRUE, `x` is passed to
#' `time_is_regular`, which checks that the time elapsed between successive
#' values are in increasing order and are whole numbers.
#' For more strict checks, see `?time_is_regular`.
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
#' time_num_gaps(flights$time_hour, g = flights$origin)
#'
#' # Number of missing hours by origin and dest
#' flights %>%
#'   group_by(origin, dest) %>%
#'   summarise(n_missing = time_num_gaps(time_hour, "hours"))
#' @rdname time_gaps
#' @export
time_gaps <- function(x, time_by = NULL,
                      g = NULL, use.g.names = TRUE,
                      time_type = c("auto", "duration", "period"),
                      check_time_regular = FALSE){
  g <- GRP2(g)
  if (!is.null(g)){
    check_data_GRP_size(x, g)
    names(x) <- GRP_names(g, expand = TRUE)
  }
  time_by <- time_by_get(x, time_by = time_by)
  time_seq <- time_expandv(x, time_by = time_by,
                           g = g, use.g.names = TRUE,
                           time_type = time_type)
  if (check_time_regular){
    is_regular <- time_is_regular(x, time_by = time_by,
                                  g = g, use.g.names = FALSE,
                                  time_type = time_type)
    if (collapse::anyv(is_regular, FALSE)){
      stop("x is not regular given the chosen time unit")
    }
  }
  time_tbl <- fenframe(x,
                       name = "group",
                       value = "time")
  # num_na <- fnmiss(x, g = g, TRA = "replace_fill")
  time_not_na <- !is.na(time_tbl[["time"]])
  time_tbl <- df_row_slice(time_tbl, time_not_na)
  time_full_tbl <- fenframe(time_seq,
                            name = "group",
                            value = "time")
  # if (!is.null(time_by) && check_time_regular){
  #   if (nrow2(
  #     dplyr::anti_join(time_tbl,
  #                      time_full_tbl,
  #                      by = names(time_tbl))
  #   ) > 0L){
  #     stop("x is not regular given the chosen time unit")
  #   }
  # }
  out_tbl <- dplyr::anti_join(time_full_tbl,
                              time_tbl,
                              by = names(time_tbl))
  if (!use.g.names){
    out_tbl <- fselect(out_tbl, .cols = "time")
  }
  fdeframe(out_tbl)
}
#' @rdname time_gaps
#' @export
time_num_gaps <- function(x, time_by = NULL,
                          g = NULL, use.g.names = TRUE,
                          na.rm = TRUE,
                          time_type = c("auto", "duration", "period"),
                          check_time_regular = FALSE){
  check_is_time_or_num(x)
  time_type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
  if (length(x) == 0L){
    return(0L)
  }
  g <- GRP2(g)
  check_data_GRP_size(x, g)
  tby <- time_by_get(x, time_by = time_by)
  if (check_time_regular){
    is_regular <- time_is_regular(x, g = g, time_by = tby,
                                  use.g.names = FALSE,
                                  time_type = time_type)
    if (collapse::anyv(is_regular, FALSE)){
      stop("x is not regular given the chosen time unit")
    }
  }
  # start <- collapse::fmin(x, g = g, na.rm = na.rm, use.g.names = FALSE)
  # end <- collapse::fmax(x, g = g, na.rm = na.rm, use.g.names = FALSE)
  # full_seq_size <- time_seq_sizes(start,
  #                                 end,
  #                                 time_by = tby,
  #                                 time_type = time_type)
  n_unique <- collapse::fndistinct(x, g = g, na.rm = na.rm, use.g.names = FALSE)
  full_seq_size <- time_span_size(x, time_by = tby,
                                  time_type = time_type,
                                  g = g, use.g.names = FALSE)
  out <- full_seq_size - n_unique
  if (use.g.names){
    names(out) <- GRP_names(g)
  }
  out
}
#' @rdname time_gaps
#' @export
time_has_gaps <- function(x, time_by = NULL,
                          g = NULL, use.g.names = TRUE,
                          na.rm = TRUE,
                          time_type = c("auto", "duration", "period"),
                          check_time_regular = FALSE){
  time_num_gaps(x, time_by = time_by,
                g = g, use.g.names = use.g.names,
                na.rm = na.rm,
                time_type = time_type,
                check_time_regular = check_time_regular) > 0
}
