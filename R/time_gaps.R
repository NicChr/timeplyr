#' Gaps in a regular time sequence
#'
#' @description `time_gaps()` checks for implicit missing gaps in time for any
#' regular date or datetime sequence.
#'
#' @param x Time vector. \cr
#' E.g. a `Date`, `POSIXt`, `numeric` or any time-based vector.
#' @param timespan [timespan].
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
#' @returns
#' `time_gaps` returns a vector of time gaps. \cr
#' `time_num_gaps` returns the number of time gaps. \cr
#' `time_has_gaps` returns a logical(1) of whether there are gaps.
#'
#' @examples
#'
#' library(timeplyr)
#' library(fastplyr)
#' library(lubridate)
#' library(nycflights13)
#' missing_dates(flights$time_hour)
#' time_has_gaps(flights$time_hour)
#' time_num_gaps(flights$time_hour)
#' length(time_gaps(flights$time_hour))
#' time_num_gaps(flights$time_hour, g = flights$origin)
#'
#' # Number of missing hours by origin and dest
#' flights %>%
#'   f_group_by(origin, dest) %>%
#'   f_summarise(n_missing = time_num_gaps(time_hour, "hours"))
#'
#' @rdname time_gaps
#' @export
time_gaps <- function(x, timespan = granularity(x),
                      g = NULL, use.g.names = TRUE,
                      check_time_regular = FALSE){
  check_is_time_or_num(x)
  g <- GRP2(g, return.groups = use.g.names)
  check_data_GRP_size(x, g)
  if (!is.null(g)){
    names(x) <- GRP_names(g, expand = TRUE)
  }
  timespan <- timespan(timespan)
  time_seq <- time_expandv(x, timespan, g = g, use.g.names = TRUE)
  x <- time_cast(x, time_seq)
  if (check_time_regular){
    is_regular <- time_is_regular(x, timespan,
                                  g = g, use.g.names = FALSE)
    if (collapse::anyv(is_regular, FALSE)){
      cli::cli_abort("{.arg x} is not regular given the chosen time unit")
    }
  }
  time_tbl <- fastplyr::f_enframe(x, name = "group", value = "time")
  time_not_na <- cheapr::val_find(time_tbl[["time"]], NA, invert = TRUE)
  time_tbl <- df_row_slice(time_tbl, time_not_na)
  time_full_tbl <- fastplyr::f_enframe(time_seq, name = "group", value = "time")
  out_tbl <- fastplyr::f_anti_join(time_full_tbl, time_tbl, by = names(time_tbl))
  if (!use.g.names){
    out_tbl <- fastplyr::f_select(out_tbl, .cols = "time")
  }
  fastplyr::f_deframe(out_tbl)
}
#' @rdname time_gaps
#' @export
time_num_gaps <- function(x, timespan = granularity(x),
                          g = NULL, use.g.names = TRUE,
                          na.rm = TRUE,
                          check_time_regular = FALSE){
  check_is_time_or_num(x)
  if (length(x) == 0L){
    return(0L)
  }
  g <- GRP2(g, return.groups = use.g.names)
  check_data_GRP_size(x, g)
  tby <- timespan(timespan)
  if (check_time_regular){
    is_regular <- time_is_regular(x, tby, g = g, use.g.names = FALSE)
    if (collapse::anyv(is_regular, FALSE)){
      cli::cli_abort("{.arg x} is not regular given the chosen time unit")
    }
  }
  n_unique <- collapse::fndistinct(x, g = g, use.g.names = FALSE)
  full_seq_size <- time_expanded_sizes(x, tby, g = g, use.g.names = FALSE)
  out <- full_seq_size - n_unique
  if (!na.rm){
    nmiss <- fnmiss(x, g = g, use.g.names = FALSE)
    out[which(nmiss > 0)] <- NA
  }
  if (use.g.names){
    names(out) <- GRP_names(g)
  }
  out
}
#' @rdname time_gaps
#' @export
time_has_gaps <- function(x, timespan = granularity(x),
                          g = NULL, use.g.names = TRUE,
                          na.rm = TRUE, check_time_regular = FALSE){
  time_num_gaps(x, timespan, g = g, use.g.names = use.g.names,
                na.rm = na.rm,
                check_time_regular = check_time_regular) > 0L
}
