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
#'
#' @details If your data consists of sequences that increase by
#' an increment lower than the one provided through `time_by`,
#' an error is thrown.
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
                      time_type = c("auto", "duration", "period")){
  if (!is.null(g)){
    g <- GRP2(g)
    if (GRP_data_size(g) != length(x)){
      stop("g must have the same size as x")
    }
    names(x) <- GRP_names(g, expand = TRUE)
  }
  time_seq <- time_expandv(x, time_by = time_by,
                           g = g, use.g.names = TRUE,
                           time_type = time_type)
  time_tbl <- fenframe(x,
                       name = "group",
                       value = "time")
  # num_na <- fnmiss(x, g = g, TRA = "replace_fill")
  time_not_na <- !is.na(time_tbl[["time"]])
  time_tbl <- df_row_slice(time_tbl, time_not_na)
  time_full_tbl <- fenframe(time_seq,
                            name = "group",
                            value = "time")
  if (!is.null(time_by)){
    if (nrow2(
      dplyr::anti_join(time_tbl,
                       time_full_tbl,
                       by = names(time_tbl))
    ) > 0L){
      stop("x is not regular given the chosen time unit")
    }
  }
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
                          time_type = c("auto", "duration", "period")){
  stopifnot(is_time_or_num(x))
  time_type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
  if (length(x) == 0L){
    return(0L)
  }
  if (!is.null(g)){
    g <- GRP2(g)
    if (GRP_data_size(g) != length(x)){
      stop("g must have the same size as x")
    }
  }
  tby <- time_by_get(x, time_by = time_by)
  if (!is.null(time_by)){
    is_regular <- time_is_regular(x, g = g, time_by = tby,
                                  use.g.names = FALSE,
                                  time_type = time_type)
    if (any(!is_regular, na.rm = TRUE)){
      stop("x is not regular given the chosen time unit")
    }
  }
  start <- collapse::fmin(x, g = g, na.rm = TRUE, use.g.names = FALSE)
  end <- collapse::fmax(x, g = g, na.rm = TRUE, use.g.names = FALSE)
  n_unique <- collapse::fndistinct(x, g = g, na.rm = TRUE, use.g.names = FALSE)
  full_seq_size <- time_seq_sizes(start,
                                  end,
                                  time_by = tby,
                                  time_type = time_type)
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
                          time_type = c("auto", "duration", "period")){
  time_num_gaps(x, time_by = time_by,
                g = g, use.g.names = use.g.names,
                time_type = time_type) > 0
}
check_time_regular <- function(x, seq, time_by){
  if (!is.null(time_by)){
    if (length(setdiff(x, seq) > 0L)){
      stop("x is not regular given the chosen time unit")
    }
  }
}
time_which_gaps <- function(x, time_by = NULL,
                            time_type = c("auto", "duration", "period"),
                            na.rm = TRUE){
  t_seq_id <- time_seq_id(x,
                          time_by = time_by,
                          time_type = time_type,
                          na.rm = na.rm)
  out <- collapse::flast(seq_along(x), g = t_seq_id, use.g.names = FALSE,
                         na.rm = FALSE)
  out[-length(out)]
}
# time_gaps <- function(x, time_by = NULL,
#                       time_type = c("auto", "duration", "period"),
#                       check_regular = TRUE,
#                       na.rm = TRUE){
#   if (!na.rm && sum(is.na(x)) > 0){
#     vctrs::vec_init(x, n = 1L)
#   } else {
#     time_seq <- time_expandv(x, time_by = time_by,
#                              time_type = time_type)
#     if (check_regular){
#       check_time_regular(x, time_seq, time_by)
#     }
#     time_seq[!time_seq %in% x]
#   }
# }
# time_which_gaps <- function(x, time_by = NULL,
#                             time_type = c("auto", "duration", "period"),
#                             check_regular = TRUE, na.rm = TRUE){
#   stopifnot(is_time_or_num(x))
#   if (length(x) <= 1L){
#     return(0L)
#   }
#   n_unique <- n_unique(x, na.rm = na.rm)
#   if (n_unique == 1){
#     return(0L)
#   }
#   time_type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
#   tby <- time_by_get(x, time_by = time_by)
#
#   if (!na.rm && sum(is.na(x)) > 0){
#     out <- NA_integer_
#   } else {
#     time_seq <- time_expandv(x, time_by = time_by,
#                              time_type = time_type)
#     x_completed <- fcomplete(dplyr::tibble(x = x, .time.id = seq_along(x)),
#                              x = time_seq, sort = TRUE)
#     out <- which(is.na(fpluck(x_completed, ".time.id"))) - 1L
#     if (check_regular){
#       check_time_regular(x, time_seq, time_by)
#     }
#   }
#   out
# }
# time_num_gaps <- function(x, time_by = NULL,
#                           time_type = c("auto", "duration", "period"),
#                           check_regular = TRUE,
#                           na.rm = TRUE){
#   stopifnot(is_time_or_num(x))
#   if (length(x) <= 1L){
#     return(0L)
#   }
#   n_unique <- n_unique(x, na.rm = na.rm)
#   if (n_unique == 1){
#     return(0L)
#   }
#   time_type <- rlang::arg_match0(time_type, c("auto", "duration", "period"))
#   tby <- time_by_get(x, time_by = time_by)
#   x_rng <- collapse::frange(x, na.rm = na.rm)
#   if (check_regular && !is.null(time_by)){
#     full_seq <- time_seq(x_rng[1L],
#                          x_rng[2L],
#                          time_by = tby,
#                          time_type = time_type)
#     full_seq_size <- length(full_seq)
#     check_time_regular(x, full_seq, time_by)
#   } else {
#     full_seq_size <- time_seq_sizes(x_rng[1L],
#                                     x_rng[2L],
#                                     time_by = tby,
#                                     time_type = time_type)
#   }
#   full_seq_size - n_unique
# }
