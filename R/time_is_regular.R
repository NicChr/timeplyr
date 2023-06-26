#' Is time a regular sequence?
#'
#' @description This function is a fast way to check if a time vector
#' is a regular sequence, possibly for many groups.
#' Regular in this context means that the lagged time differences are a
#' whole multiple of the specified time unit. \cr
#' This means `x` can be a regular sequence with or without gaps in time.
#'
#' @param x A vector. Can be a
#' `Date`, `POSIXt`, `numeric`, `integer`, `yearmon`, or `yearqtr`.
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
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame. \cr
#' Note that when `g` is supplied the output is a logical with length
#' matching the number of unique groups.
#' @param use.g.names Should the result include group names?
#' Default is `TRUE`.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks,
#' months or years are specified, and `durations`
#' are used otherwise. If `durations`
#' are used the output is always of class `POSIXt`.
#' @param is_sorted Should the function assume `x` is sorted? \cr
#' If `FALSE` the data is sorted before calculation.
#' @param allow_gaps \bold{Not currently used}.
#' @param allow_dups \bold{Not currently used}.
time_is_regular <- function(x, time_by = NULL,
                            g = NULL, use.g.names = TRUE,
                            is_sorted = FALSE,
                            time_type = c("auto", "duration", "period"),
                            allow_gaps = TRUE,
                            allow_dups = TRUE){
  stopifnot(is_time_or_num(x))
  if (length(x) == 0L){
    return(TRUE)
  }
  x <- unname(x)
  if (!is.null(g)){
    g <- GRP2(g)
    if (GRP_data_size(g) != length(x)){
      stop("g must have the same size as x")
    }
    group_sizes <- GRP_group_sizes(g)
    if (use.g.names){
      names(x) <- GRP_names(g, expand = TRUE)
    }
  } else {
    group_sizes <- n_unique(x)
  }
  time <- gunique(x, sort = !is_sorted, g = g, use.g.names = TRUE)
  groups <- names(time)
  telapsed <- time_elapsed(time, time_by = time_by, g = groups,
                           time_type = time_type, rolling = TRUE)
  telapsed <- gunique(telapsed, g = groups, use.g.names = TRUE, sort = FALSE)
  # telapsed <- telapsed[!is.na(telapsed)]
  groups <- names(telapsed)
  if (is.null(time_by)){
    if (is.null(g)){
      out <- TRUE
    } else {
      out <- rep_len(TRUE, n_unique(groups))
    }
  } else {
    is_whole_num <- floor(telapsed) == telapsed
    n_whole_num <- collapse::fsum(is_whole_num, g = groups, use.g.names = FALSE)
    if (is.null(g)){
      group_size <- length(telapsed)
    } else {
      group_size <- fn(groups, g = groups, sort = TRUE)
    }
    out <- n_whole_num == group_size
  }
  if (isTRUE(any(collapse::fmin(telapsed, na.rm = TRUE,
                                g = groups, use.g.names = FALSE) < 0))){
    stop("x must be in ascending or descending order")
  }
  if (use.g.names){
    names(out) <- unique(groups)
  }
  out
}
