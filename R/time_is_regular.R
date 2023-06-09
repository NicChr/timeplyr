#' Is time a regular sequence? (Experimental)
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
#' @param na.rm Should `NA` values be removed before calculation?
#' Default is `TRUE`.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks,
#' months or years are specified, and `durations`
#' are used otherwise. If `durations`
#' are used the output is always of class `POSIXt`.
#' @param allow_gaps Should gaps be allowed? Default is `TRUE`.
#' @param allow_dups Should duplicates be allowed? Default is `TRUE`.
#' @export
time_is_regular <- function(x, time_by = NULL,
                            g = NULL, use.g.names = TRUE,
                            na.rm = TRUE,
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
  time_by <- time_by_get(x, time_by = time_by)
  time <- gunique(x, sort = TRUE, g = g, use.g.names = TRUE)
  groups <- names(time)
  telapsed <- time_elapsed(time, time_by = time_by, g = groups,
                           time_type = time_type, rolling = FALSE,
                           na_skip = na.rm,
                           fill = 0)
  # telapsed <- gunique(telapsed, g = groups, use.g.names = TRUE, sort = FALSE)
  # # telapsed <- telapsed[!is.na(telapsed)]
  # groups <- names(telapsed)
  # x_na <- is.na(x)
  if (is.null(time_by)){
    if (is.null(g)){
      out <- TRUE
    } else {
      out <- rep_len(TRUE, n_unique(groups))
    }
  } else {
  tol <- sqrt(.Machine$double.eps)
  is_whole_num <- abs(round(telapsed) - telapsed) < tol
  if (na.rm){
    is_whole_num[is.na(is_whole_num)] <- TRUE
  }
    n_whole_num <- collapse::fsum(is_whole_num, g = groups, use.g.names = FALSE,
                                  na.rm = na.rm, fill = FALSE)
    if (is.null(g)){
      group_size <- length(telapsed)
    } else {
      group_size <- fn(groups, g = groups, sort = TRUE)
    }
    out <- n_whole_num == group_size
  }
  if (use.g.names){
    names(out) <- unique(groups)
  }
  if (!allow_gaps){
    has_gaps <- time_has_gaps(x, time_by = time_by,
                              g = g, use.g.names = TRUE,
                              time_type = time_type,
                              check_time_regular = FALSE)
    out <- out & !has_gaps
  }
  if (!allow_dups){
    gduplicated <- gduplicated(x, g = g)
    if (na.rm){
      gduplicated <- gduplicated & !is.na(x)
    }
    has_dups <- collapse::fsum(gduplicated) > 0
    out <- out & !has_dups
  }
  out
}
check_time_elapsed_regular <- function(x){
    unique_elapsed <- collapse::funique(x)
    is_regular <- is_whole_number(unique_elapsed, na.rm = TRUE)
    if (!is_regular){
      stop("x is not regular given the chosen time unit")
    }
}
check_time_elapsed_order <- function(x){
  if (isTRUE(collapse::fmin(x, na.rm = TRUE) < 0)){
    stop("x must be in ascending or descending order")
  }
}
