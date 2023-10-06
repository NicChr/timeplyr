#' Vector date and datetime functions
#'
#' @description These are atomic vector-based functions
#' of the tidy equivalents which all have a "v" suffix to denote this.
#' These are more geared towards programmers and allow for working with date and
#' datetime vectors.
#'
#' @param x Time variable. \cr
#' Can be a `Date`, `POSIXt`, `numeric`, `integer`, `yearmon`, or `yearqtr`.
#' @param time_by Time unit. \cr
#' Must be one of the following:
#' * string, specifying either the unit or the number and unit, e.g
#' `time_by = "days"` or `time_by = "2 weeks"`
#' * named list of length one, the unit being the name, and
#' the number the value of the list, e.g. `list("days" = 7)`.
#' For the vectorized time functions, you can supply multiple values,
#' e.g. `list("days" = 1:10)`.
#' * Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
#' then arithmetic is used, e.g `time_by = 1`.
#' @param from Time series start date.
#' @param to Time series end date.
#' @param unique Should the result be unique or match the length of the vector?
#' Default is `TRUE`.
#' @param sort Should the output be sorted? Default is `TRUE`.
#' @param include_interval Logical. If `TRUE` then the result is a `tibble`
#' with a column "interval" of the form `time_min <= x < time_max`
#' showing the time interval in which the aggregated time points belong to.
#' The rightmost interval will always be closed.
#' @param time_type If "auto", `periods` are used for
#' the time expansion when days, weeks, months or years are specified,
#' and `durations` are used otherwise.
#' @param time_floor Should `from` be floored to the nearest unit specified
#' through the `time_by` argument?
#' This is particularly useful for starting sequences at the
#' beginning of a week or month for example.
#' @param week_start day on which week starts following ISO conventions - 1
#' means Monday (default), 7 means Sunday.
#' This is only used when `time_floor = TRUE`.
#' @param roll_month Control how impossible dates are handled when
#' month or year arithmetic is involved.
#' Options are "preday", "boundary", "postday", "full" and "NA".
#' See `?timechange::time_add` for more details.
#' @param roll_dst See `?timechange::time_add` for the full list of details.
#' @param use.names Should a named vector be returned for `time_countv()`?
#' @param complete Logical. If `TRUE` implicit gaps in time are filled
#' before counting and after time aggregation (controlled using `time_by`).
#' The default is `TRUE`.
#' @param g Grouping object passed directly to `collapse::GRP()`.
#' This can for example be a vector or data frame.
#' @param use.g.names Should the result include group names?
#' Default is `TRUE`.
#' @returns
#' Vectors (typically the same class as `x`) of varying lengths depending
#' on the arguments supplied.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' flights <- nycflights13::flights
#' x <- flights$time_hour
#'
#' # Number of missing hours
#' time_num_gaps(x)
#'
#' # Same as above
#' time_span_size(x) - length(unique(x))
#'
#' # Time sequence that spans the data
#' time_span(x) # Automatically detects hour granularity
#' time_span(x, time_by = "hour")
#' time_span(x, time_by = "month")
#' time_span(x, time_by = list("quarters" = 1),
#'              to = today(),
#'              # Floor start of sequence to nearest month
#'              time_floor = TRUE)
#'
#' # Complete missing gaps in time using time_completev
#' ux <- unique(x)
#' y <- time_completev(ux, time_by = "hour")
#' all.equal(y[!y %in% ux], time_gaps(ux))
#'
#' # Summarise time using time_summarisev
#' time_summarisev(y, time_by = "quarter")
#' time_summarisev(y, time_by = "quarter", unique = TRUE)
#' flights %>%
#'   fcount(quarter_start = time_summarisev(time_hour, "quarter"))
#' @rdname time_core
#' @export
time_expandv <- function(x, time_by = NULL, from = NULL, to = NULL,
                         g = NULL, use.g.names = TRUE,
                         time_type = c("auto", "duration", "period"),
                         time_floor = FALSE,
                         week_start = getOption("lubridate.week.start", 1),
                         roll_month = "preday", roll_dst = "pre"){
  check_is_time_or_num(x)
  if (length(from) > 1L){
    stop("from must be of length 1")
  }
  if (length(to) > 1L){
    stop("to must be of length 1")
  }
  time_by <- time_by_get(x, time_by = time_by,
                         is_sorted = FALSE)
  if (time_by_length(time_by) > 1L){
    stop("time_by must be a time unit containing a single numeric increment")
  }
  g <- GRP2(g)
  check_data_GRP_size(x, g)
  has_groups <- !is.null(g)
  if (is.null(from)){
    from <- collapse::fmin(x, g = g, use.g.names = FALSE, na.rm = TRUE)
  }
  if (is.null(to)){
    to <- collapse::fmax(x, g = g, use.g.names = FALSE, na.rm = TRUE)
  }
  # Make sure from/to are datetimes if x is datetime
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  if (time_floor){
    from <- time_floor2(from, time_by, week_start = week_start)
  }
  seq_sizes <- time_seq_sizes(from, to, time_by, time_type = time_type)
  if (isTRUE(log10(sum(seq_sizes)) >= 8)){
    message("The final size exceeds 100m rows, this may take a while")
  }
  out <- time_seq_v2(seq_sizes,
                     from = from,
                     time_by = time_by,
                     time_type = time_type,
                     time_floor = FALSE,
                     week_start = week_start,
                     roll_month = roll_month,
                     roll_dst = roll_dst)
  if (has_groups && use.g.names){
    group_names <- GRP_names(g)
    if (!is.null(group_names)){
      names(out) <- rep.int(group_names, times = seq_sizes)
    }
  }
  out
}
#' @rdname time_core
#' @export
time_completev <- function(x, time_by = NULL, from = NULL, to = NULL,
                           sort = TRUE,
                           time_type = c("auto", "duration", "period"),
                           time_floor = FALSE,
                           week_start = getOption("lubridate.week.start", 1),
                           roll_month = "preday", roll_dst = "pre"){
  time_full <- time_expandv(x, time_by = time_by,
                            from = from, to = to,
                            time_type = time_type,
                            time_floor = time_floor,
                            week_start = week_start,
                            roll_month = roll_month,
                            roll_dst = roll_dst)
  out <- time_c(x, time_full[!time_full %in% x])
  if (sort){
    out <- conditional_sort(out)
  }
  out
}
#' @rdname time_core
#' @export
time_summarisev <- function(x, time_by = NULL, from = NULL, to = NULL,
                            sort = FALSE, unique = FALSE,
                            time_type = c("auto", "duration", "period"),
                            time_floor = FALSE,
                            week_start = getOption("lubridate.week.start", 1),
                            roll_month = "preday", roll_dst = "pre",
                            include_interval = FALSE){
  check_is_time_or_num(x)
  if (is.null(from)){
    from <- collapse::fmin(x, na.rm = TRUE)
  }
  if (is.null(to)){
    to <- collapse::fmax(x, na.rm = TRUE)
  }
  # Time sequence
  time_breaks <- time_expandv(x, time_by = time_by,
                              from = from, to = to,
                              time_type = time_type,
                              time_floor = time_floor,
                              week_start = week_start,
                              roll_month = roll_month,
                              roll_dst = roll_dst)
  x <- time_cast(x, time_breaks)
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  # Cut time
  time_bins <- c(time_as_number(time_breaks),
                 time_as_number(to))
  time_break_ind <- cut_time(x, breaks = time_bins, codes = TRUE)
  # Time breaks subset on cut indices
  out <- time_breaks[time_break_ind]
  if (include_interval){
    time_int <- tseq_interval(x = to, time_breaks)
    time_int <- time_int[time_break_ind]
    out <- new_tbl(x = out, interval = time_int)
    # Unique and sorting
    if (unique){
      out <- fdistinct(out, .cols = "x", .keep_all = TRUE)
    }
    if (sort){
      out <- farrange(out, .cols = "x")
    }
    if (!is_interval(time_int)){
      attr(out[["interval"]], "start") <- out[["x"]]
    }
  } else {
    if (unique){
      out <- collapse::funique(out, sort = sort)
    } else {
      if (sort){
        out <- radix_sort(out)
      }
    }
  }
  out
}
# Working function
# ungrouped_time_expand <- function(x, time_by = NULL, from = NULL, to = NULL,
#                                   time_type = c("auto", "duration", "period"),
#                                   time_floor = FALSE,
#                                   week_start = getOption("lubridate.week.start", 1),
#                                   roll_month = "preday", roll_dst = "pre"){
#   stopifnot(is_time_or_num(x))
#   if (length(from) > 1L){
#     stop("from must be of length 1")
#   }
#   if (length(to) > 1L){
#     stop("to must be of length 1")
#   }
#   time_by <- time_by_get(x, time_by = time_by,
#                          is_sorted = FALSE)
#   if (time_by_length(time_by) > 1L){
#     stop("time_by must be a time unit containing a single numeric increment")
#   }
#   if (is.null(from) || is.null(to)){
#     time_range <- collapse::frange(x, na.rm = TRUE)
#   }
#   if (is.null(from)){
#     from <- time_range[1L]
#   }
#   if (is.null(to)){
#     to <- time_range[2L]
#   }
#   # Make sure from/to are datetimes if x is datetime
#   from <- time_cast(from, x)
#   to <- time_cast(to, x)
#   if (time_floor){
#     from <- time_floor2(from, time_by, week_start = week_start)
#   }
#   seq_sizes <- time_seq_sizes(from, to, time_by, time_type = time_type)
#   if (isTRUE(log10(sum(seq_sizes)) >= 8)){
#     message("The final size exceeds 8m, this may take a while")
#   }
#   time_seq_v2(seq_sizes, from = from, time_by = time_by,
#               time_type = time_type,
#               time_floor = FALSE,
#               week_start = week_start,
#               roll_month = roll_month, roll_dst = roll_dst)
# }
# Working function
# grouped_time_expand <- function(x, time_by = NULL, from = NULL, to = NULL,
#                                 g, use.g.names = TRUE,
#                                 time_type = c("auto", "duration", "period"),
#                                 time_floor = FALSE,
#                                 week_start = getOption("lubridate.week.start", 1),
#                                 roll_month = "preday", roll_dst = "pre"){
#   stopifnot(is_time_or_num(x))
#   if (length(from) > 1L){
#     stop("from must be of length 1")
#   }
#   if (length(to) > 1L){
#     stop("to must be of length 1")
#   }
#   time_by <- time_by_get(x, time_by = time_by,
#                          is_sorted = FALSE)
#   if (time_by_length(time_by) > 1L){
#     stop("time_by must be a time unit containing a single numeric increment")
#   }
#   GRP <- GRP2(g)
#   if (GRP_data_size(GRP) != length(x)){
#     stop("g must have the same size as x")
#   }
#   if (is.null(from)){
#     from <- collapse::fmin(x, g = GRP, use.g.names = FALSE)
#   }
#   if (is.null(to)){
#     to <- collapse::fmax(x, g = GRP, use.g.names = FALSE)
#   }
#   # Make sure from/to are datetimes if x is datetime
#   from <- time_cast(from, x)
#   to <- time_cast(to, x)
#   if (time_floor){
#     from <- time_floor2(from, time_by, week_start = week_start)
#   }
#   seq_sizes <- time_seq_sizes(from, to, time_by, time_type = time_type)
#   if (isTRUE(log10(sum(seq_sizes)) >= 8)){
#     message("The final size exceeds 8m, this may take a while")
#   }
#   full_seq <- time_seq_v2(seq_sizes, from = from, time_by = time_by,
#                           time_type = time_type,
#                           time_floor = FALSE,
#                           week_start = week_start,
#                           roll_month = roll_month, roll_dst = roll_dst)
#   out <- df_rep(GRP_group_data(GRP), seq_sizes)
#   if (use.g.names){
#     out[[".GRP.NAMES"]] <- rep.int(GRP_names(GRP), seq_sizes)
#   }
#   out[[".time"]] <- full_seq
#   out
# }

# Almost-working function
# time_completev <- function(x, time_by = NULL, from = NULL, to = NULL,
#                            g = NULL, use.g.names = TRUE,
#                            sort = TRUE,
#                            time_type = c("auto", "duration", "period"),
#                            time_floor = FALSE,
#                            week_start = getOption("lubridate.week.start", 1),
#                            roll_month = "preday", roll_dst = "pre",
#                            as_tbl = FALSE){
#   has_groups <- length(g) > 0
#   if (!has_groups){
#     time_tbl <- fenframe(unname(x), value = ".time")
#   } else {
#     g <- GRP2(g)
#     if (GRP_data_size(g) != length(x)){
#       stop("g must have the same size as x")
#     }
#     check_GRP_has_groups(g)
#     time_tbl <- GRP_group_data(g, expand = TRUE)
#     time_tbl[[".time"]] <- x
#   }
#   time_full_tbl <- time_expandv(x, time_by = time_by,
#                                 from = from, to = to,
#                                 g = g, use.g.names = TRUE,
#                                 time_type = time_type,
#                                 time_floor = time_floor,
#                                 week_start = week_start,
#                                 roll_month = roll_month,
#                                 roll_dst = roll_dst,
#                                 as_tbl = TRUE)
#   out_tbl <- merge(as_DT(time_tbl),
#                    as_DT(time_full_tbl),
#                    by = names(time_tbl),
#                    all = TRUE,
#                    sort = FALSE,
#                    allow.cartesian = TRUE)
#   if (sort){
#     out_tbl <- farrange(out_tbl, .cols = names(out_tbl))
#   }
#   group_nms <- setdiff(names(out_tbl), ".time")
#   if (as_tbl){
#     out <- out_tbl
#     if (!use.g.names){
#       out <- fselect(out, .cols = ".time")
#     }
#   } else {
#     out <- out_tbl[[".time"]]
#     if (use.g.names && has_groups){
#       names(out) <- df_paste_names(out_tbl, .cols = group_nms)
#     }
#   }
#   out
# }
# Working function
# time_summarisev <- function(x, time_by = NULL, from = NULL, to = NULL,
#                             g = NULL, use.g.names = TRUE,
#                             sort = FALSE, unique = FALSE,
#                             time_type = c("auto", "duration", "period"),
#                             time_floor = FALSE,
#                             week_start = getOption("lubridate.week.start", 1),
#                             roll_month = "preday", roll_dst = "pre",
#                             include_interval = FALSE){
#   has_groups <- length(g) > 0
#   if (!has_groups){
#     ungrouped_time_summarise(x, time_by = time_by, from = from, to = to,
#                              time_type = time_type,
#                              sort = sort, unique = unique,
#                              time_floor = time_floor, week_start = week_start,
#                              roll_month = roll_month, roll_dst = roll_dst,
#                              include_interval = include_interval)
#   } else {
#     grouped_time_summarise(x, time_by = time_by, from = from, to = to,
#                            g = g, use.g.names = use.g.names,
#                            time_type = time_type,
#                            sort = sort, unique = unique,
#                            time_floor = time_floor, week_start = week_start,
#                            roll_month = roll_month, roll_dst = roll_dst,
#                            include_interval = include_interval)
#   }
#
# }
# Working function
# ungrouped_time_summarise <- function(x, time_by = NULL, from = NULL, to = NULL,
#                                      sort = FALSE, unique = FALSE,
#                                      time_type = c("auto", "duration", "period"),
#                                      time_floor = FALSE,
#                                      week_start = getOption("lubridate.week.start", 1),
#                                      roll_month = "preday", roll_dst = "pre",
#                                      include_interval = FALSE){
#   if (is.null(from) || is.null(to)){
#     x_range <- collapse::frange(x, na.rm = TRUE)
#   }
#   if (is.null(from)){
#     from <- x_range[1L]
#   } else {
#     from <- time_cast(from, x)
#   }
#   if (is.null(to)){
#     to <- x_range[2L]
#   } else {
#     to <- time_cast(to, x)
#   }
#   # Time sequence
#   time_breaks <- time_expandv(x, time_by = time_by, from = from, to = to,
#                               time_type = time_type,
#                               time_floor = time_floor, week_start = week_start,
#                               roll_month = roll_month, roll_dst = roll_dst)
#   # time_breaks <- time_cast(time_breaks, x)
#   # Cut time
#   time_break_ind <- fcut_ind(x, c(time_breaks, to + 1))
#   # Time breaks subset on cut indices
#   out <- time_breaks[time_break_ind]
#
#   if (include_interval){
#     time_int <- tseq_interval(x = to, time_breaks)
#     time_int <- time_int[time_break_ind]
#     out <- list("x" = out,
#                 "interval" = time_int)
#     out <- list_to_tibble(out)
#     # Unique and sorting
#     if (unique){
#       out <- fdistinct(out, .cols = "x", .keep_all = TRUE)
#       # out <- gunique(out, g = out[["x"]])
#     }
#     if (sort){
#       out <- farrange(out, .cols = "x")
#     }
#     if (!is_interval(time_int)){
#       attr(out[["interval"]], "start") <- out[["x"]]
#     }
#   } else {
#     if (unique){
#       out <- collapse::funique(out, sort = sort)
#     } else {
#       if (sort) out <- radix_sort(out)
#     }
#   }
#   out
# }
# Working function
# grouped_time_summarise <- function(x, time_by = NULL, from = NULL, to = NULL,
#                                    g, use.g.names = TRUE,
#                                    sort = FALSE, unique = FALSE,
#                                    time_type = c("auto", "duration", "period"),
#                                    time_floor = FALSE,
#                                    week_start = getOption("lubridate.week.start", 1),
#                                    roll_month = "preday", roll_dst = "pre",
#                                    include_interval = FALSE){
#   g <- GRP2(g)
#   if (GRP_data_size(g) != length(x)){
#     stop("g must have the same size as x")
#   }
#   if (use.g.names){
#     names(x) <- GRP_names(g, expand = TRUE)
#   }
#   g2 <- GRP2(
#     list(GRP_group_id(g), x)
#   )
#   group_order <- GRP_order(g2)
#   group_id <- GRP_group_id(g)
#   if (!GRP_is_sorted(g2)){
#     x <- x[group_order]
#     g <- collapse::GRP(group_id[group_order])
#   }
#   # Time sequence
#   time_breaks <- time_expandv(x, time_by = time_by, from = from, to = to,
#                               g = g, use.g.names = TRUE,
#                               time_type = time_type,
#                               time_floor = time_floor, week_start = week_start,
#                               roll_month = roll_month, roll_dst = roll_dst)
#   seq_group_id <- group_id(names(time_breaks), order = FALSE)
#   out <- taggregate(x, seq = time_breaks,
#                     gx = GRP_group_id(g), gseq = seq_group_id)
#   if (use.g.names){
#     names(out) <- names(x)
#   }
#   if (include_interval){
#     time_int <- tagg_interval(xagg = out,
#                               x = x,
#                               seq = time_breaks,
#                               gagg = GRP_group_id(g),
#                               gx = GRP_group_id(g),
#                               gseq = seq_group_id)
#     out <- fenframe(out, name = "group", value = "x")
#     out[["interval"]] <- time_int
#     if (!GRP_is_sorted(g2)){
#       out <- df_row_slice(out, collapse::radixorderv(group_order))
#     }
#     # Unique and sorting
#     if (unique){
#       out <- fdistinct(out, .cols = c("group", "x"),
#                        sort = sort, .keep_all = TRUE)
#     }
#     if (sort && !unique){
#       out <- farrange(out, .cols = c("group", "x"))
#     }
#     if (!is_interval(time_int)){
#       attr(out[["interval"]], "start") <- out[["x"]]
#     }
#   } else {
#     if (!GRP_is_sorted(g2) && !sort){
#       out <- out[collapse::radixorderv(group_order)]
#     }
#     if (unique){
#       # This needs a bit of explanation..
#       # If the user wants the data sorted
#       # We must take the unique values of the sorted groups
#       # Otherwise we take the unique values of the pre-sorted groups
#       if (sort){
#         out <- gunique(out, g = GRP_group_id(g), sort = TRUE)
#       } else {
#         out <- gunique(out, g = group_id, sort = FALSE)
#       }
#     }
#   }
#   out
# }
#' @rdname time_core
#' @export
time_countv <- function(x, time_by = NULL, from = NULL, to = NULL,
                        sort = TRUE, unique = TRUE,
                        use.names = TRUE, complete = TRUE,
                        time_type = c("auto", "duration", "period"),
                        include_interval = FALSE,
                        time_floor = FALSE,
                        week_start = getOption("lubridate.week.start", 1),
                        roll_month = "preday", roll_dst = "pre"){
  check_is_time_or_num(x)
  x_na <- collapse::whichNA(x)
  missing_from <- is.null(from)
  missing_to <- is.null(to)
  time_by <- time_by_get(x, time_by = time_by, is_sorted = FALSE)
  if (missing_from){
    from <- collapse::fmin(x, na.rm = TRUE, use.g.names = FALSE)
  } else {
    from <- time_cast(from, x)
  }
  if (missing_to){
    to <- collapse::fmax(x, na.rm = TRUE, use.g.names = FALSE)
  } else {
    to <- time_cast(to, x)
  }
  # Time sequence
  time_breaks <- time_seq_v(from = from, to = to,
                            time_by = time_by,
                            time_type = time_type,
                            time_floor = time_floor,
                            week_start = week_start,
                            roll_month = roll_month, roll_dst = roll_dst)
  x <- time_cast(x, time_breaks)
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  if (!missing_from || !missing_to){
    x <- x[data.table::between(x, from, to, incbounds = TRUE, NAbounds = NA)]
  }
  out_len <- length(x)
  # Aggregate time/cut time
  time_bins <- c(time_as_number(time_breaks),
                 time_as_number(to))
  time_break_ind <- cut_time(x, breaks = time_bins, codes = TRUE)
  # time_break_ind <- cut_time_intervals(x, time_breaks, end = to, codes = TRUE)
  # Time breaks subset on cut indices
  x <- time_breaks[time_break_ind]

  # (Optionally) complete time data
  time_missed <- x[0L]
  if (complete){
    time_missed <- time_breaks[!time_breaks %in% x]
    if (length(time_missed) > 0L){
      x <- c(x, time_missed) # Complete time sequence
    }
  }
  # Count time
  # Don't count completed sequence items, only original..
  cnt_grps <- GRP2(if (complete) x[seq_len(out_len)] else x,
                   sort = FALSE,
                   call = FALSE, return.groups = FALSE,
                   na.last = TRUE, decreasing = FALSE,
                   return.order = FALSE)
  out <- integer(out_len + length(time_missed))
  # Replace allocated integer with counts
  setv(out, seq_len(out_len), collapse::GRPN(cnt_grps, expand = TRUE),
       vind1 = TRUE)
  # if (use.names && !include_interval) out <- add_names(out, x)
  if (include_interval){
    time_seq_int <- tseq_interval(x = to, time_breaks)
    time_int <- time_seq_int[time_break_ind]
    if (complete && length(time_missed) > 0L){
      time_int <- c(time_int, time_seq_int[which(attr(time_seq_int, "start") %in%
                                                   time_cast(time_missed, attr(time_seq_int, "start")))])
    }
    out <- new_tbl(x = x, interval = time_int, n = out)
    if (unique){
      out <- fdistinct(out, .cols = "x", .keep_all = TRUE)
    }
    if (sort){
      if (sort){
        out <- farrange(out, .cols = "x")
      }
    }
    if (!is_interval(out[["interval"]])){
      attr(out[["interval"]], "start") <- out[["x"]]
    }
  } else {
    if (unique || sort){
      dt <- data.table::data.table(x, out)
      if (unique){
        dt <- collapse::funique(dt, cols = "x", sort = sort)
      } else if (!unique && sort){
        data.table::setorderv(dt, cols = "x", na.last = TRUE)
      }
      out <- dt[["out"]]
      if (use.names){
        out <- add_names(out, dt[["x"]])
      }
    } else {
      if (use.names){
        out <- add_names(out, x)
      }
    }
  }
  out
}
#' @rdname time_core
#' @export
time_span <- time_expandv
#' @rdname time_core
#' @export
time_span_size <- function(x, time_by = NULL, from = NULL, to = NULL,
                           g = NULL, use.g.names = TRUE,
                           time_type = c("auto", "duration", "period"),
                           time_floor = FALSE,
                           week_start = getOption("lubridate.week.start", 1)){
  check_is_time_or_num(x)
  if (length(from) > 1L){
    stop("from must be of length 1")
  }
  if (length(to) > 1L){
    stop("to must be of length 1")
  }
  time_by <- time_by_get(x, time_by = time_by,
                         is_sorted = FALSE)
  if (time_by_length(time_by) > 1L){
    stop("time_by must be a time unit containing a single numeric increment")
  }
  g <- GRP2(g)
  check_data_GRP_size(x, g)
  has_groups <- is.null(g)
  if (is.null(from)){
    from <- collapse::fmin(x, g = g, use.g.names = FALSE, na.rm = TRUE)
  }
  if (is.null(to)){
    to <- collapse::fmax(x, g = g, use.g.names = FALSE, na.rm = TRUE)
  }
  # Make sure from/to are datetimes if x is datetime
  from <- time_cast(from, x)
  to <- time_cast(to, x)
  if (time_floor){
    from <- time_floor2(from, time_by = time_by, week_start = week_start)
  }
  out <- time_seq_sizes(from = from, to = to,
                        time_by = time_by,
                        time_type = time_type)
  if (has_groups && use.g.names){
    group_names <- GRP_names(g)
    if (!is.null(group_names)){
      names(out) <- group_names
    }
  }
  out
}
