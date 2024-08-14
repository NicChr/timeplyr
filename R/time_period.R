time_period <- function(years = 0L,
                        months = 0L,
                        weeks = 0L,
                        days = 0L,
                        hours = 0L,
                        minutes = 0L,
                        seconds = 0L){

  ### Find which args are missing

  units <- rev(.period_units)
  is_missing <- logical(length(units))
  names(is_missing) <- units
  is_missing[1] <- missing(years)
  is_missing[2] <- missing(months)
  is_missing[3] <- missing(weeks)
  is_missing[4] <- missing(days)
  is_missing[5] <- missing(hours)
  is_missing[6] <- missing(minutes)
  is_missing[7] <- missing(seconds)

  ### Arguments

  args <- list(
    years = years,
    months = months,
    weeks = weeks,
    days = days,
    hours = hours,
    minutes = minutes,
    seconds = seconds
  )

  ### Stop if there are any decimals

  for (i in seq_len(length(args) - 1L)){
    if (!is_whole_number(args[[i]])){
      stop(paste(names(args)[i], "must be a vector of whole numbers"))
    }
  }
  ### Recycled size

  period_lengths <- cheapr::lengths_(args)
  max_length <- max(period_lengths)

  ### Template output

  out <- new_list(length(units), default = integer())
  names(out) <- units

  ### Recycle

  if (all(period_lengths > 0L)){
    out[!is_missing] <- do.call(cheapr::recycle, args[!is_missing])

    # Fill all rows with NA if any of them contain NA

    # which_na_fill <- cheapr::which_(cheapr::row_any_na(list_as_df(out[!is_missing])))



    ### Fill the vectors not specified by the user with an integer of zeroes
    if (any(is_missing)){
      fill <- integer(max_length)
      # if (length(which_na_fill) > 0){
      #   fill[which_na_fill] <- NA_integer_
      # }
      out[is_missing] <- new_list(sum(is_missing), default = fill)
    }
    # else if (length(which_na_fill) > 0) {
    #   for (per in out){
    #     out[[per]][which_na_fill] <- NA
    #   }
    # }
  }
  do.call(new_time_period, out)
}
new_time_period <- function(years = 0L,
                            months = 0L,
                            weeks = 0L,
                            days = 0L,
                            hours = 0L,
                            minutes = 0L,
                            seconds = 0L){
  out <- list(
    years = years,
    months = months,
    weeks = weeks,
    days = days,
    hours = hours,
    minutes = minutes,
    seconds = seconds
  )
  class(out) <- c("time_period", "vctrs_rcrd", "vctrs_vctr")
  out
}
#' @export
print.time_period <- function(x, max = NULL, ...){
  out <- x
  N <- length(out)
  if (is.null(max)){
    max <- getOption("max.print", 9999L)
  }
  max <- min(max, N)
  if (max < N){
    i <- seq_len(max)
    out <- out[i]
    additional_msg <- paste(" [ reached 'max' / getOption(\"max.print\") -- omitted",
                            N - max, "entries ]\n")
  } else {
    additional_msg <- character()
  }
  vctrs::obj_print_header(x)
  vctrs::obj_print_data(out)
  cat(additional_msg)
  invisible(x)
}
#' @export
format.time_period <- function(x, ...){
  periods <- unclass(x)
  abbrs <- c("y", "m", "w", "d", "H", "M", "S")
  units <- names(periods)
  ranges <- lapply(
   periods, function(x) collapse::frange(x, na.rm = TRUE)
  )
  keep <- logical(length(periods))
  fmts <- character(length(periods))
  # We only print periods that have at least 1 non-zero element
  # We're also deciding whether to use an integer or floating point print format

  # time_period() already checked that all values are whole numbers
  # But they might be larger than 32-bit integers..
  for (i in seq_along(keep)){
    keep[i] <- sum(abs(ranges[[i]])) > 0
    if (isTRUE(is_integerable(max(abs(ranges[[i]]))))){
      fmts[i] <- "%d"
    } else {
      fmts[i] <- "%.0f"
    }
  }
  keep <- which_(keep)
  time_list <- periods[keep]
  fmts <- fmts[keep]
  # sprintf() formats
  sprint_fmt <- paste(paste0(fmts, abbrs[match(names(time_list), units)]), collapse = " ")
  if (length(time_list) == 0){
    out <- sprintf("%.0fS", periods[["seconds"]])
  } else {
    out <- do.call(sprintf, c(list(sprint_fmt), time_list))
  }
  out
}

is.time_period <- function(x){
  inherits(x, "time_period")
}
