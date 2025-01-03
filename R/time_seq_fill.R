time_seq_fill <- function(x){
  check_is_time_or_num(x)
  if (cheapr::all_na(x)){
    warning("all values of x are NA, cannot fill the explicit missing values")
    return(x)
  }
  timespan <- granularity(x)
  # If the sequence starts/ends with NA, we need to work with everything but these
  # first/last NA values
  which_na <- which_na(x)
  num_na <- length(which_na)
  if (num_na > 0 && which_na[1] == 1){
    which_last_missing_value_from_left <- cpp_which_first_gap(which_na, 1L, TRUE)
    if (length(which_last_missing_value_from_left) == 0){
      which_last_missing_value_from_left <- num_na
    }
  } else {
    which_last_missing_value_from_left <- integer()
  }
  if (num_na > 0 && which_na[num_na] == length(x)){
    which_last_missing_value_from_right <- cpp_which_first_gap(which_na, 1L, FALSE)
    if (length(which_last_missing_value_from_right) == 0){
      which_last_missing_value_from_right <- 1L
    }
  } else {
    which_last_missing_value_from_right <- integer()
  }
  first_na_locs <- integer()
  if (length(which_last_missing_value_from_left) == 0){
    n_first_nas <- 0L
  } else {
    n_first_nas <- which_last_missing_value_from_left
  }
  if (length(which_last_missing_value_from_right) == 0){
    n_last_nas <- 0L
  } else {
    n_last_nas <- num_na - which_last_missing_value_from_right + 1L
  }
  if (n_first_nas == 0){
    start <- vec_head(x)
  } else {
    time_to_subtract <- timespan(timespan, (timespan_num(timespan) * n_first_nas))
    start <- time_subtract(x[n_first_nas + 1L], time_to_subtract)
  }
  if (n_last_nas == 0){
    end <- vec_tail(x)
  } else {
    time_to_add <- timespan(timespan, (timespan_num(timespan) * n_last_nas))
    end <- time_add(x[length(x) - n_last_nas], time_to_add)
  }
  elapsed <- time_elapsed(x, timespan, rolling = TRUE, na_skip = TRUE)
  desc <- sum(elapsed, na.rm = TRUE) < 0
  if (num_na > 0){
    na_count <- cpp_consecutive_na_id(x, TRUE)

    # Lag na_count without taking a copy
    na_count <- cheapr::lag_(na_count, fill = 0L, set = TRUE)

    # Adjust elapsed time between non-NA values
    # by the amount of NA values between them
    # e.g. if there are 2 NA values between a pair of values then
    # We subtract 2 from the elapsed time between this pair
    if (desc){
      elapsed <- elapsed + na_count
      timespan <- timespan(timespan, num = -timespan_num(timespan))
    } else {
      elapsed <- elapsed - na_count
    }
  }
  elapsed <- as.double(elapsed)
  is_regular <-
    cppdoubles::all_equal(elapsed, 1, na.rm = TRUE) ||
    cppdoubles::all_equal(elapsed, -1, na.rm = TRUE)

  # is_regular <- all(abs(seq_diff - 1L) < sqrt(.Machine$double.eps), na.rm = TRUE)
  if (!is_regular){
    stop("x must be a regular sequence with no duplicates or implicit gaps in time.")
  }
  if (num_na > 0){
    time_seq_v(start, end, timespan)
  } else {
    x
  }
}
