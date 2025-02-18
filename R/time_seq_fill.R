seq_direction <- function(x){
  first <- collapse::ffirst(x, na.rm = TRUE)
  last <- collapse::flast(x, na.rm = TRUE)
  sign(last - first)
}

time_seq_fill <- function(x){
  check_is_time_or_num(x)
  if (length(x) == 0){
    return(x)
  }
  if (cheapr::all_na(x)){
    warning("all values of x are NA, cannot fill the explicit missing values")
    return(x)
  }
  desc <- seq_direction(x) < 1
  timespan <- granularity(x, exact = TRUE)
  if (desc){
    timespan <- -timespan
  }
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
    time_to_subtract <- timespan * n_first_nas
    start <- time_subtract(x[n_first_nas + 1L], time_to_subtract)
  }
  if (n_last_nas == 0){
    end <- vec_tail(x)
  } else {
    time_to_add <- timespan * n_last_nas
    end <- time_add(x[length(x) - n_last_nas], time_to_add)
  }

  # Filter out first and last NA sequence

  x <- x[(n_first_nas + 1L):(length(x) - n_last_nas)]

  elapsed <- time_elapsed(x,
    timespan, rolling = FALSE, na_skip = TRUE
  )
  if (is.integer(elapsed)){
    target <- seq.int(0L, length(x) - 1L, 1L)
    is_regular <- all(elapsed == target, na.rm = TRUE)
  } else {
    is_regular <- cppdoubles::all_equal(elapsed, seq.int(0, length(x) - 1, 1), na.rm = TRUE)
  }

  if (!is_regular){
    cli::cli_abort("{.arg x} must be a regular sequence with no duplicates or implicit gaps in time")
  }
  if (num_na > 0){
    time_seq_v(start, end, timespan)
  } else {
    x
  }
}
