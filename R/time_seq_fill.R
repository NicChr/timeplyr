time_seq_fill <- function(x, time_by = NULL,
                          time_type = getOption("timeplyr.time_type", "auto"),
                          roll_month = getOption("timeplyr.roll_month", "preday"),
                          roll_dst = getOption("timeplyr.roll_dst", "boundary")){
  check_is_time_or_num(x)
  time_by <- time_by_get(x, time_by)
  check_time_by_length_is_one(time_by)
  # If the sequence starts/ends with NA, we need to work with everything but these
  # first/last NA values
  which_na <- cheapr::which_na(x)
  if (length(which_na) == length(x)){
    warning("all values of x are NA, cannot fill the explicit missing values")
    return(x)
  }
  if (length(which_na) > 0 && which_na[1] == 1){
    which_last_missing_value_from_left <- cpp_which_first_gap(which_na, 1L, TRUE)
    if (length(which_last_missing_value_from_left) == 0){
      which_last_missing_value_from_left <- length(which_na)
    }
  } else {
    which_last_missing_value_from_left <- integer()
  }
  if (length(which_na) > 0 && which_na[length(which_na)] == length(x)){
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
    n_last_nas <- length(which_na) - which_last_missing_value_from_right + 1L
  }
  if (n_first_nas == 0){
    start <- vec_head(x)
  } else {
    time_add <- add_names(
      list(
        -(time_by_num(time_by) * n_first_nas)
      ),
      time_by_unit(time_by)
    )
    start <- time_add2(x[n_first_nas + 1L], time_add,
                       time_type = time_type,
                       roll_month = roll_month,
                       roll_dst = roll_dst)
  }
  if (n_last_nas == 0){
    end <- vec_tail(x)
  } else {
    time_add <- add_names(
      list(
        (time_by_num(time_by) * n_last_nas)
      ),
      time_by_unit(time_by)
    )
    end <- time_add2(x[length(x) - n_last_nas], time_add,
                     time_type = time_type,
                     roll_month = roll_month,
                     roll_dst = roll_dst)
  }
  elapsed <- time_elapsed(x, rolling = TRUE,
                          time_by = time_by,
                          time_type = time_type,
                          na_skip = TRUE)
  na_count <- cpp_consecutive_na_id(x, TRUE)

  # These should all be equal to 1
  seq_diff <- (elapsed - roll_lag(na_count, fill = 0L))
  is_regular <- all(abs(seq_diff - 1) < sqrt(.Machine$double.eps), na.rm = TRUE)
  if (!is_regular){
    stop("x must be a regular sequence with no duplicates or implicit gaps in time.")
  }
  time_seq_v(start, end, time_by = time_by,
             time_type = time_type,
             roll_month = roll_month,
             roll_dst = roll_dst)
}
