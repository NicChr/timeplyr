# time_fill <- function(x, time_by = NULL,
#                           time_type = getOption("timeplyr.time_type", "auto"),
#                           roll_month = getOption("timeplyr.roll_month", "preday"),
#                           roll_dst = getOption("timeplyr.roll_dst", "boundary")){
#   check_is_time_or_num(x)
#   time_by <- time_by_get(x, time_by)
#   check_time_by_length_is_one(time_by)
#   # If the sequence starts/ends with NA, we need to work with everything but these
#   # first/last NA values
#   which_na <- cpp_which_na(x)
#   if (length(which_na) == length(x)){
#     warning("all values of x are NA, cannot fill the explicit missing values")
#     return(x)
#   }
#   if (length(which_na) > 0 && which_na[1] == 1){
#     which_last_missing_value_from_left <- cpp_which_first_gap(which_na, 1L, 0L)
#     if (length(which_last_missing_value_from_left) == 0){
#       which_last_missing_value_from_left <- length(which_na)
#     }
#   } else {
#     which_last_missing_value_from_left <- integer()
#   }
#   if (length(which_na) > 0 && which_na[length(which_na)] == length(x)){
#     which_last_missing_value_from_right <- cpp_which_first_gap(which_na, 1L, 1L)
#     if (length(which_last_missing_value_from_right) == 0){
#       which_last_missing_value_from_right <- 1L
#     }
#   } else {
#     which_last_missing_value_from_right <- integer()
#   }
#   first_na_locs <- integer()
#   if (length(which_last_missing_value_from_left) == 0){
#     n_first_nas <- 0L
#   } else {
#     n_first_nas <- which_last_missing_value_from_left
#     first_na_locs <- c(first_na_locs, seq_len(n_first_nas))
#   }
#   if (length(which_last_missing_value_from_right) == 0){
#     n_last_nas <- 0L
#   } else {
#     n_last_nas <- length(which_na) - which_last_missing_value_from_right + 1L
#     first_na_locs <- c(first_na_locs,
#                        sequence2(n_last_nas,
#                                  from = which_na[which_last_missing_value_from_right],
#                                  by = 1L))
#   }
#   y <- x
#   if (n_first_nas == 0){
#     start <- vec_head(x)
#   } else {
#     time_add <- add_names(
#       list(
#         -(time_by_num(time_by) * n_first_nas)
#       ),
#       time_by_unit(time_by)
#     )
#     start <- time_add2(x[n_first_nas + 1L], time_add,
#                        time_type = time_type,
#                        roll_month = roll_month,
#                        roll_dst = roll_dst)
#     y[1L] <- start
#   }
#   if (n_last_nas == 0){
#     end <- vec_tail(x)
#   } else {
#     time_add <- add_names(
#       list(
#         (time_by_num(time_by) * n_last_nas)
#       ),
#       time_by_unit(time_by)
#     )
#     end <- time_add2(x[length(x) - n_last_nas], time_add,
#                      time_type = time_type,
#                      roll_month = roll_month,
#                      roll_dst = roll_dst)
#     y[length(x)] <- end
#   }
#   x_filled <- roll_na_fill(y)
#   elapsed <- time_elapsed(x_filled, rolling = TRUE, fill = 1L,
#                           time_by = time_by,
#                           time_type = time_type,
#                           na_skip = TRUE)
#   na_count <- cpp_consecutive_na_id(y)
#
#   # These should all be equal to 1
#   is_regular <- allv2(
#     (elapsed - roll_lag(na_count, fill = 0L))[cpp_which(is.na(y), invert = TRUE)],
#     1
#   )
#   if (!is_regular){
#     stop("x must be a regular sequence with no duplicates or implicit gaps in time.")
#   }
#   time_seq_v(start, end, time_by = time_by,
#              time_type = time_type,
#              roll_month = roll_month,
#              roll_dst = roll_dst)
# }
