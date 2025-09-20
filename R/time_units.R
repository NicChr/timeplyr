#' Time units
#'
#' @rdname time_units
#' @export
.time_units <- c("picoseconds", "nanoseconds",
                 "microseconds", "milliseconds",
                 "seconds", "minutes", "hours", "days",
                 "weeks", "months", "years",
                 "fortnights", "quarters", "semesters",
                 "olympiads", "lustrums",
                 "decades", "indictions", "scores",
                 "centuries", "milleniums")
#' @rdname time_units
#' @export
.period_units <- c("seconds", "minutes", "hours", "days",
                   "weeks", "months", "years")
#' @rdname time_units
#' @export
.duration_units <- c("picoseconds", "nanoseconds",
                     "microseconds", "milliseconds",
                     "seconds", "minutes", "hours", "days",
                     "weeks", "months", "years")
#' @rdname time_units
#' @export
.extra_time_units <- c("fortnights", "quarters", "semesters",
                       "olympiads", "lustrums",
                       "decades", "indictions", "scores",
                       "centuries", "milleniums")

# New time units
period_units <- c("days", "weeks", "months", "years")
duration_units <- c("seconds", "minutes", "hours")
time_units <- c(duration_units, period_units)
