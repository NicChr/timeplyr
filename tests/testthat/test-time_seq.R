# Set number of data.table threads to 1
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Dates", {
  start1 <- lubridate::now()
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  # Dates
  testthat::expect_length(time_seq(start2, end2, time_by = "day"), 11)
  testthat::expect_equal(time_seq(start2, end2, length.out = 11), # Result is datetime
                          lubridate::as_datetime(seq(start2, by = "day", length.out = 11)))
  # testthat::expect_error(time_seq(start2, end2, length.out = 22, time_type = "period"))
  # testthat::expect_error(time_seq(start2, end2, length.out = 10,
  #                                 time_type = "period"))
  testthat::expect_equal(time_seq(start2, end2, time_by = "day"),
                             seq(start2, end2, by = "day"))
  testthat::expect_equal(time_seq(start2, end2,
                                      length.out = 10,
                                      time_type = "duration"),
                             seq(lubridate::as_datetime(start2),
                                 lubridate::as_datetime(end2), length.out = 10))
  # Test for month arithmetic when to is at the end of a month
  # This may be unexpected but at the moment, emphasis is always placed on a start point of any seq
  testthat::expect_equal(time_seq(to = lubridate::ymd("1946-01-31"), length.out = 2, time_by = "9 months"),
                             lubridate::ymd(c("1945-04-30", "1946-01-30")))

  # Very basic tests
  ### DATES ###
  testthat::expect_equal(time_seq(lubridate::Date(0), start2, time_by = "day", time_type = "duration"),
                         lubridate::POSIXct(0))
  testthat::expect_equal(time_seq(lubridate::Date(0), start2, time_by = "day", time_type = "period"),
                         lubridate::Date(0))
  testthat::expect_equal(time_seq(lubridate::POSIXct(0), start2, time_by = "day", time_type = "period"),
                         lubridate::POSIXct(0))
  testthat::expect_error(suppressWarnings(time_seq(start2, lubridate::Date(0), time_by = "day", time_type = "duration")))
  testthat::expect_error(suppressWarnings(time_seq(start2, lubridate::Date(0), time_by = "day", time_type = "period")))
  testthat::expect_error(suppressWarnings(time_seq(start2, lubridate::POSIXct(0), time_by = "day", time_type = "period")))

  testthat::expect_equal(time_seq(start2, end2, time_by = "day"),
                         seq(start2, end2, by = "day"))
  testthat::expect_equal(time_seq(start2, end2, time_by = "3 days"),
                         seq(start2, end2, by = "3 days"))
  testthat::expect_equal(time_seq(start2, end2, time_by = "hour"),
                         seq(time_cast(start2, lubridate::origin),
                             time_cast(end2, lubridate::origin),
                             by = "hour"))
  testthat::expect_equal(time_seq(start2, end2, time_by = "min"),
                         seq(time_cast(start2, lubridate::origin),
                             time_cast(end2, lubridate::origin),
                             by = "min"))
  testthat::expect_equal(time_seq(start2, time_by = "day", length.out = 3),
                         seq(start2, by = "day", length.out = 3))
  # Extreme cases
  testthat::expect_equal(time_seq(start2, time_by = "day", length.out = 0),
                             lubridate::Date(0))
  testthat::expect_equal(time_seq(start2, time_by = "day", length.out = 1),
                             start2)
  testthat::expect_equal(time_seq(start2, time_by = "day", length.out = 0,
                                      time_type = "duration"),
                             lubridate::POSIXct(0))
  testthat::expect_equal(time_seq(start2, time_by = "day", length.out = 1,
                                  time_type = "duration"),
                         time_cast(start2, lubridate::origin))
  # testthat::expect_equal(time_seq(start2, time_by = "day", length.out = 1,
  #                                 time_type = "duration",
  #                                 tz = "Europe/London"),
  #                        lubridate::with_tz(start2, tzone = "Europe/London"))
  # When by isn't specified, the output may be POSIX even if from and to are dates
  testthat::expect_equal(time_seq(start2, end2, length.out = 0),
                             lubridate::POSIXct(0))
  testthat::expect_equal(time_seq(start2, end2, length.out = 0, time_type = "period"),
                         lubridate::Date(0))
  testthat::expect_equal(time_seq(start2, end2,  length.out = 1),
                         time_cast(start2, lubridate::origin))
  testthat::expect_equal(time_seq(start2, end2,  length.out = 0, time_type = "duration"),
                         lubridate::POSIXct(0))
  testthat::expect_equal(time_seq(start2, end2,  length.out = 1, time_type = "duration"),
                         time_cast(start2, lubridate::origin))
  # Special case where by calculates to 0 seconds, and so the output is a datetime.
  # This is likely a feature that will change in the timechange package
  testthat::expect_equal(time_seq(start2, start2, length.out = 3, time_type = "period"),
                             rep_len(lubridate::as_datetime(start2), 3))
  testthat::expect_equal(time_seq(start2, start2, length.out = 3, time_type = "duration"),
                         rep_len(lubridate::as_datetime(start2), 3))
  testthat::expect_equal(time_seq(end2, end2, length.out = 3, time_type = "duration"),
                         rep_len(lubridate::as_datetime(end2), 3))
  testthat::expect_equal(time_seq(start2, start2, time_type = "period", time_by = list("days" = 1)),
                             start2)
  testthat::expect_equal(time_seq(start2, start2, time_type = "duration", time_by = list("days" = 1)),
                         lubridate::as_datetime(start2))
  # Warning when 4 arguments supplied
  testthat::expect_warning(time_seq(start2, start2, length.out = 10, time_by = "days", time_type = "period"))
  testthat::expect_warning(time_seq(start2, start2, length.out = 10, time_by = "days", time_type = "duration"))
  # Error with too few arguments
  testthat::expect_error(time_seq(start2, time_by = "day"))
  testthat::expect_error(time_seq(to = end2, time_by = "day"))
  testthat::expect_error(time_seq(start2, time_by = "day"))
  testthat::expect_error(time_seq(start2, length.out = 5))
  testthat::expect_error(time_seq(to = start2, length.out = 5))
  # Error, cannot supply time_by = 0
  testthat::expect_error(time_seq(start2, end2, time_type = "period", time_by = list("days" = 0)))
  testthat::expect_error(time_seq(start2, end2, time_type = "duration", time_by = list("days" = 0)))
  # testthat::expect_error(time_seq(start2, start2, time_type = "period", time_by = list("days" = 0)))
  # testthat::expect_error(time_seq(start2, start2, time_type = "duration", time_by = list("days" = 0)))

  # from > to examples

  # Wrong result with wrong by sign, will upgrade this to error in the future
  # testthat::expect_error(time_seq(end2, start2, time_by = list("days" = -1)))
  testthat::expect_equal(time_seq(end2, start2, length.out = 11, time_type = "duration"),
                         seq.POSIXt(lubridate::as_datetime(end2),
                                    lubridate::as_datetime(start2), length.out = 11))

  testthat::expect_equal(time_seq(end2, start2, length.out = 11, time_type = "period"),
                             seq(end2, start2, length.out = 11))
  testthat::expect_equal(time_seq(end2, start2, time_by = list("days" = -1), time_type = "duration"),
                         seq.POSIXt(lubridate::as_datetime(end2),
                                    lubridate::as_datetime(start2), by = -86400))
  testthat::expect_equal(time_seq(end2, start2, time_by = list("days" = -1), time_type = "period"),
                         seq(end2, start2, by = -1))
})

testthat::test_that("Datetimes", {
  start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  testthat::expect_equal(time_seq(3, 100, time_by = 12.5),
                         seq(3, 100, by = 12.5))
  testthat::expect_equal(time_seq(3, length.out = 100, time_by = 2.5),
                         seq(3, length.out = 100, by = 2.5))
  testthat::expect_equal(time_seq(start1, end2, time_by = "day",
                                  time_type = "duration"),
                         seq(start1, time_cast(end2, start1), by = "day"))
  testthat::expect_equal(time_seq(16, 107, time_by = 7, time_floor = TRUE),
                         seq(time_floor(16, time_by = 7), 107, by = 7))
  testthat::expect_equal(time_seq(start1, start1 + lubridate::years(1),
                                  time_by = "3 weeks",
                                  time_type = "duration",
                                  week_start = 1,
                                  time_floor = TRUE),
                         seq(lubridate::floor_date(start1,
                                                   unit = "week",
                                                   week_start = 1),
                             start1 + lubridate::years(1), by = "3 weeks"))
  testthat::expect_equal(time_seq(start1, time_by = "week",
                                  length.out = 54,
                                  week_start = 1,
                                  time_floor = TRUE,
                                  time_type = "duration"),
                         seq(lubridate::floor_date(start1,
                                                   unit = "week",
                                                   week_start = 1),
                             length.out = 54, by = "week"))
  testthat::expect_equal(time_seq(to = max(seq(start1, length.out = 54, by = "week")),
                                  time_by = "week",
                                  length.out = 54,
                                  week_start = 1,
                                  time_floor = TRUE,
                                  time_type = "duration"),
                         seq(lubridate::floor_date(start1,
                                                   unit = "week",
                                                   week_start = 1),
                             length.out = 54, by = "week"))
  testthat::expect_warning(time_seq(start1, end1,
                                  length.out = 54,
                                  week_start = 1,
                                  time_floor = TRUE,
                                  time_type = "duration"))
  testthat::expect_equal(time_seq(end1, length.out = 10, time_by = "day", time_type = "duration"),
                         seq(end1, length.out = 10, by = "day"))
  testthat::expect_equal(time_seq(to = end1, length.out = 10, time_by = "day", time_type = "duration"),
                         seq(end1 - lubridate::ddays(9),
                             to = end1, by = "day"))
  testthat::expect_equal(time_seq(start2, end1, time_by = "day",
                                  time_type = "duration"),
                         seq(lubridate::as_datetime(start2, tz = lubridate::tz(end1)), end1, by = "day"))
  testthat::expect_length(time_seq(start1, end1, time_by = "day"), 11)
  testthat::expect_equal(time_seq(start1, end1, length.out = 11), # Result is datetime
                         lubridate::as_datetime(seq(start1, by = "day", length.out = 11)))
  testthat::expect_equal(time_seq(start1, end1, length.out = 22, time_type = "duration"),
                         seq(start1, end1, length.out = 22))
  testthat::expect_equal(time_seq(end1, start1, length.out = 22, time_type = "duration"),
                         seq(end1, start1, length.out = 22))
  testthat::expect_true(length(setdiff(time_seq(start1, end1, length.out = 33, time_type = "period"),
                          time_seq(start1, end1, length.out = 33, time_type = "duration"))) == 32)
  testthat::expect_equal(time_seq(start1, end1, time_by = "day", time_type = "duration"),
                             seq(start1, end1, by = "day"))
  testthat::expect_equal(time_seq(start1, end1,
                                  length.out = 10,
                                  time_type = "duration"),
                         seq(lubridate::as_datetime(start1),
                             lubridate::as_datetime(end1), length.out = 10))

  # Very basic tests
  ### DATETIMES ###
  testthat::expect_error(time_seq(start1, lubridate::POSIXct(0), time_by = "day"))
  testthat::expect_equal(time_seq(start1, end1, time_by = "day", time_type = "duration"),
                         seq(start1, end1, by = "day"))
  testthat::expect_equal(time_seq(start1, end1, time_by = "3 days", time_type = "duration"),
                         seq(start1, end1, by = "3 days"))
  testthat::expect_equal(time_seq(start1, end1, time_by = "hour"),
                         seq(start1,
                             end1,
                             by = "hour"))
  testthat::expect_equal(time_seq(start1, end1, time_by = "min"),
                         seq(start1, end1,
                             by = "min"))
  testthat::expect_equal(time_seq(start1, time_by = "day", length.out = 3,
                                  time_type = "duration"),
                         seq(start1, by = "day", length.out = 3))
  # Extreme cases
  testthat::expect_equal(time_seq(start1, time_by = "day", length.out = 0,
                                      time_type = "duration"),
                             lubridate::with_tz(lubridate::POSIXct(0), tz = "Europe/London"))
  testthat::expect_equal(time_seq(start1, time_by = "day", length.out = 1,
                                      time_type = "duration"),
                             start1)
  testthat::expect_equal(time_seq(start1, time_by = "day", length.out = 0,
                                  time_type = "duration"),
                         lubridate::with_tz(lubridate::POSIXct(0), tz = "Europe/London"))
  testthat::expect_equal(time_seq(start1, time_by = "day", length.out = 1,
                                  time_type = "duration"),
                         start1)
  # testthat::expect_equal(time_seq(start1, time_by = "day", length.out = 1,
  #                                 time_type = "duration",
  #                                 tz = "UTC"),
  #                        lubridate::with_tz(start1, tzone = "UTC"))
  # When by isn't specified, the output may be POSIX even if from and to are dates
  testthat::expect_equal(time_seq(start1, end1, length.out = 0),
                         lubridate::with_tz(lubridate::POSIXct(0), tz = "Europe/London"))
  testthat::expect_equal(time_seq(start1, end1, length.out = 0, time_type = "period"),
                         lubridate::with_tz(lubridate::POSIXct(0), tz = "Europe/London"))
  testthat::expect_equal(time_seq(start1, end1,  length.out = 1),
                         start1)
  testthat::expect_equal(time_seq(start1, end1,  length.out = 0, time_type = "duration"),
                         lubridate::with_tz(lubridate::POSIXct(0), tz = "Europe/London"))
  testthat::expect_equal(time_seq(start1, end1,  length.out = 1, time_type = "duration"),
                         start1)
  # Special case where by calculates to 0 seconds, and so the output is a datetime.
  # This is likely a feature that will change in the timechange package
  testthat::expect_equal(time_seq(start1, start1, length.out = 3, time_type = "period"),
                         rep_len(start1, 3))
  testthat::expect_equal(time_seq(start1, start1, length.out = 3, time_type = "duration"),
                         rep_len(start1, 3))
  testthat::expect_equal(time_seq(end1, end1, length.out = 3, time_type = "duration"),
                         rep_len(end1, 3))
  testthat::expect_equal(time_seq(start1, start1, time_type = "period", time_by = list("days" = 1)),
                             start1)
  testthat::expect_equal(time_seq(start1, start1, time_type = "duration", time_by = list("days" = 1)),
                         start1)
  # Warning when 4 arguments supplied
  testthat::expect_warning(time_seq(start1, start1, length.out = 10, time_by = "days", time_type = "period"))
  testthat::expect_warning(time_seq(start1, start1, length.out = 10, time_by = "days", time_type = "duration"))
  # Error with too few arguments
  testthat::expect_error(time_seq(start1, time_by = "day"))
  testthat::expect_error(time_seq(to = end1, time_by = "day"))
  testthat::expect_error(time_seq(start1, time_by = "day"))
  testthat::expect_error(time_seq(start1, length.out = 5))
  testthat::expect_error(time_seq(to = start1, length.out = 5))
  # Error, cannot supply time_by = 0
  testthat::expect_error(time_seq(end1, start1, time_type = "period", time_by = list("days" = 0)))
  testthat::expect_error(time_seq(end1, start1, time_type = "duration", time_by = list("days" = 0)))
  # testthat::expect_error(time_seq(start1, start1, time_type = "period", time_by = list("days" = 0)))
  # testthat::expect_error(time_seq(start1, start1, time_type = "duration", time_by = list("days" = 0)))

  # from > to examples

  # Wrong result with wrong by sign, will upgrade this to error in the future
  # testthat::expect_error(time_seq(end1, start1, time_by = list("days" = -1)))
  testthat::expect_equal(time_seq(end1, start1, length.out = 11, time_type = "duration"),
                         seq.POSIXt(end1,
                                    start1, length.out = 11))

  testthat::expect_equal(time_seq(end1, start1, length.out = 11, time_type = "duration"),
                         seq(end1, start1, length.out = 11))
  testthat::expect_equal(time_seq(end1, start1, time_by = list("days" = -1), time_type = "duration"),
                         seq.POSIXt(end1, start1, by = -86400))
  testthat::expect_equal(time_seq(end1, start1, time_by = list("days" = -1), time_type = "duration"),
                         seq(end1, start1, by = -86400))

  # Testing the vectorized period and duration sequence functions
  y1 <- period_seq_v(lubridate::today(), lubridate::today() + lubridate::days(50),
                     "days", c(2, rep(1, 112), rep(2, 154), 3, 6, 9))
  # y2 <- period_seq_v2(lubridate::today(), lubridate::today() + lubridate::days(50),
  #                     "days", c(2, rep(1, 112), rep(2, 154), 3, 6, 9))
  # y3 <- period_seq_v3(lubridate::today(), lubridate::today() + lubridate::days(50),
  #                     "days", c(2, rep(1, 112), rep(2, 154), 3, 6, 9))
  y4 <- lubridate::as_date(duration_seq_v(lubridate::today(), lubridate::today() + lubridate::days(50),
                                          "days", c(2, rep(1, 112), rep(2, 154), 3, 6, 9)))
  # testthat::expect_equal(y1, y2)
  # testthat::expect_equal(y1, y3)
  testthat::expect_equal(y1, y4)
  # testthat::expect_equal(y2, y4)
  # testthat::expect_equal(y2, y3)
  # testthat::expect_equal(y3, y4)
  testthat::expect_equal(time_seq_v(1, c(50, 100, 100), time_by = 2:4),
                         c(seq(1, 50, by = 2),
                           seq(1, 100, by = 3),
                           seq(1, 100, by = 4)))
})

testthat::test_that("Time sequence lengths", {
  start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(37)
  end2 <- start1 + lubridate::days(37)
  testthat::expect_equal(time_seq_sizes(lubridate::Date(0), lubridate::Date(0), time_by = "days", time_type = "period"),
                             integer(0))
  testthat::expect_equal(time_seq_sizes(lubridate::Date(0), lubridate::Date(0), time_by = "days", time_type = "duration"),
                             integer(0))
  testthat::expect_equal(time_seq_sizes(lubridate::Date(0), lubridate::Date(0), time_by = "days", time_type = "period"),
                             integer(0))
  testthat::expect_equal(time_seq_sizes(lubridate::POSIXct(0), lubridate::Date(0), time_by = "days", time_type = "duration"),
                             integer(0))
  testthat::expect_equal(time_seq_sizes(lubridate::POSIXct(0), lubridate::Date(0), time_by = "days", time_type = "period"),
                             integer(0))
  testthat::expect_equal(time_seq_sizes(lubridate::POSIXct(0), lubridate::POSIXct(0), time_by = "days", time_type = "duration"),
                             integer(0))
  testthat::expect_equal(time_seq_sizes(lubridate::POSIXct(0), lubridate::POSIXct(0), time_by = "days", time_type = "period"),
                             integer(0))
  testthat::expect_true(identical(time_seq_sizes(start1, end2, time_by = "days", time_type = "period"),
                                  38L))
  testthat::expect_equal(time_seq_sizes(start1, lubridate::POSIXct(0), time_by = "days", time_type = "period"),
                             integer(0))
  testthat::expect_equal(time_seq_sizes(start1, lubridate::Date(0), time_by = "days", time_type = "period"),
                             integer(0))
  testthat::expect_true(identical(time_seq_sizes(start1, end2, time_by = "days", time_type = "duration"),
                                  37L))
})
testthat::test_that("Vectorised time sequences", {
  start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(37)
  end2 <- start1 + lubridate::days(37)
  testthat::expect_equal(time_seq_v(lubridate::Date(0), lubridate::Date(0), time_by = "days", time_type = "period"),
                             lubridate::Date(0))
  testthat::expect_equal(time_seq_v(lubridate::Date(0), lubridate::Date(0), time_by = "days", time_type = "duration"),
                             lubridate::POSIXct(0))
  testthat::expect_equal(time_seq_v(lubridate::Date(0), lubridate::Date(0), time_by = "days", time_type = "period"),
                             lubridate::Date(0))
  testthat::expect_equal(time_seq_v(lubridate::POSIXct(0), lubridate::Date(0), time_by = "days", time_type = "duration"),
                             lubridate::POSIXct(0))
  testthat::expect_equal(time_seq_v(lubridate::POSIXct(0), lubridate::Date(0), time_by = "days", time_type = "period"),
                             lubridate::POSIXct(0))
  testthat::expect_equal(time_seq_v(lubridate::POSIXct(0), lubridate::POSIXct(0), time_by = "days", time_type = "duration"),
                             lubridate::POSIXct(0))
  testthat::expect_equal(time_seq_v(lubridate::POSIXct(0), lubridate::POSIXct(0), time_by = "days", time_type = "period"),
                             lubridate::POSIXct(0))
  testthat::expect_equal(time_seq_v(start1, end2, time_by = "days", time_type = "period"),
                                  time_seq(start1, end2, time_by = "days", time_type = "period"))
  testthat::expect_equal(time_seq_v(start1, end2, time_by = "days", time_type = "duration"),
                             time_seq(start1, end2, time_by = "days", time_type = "duration"))
  testthat::expect_equal(time_seq_v(start1, lubridate::POSIXct(0), time_by = "days", time_type = "period"),
                         lubridate::with_tz(lubridate::POSIXct(0), tzone = "Europe/London"))
  testthat::expect_equal(time_seq_v(start1, lubridate::Date(0), time_by = "days", time_type = "period"),
                         lubridate::with_tz(lubridate::POSIXct(0), tzone = "Europe/London"))
  testthat::expect_equal(time_seq_v(lubridate::as_date(start1), lubridate::Date(0), time_by = "days", time_type = "period"),
                         lubridate::Date(0))
  foo1 <- function(x) time_seq(from = start1, to = end1, time_by = list("days" = x), time_type = "duration")
  testthat::expect_equal(time_seq_v(start1, end1, time_by = list("days" = seq(0.5, 20, 0.5)), time_type = "duration"),
                         do.call(c, lapply(seq(0.5, 20, 0.5), foo1)))
  foo2 <- function(x) time_seq(from = start1, to = end1, time_by = list("days" = x), time_type = "period")
  testthat::expect_equal(time_seq_v(start1, end1, time_by = list("days" = seq(1, 20, 1)), time_type = "period"),
                         do.call(c, lapply(seq(1, 20, 1), foo2)))
})

testthat::test_that("ftseq compared to time_seq", {
  start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(37)
  end2 <- start1 + lubridate::days(37)
  # testthat::expect_equal(time_seq(start1, end1, time_by = "2 days", time_type = "period"),
  #                            ftseq(start1, end1, units = "days", num = 2, time_type = "period"))
  # testthat::expect_equal(time_seq(start1, end1, time_by = "2 days", time_type = "duration"),
  #                            ftseq(start1, end1, units = "days", num = 2, time_type = "duration"))
  # # Negative sign for time_seq() but not ftseq()
  # testthat::expect_equal(time_seq(end1, start1, time_by = "2 days", time_type = "duration"),
  #                            ftseq(end1, start1,  units = "days", num = -2, time_type = "duration"))
  #
  # testthat::expect_equal(time_seq(start1, end1,  time_by = "2 days"),
  #                            ftseq(start1, end1, units = "days", num = 2))
  # testthat::expect_equal(time_seq(start1, end1,  time_by = list("hours" = pi)),
  #                            ftseq(start1, end1, units = "hours", num = pi))
  # testthat::expect_equal(time_seq(start1, end1,  time_by = list("hours" = pi), tz = "America/New_York"),
  #                            lubridate::with_tz(ftseq(start1, end1, units = "hours", num = pi),
  #                                               "America/New_York"))
  # testthat::expect_equal(time_seq(start1, end1,  time_by = list("hours" = pi), tz = "America/New_York",
  #                                     time_floor = TRUE),
  #                            lubridate::with_tz(ftseq(start1, end1, units = "hours", num = pi,
  #                                  time_floor = TRUE), "America/New_York"))
  # testthat::expect_equal(time_seq(13.5, 194.75,  time_by = list("numeric" = 7),
  #                                     time_floor = FALSE),
  #                            ftseq(13.5, 194.75, units = "numeric", num = 7,
  #                                  time_floor = FALSE))
  # testthat::expect_equal(time_seq(13.5, 194.75,  time_by = 7,
  #                                     time_floor = FALSE),
  #                            ftseq(13.5, 194.75, units = "numeric", num = 7,
  #                                  time_floor = FALSE))
  # testthat::expect_equal(time_seq(13.5, 194.75,  time_by = 7,
  #                                     time_floor = TRUE),
  #                            ftseq(13.5, 194.75, units = "numeric", num = 7,
  #                                  time_floor = TRUE))
  testthat::expect_equal(time_seq(100, 5, time_by = 5),
                             seq(100, 5, by = -5))
  testthat::expect_equal(time_seq(100, 5, time_by = -5),
                             seq(100, 5, by = -5))

  testthat::expect_equal(time_seq(lubridate::Date(0), lubridate::Date(0), time_by = "days", time_type = "duration"),
                             lubridate::POSIXct(0))
  # testthat::expect_equal(ftseq(lubridate::Date(0), lubridate::Date(0), units = "days", time_type = "duration"),
  #                            lubridate::POSIXct(0))
  testthat::expect_equal(time_seq(lubridate::Date(0), lubridate::Date(0), time_by = "days", time_type = "period"),
                         lubridate::Date(0))
  # testthat::expect_equal(ftseq(lubridate::Date(0), lubridate::Date(0), units = "days", time_type = "period"),
  #                        lubridate::Date(0))
})

testthat::test_that("dates, datetimes and numeric increments", {
  start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)

  testthat::expect_equal(time_seq_v(start2, end2,
                                    time_by = "3 days"),
                         time_seq_v(start2, end2,
                                    time_by = 3))
  testthat::expect_equal(time_seq_v(start1, end1,
                                    time_by = "300 seconds"),
                         time_seq_v(start1, end1,
                                    time_by = 300))
  testthat::expect_equal(time_seq_v(start1, end2,
                                    time_by = "338.5 seconds"),
                         time_seq_v(start1, end2,
                                    time_by = 338.5))
  testthat::expect_equal(time_seq_v(start2, end1,
                                    time_by = "338.5 seconds"),
                         time_seq_v(start2, end1,
                                    time_by = 338.5))

})
