
testthat::test_that("Dates", {
  start1 <- lubridate::now()
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  # Dates
  testthat::expect_length(time_seq(start2, end2, by = "day"), 11)
  testthat::expect_equal(time_seq(start2, end2, length.out = 11), # Result is datetime
                          lubridate::as_datetime(seq(start2, by = "day", length.out = 11)))
  testthat::expect_error(time_seq(start2, end2, length.out = 22, seq_type = "period"))
  testthat::expect_error(time_seq(start2, end2, length.out = 10,
                                  seq_type = "period"))
  testthat::expect_identical(time_seq(start2, end2, by = "day"),
                             seq(start2, end2, by = "day"))
  testthat::expect_equal(time_seq(start2, end2,
                                      length.out = 10,
                                      seq_type = "duration"),
                             seq(lubridate::as_datetime(start2),
                                 lubridate::as_datetime(end2), length.out = 10))

  # Very basic tests
  ### DATES ###
  testthat::expect_equal(time_seq(start2, end2, by = "day"),
                         seq(start2, end2, by = "day"))
  testthat::expect_equal(time_seq(start2, end2, by = "3 days"),
                         seq(start2, end2, by = "3 days"))
  testthat::expect_equal(time_seq(start2, end2, by = "hour"),
                         seq(time_cast(start2, lubridate::origin),
                             time_cast(end2, lubridate::origin),
                             by = "hour"))
  testthat::expect_equal(time_seq(start2, end2, by = "min"),
                         seq(time_cast(start2, lubridate::origin),
                             time_cast(end2, lubridate::origin),
                             by = "min"))
  testthat::expect_equal(time_seq(start2, by = "day", length.out = 3),
                         seq(start2, by = "day", length.out = 3))
  # Extreme cases
  testthat::expect_identical(time_seq(start2, by = "day", length.out = 0),
                             lubridate::Date(0))
  testthat::expect_identical(time_seq(start2, by = "day", length.out = 1),
                             start2)
  testthat::expect_equal(time_seq(start2, by = "day", length.out = 0,
                                      seq_type = "duration"),
                             lubridate::POSIXct(0))
  testthat::expect_equal(time_seq(start2, by = "day", length.out = 1,
                                  seq_type = "duration"),
                         time_cast(start2, lubridate::origin))
  testthat::expect_equal(time_seq(start2, by = "day", length.out = 1,
                                  seq_type = "duration",
                                  tz = "GB"),
                         lubridate::with_tz(start2, tzone = "GB"))
  # When by isn't specified, the output may be POSIX even if from and to are dates
  testthat::expect_equal(time_seq(start2, end2, length.out = 0),
                             lubridate::POSIXct(0))
  testthat::expect_equal(time_seq(start2, end2, length.out = 0, seq_type = "period"),
                         lubridate::Date(0))
  testthat::expect_equal(time_seq(start2, end2,  length.out = 1),
                         time_cast(start2, lubridate::origin))
  testthat::expect_equal(time_seq(start2, end2,  length.out = 0, seq_type = "duration"),
                         lubridate::POSIXct(0))
  testthat::expect_equal(time_seq(start2, end2,  length.out = 1, seq_type = "duration"),
                         time_cast(start2, lubridate::origin))
  # Special case where by calculates to 0 seconds, and so the output is a datetime.
  # This is likely a feature that will change in the timechange package
  testthat::expect_equal(time_seq(start2, start2, length.out = 3, seq_type = "period"),
                             rep_len(lubridate::as_datetime(start2), 3))
  testthat::expect_equal(time_seq(start2, start2, length.out = 3, seq_type = "duration"),
                         rep_len(lubridate::as_datetime(start2), 3))
  testthat::expect_equal(time_seq(end2, end2, length.out = 3, seq_type = "duration"),
                         rep_len(lubridate::as_datetime(end2), 3))
  testthat::expect_identical(time_seq(start2, start2, seq_type = "period", by = list("days" = 1)),
                             start2)
  testthat::expect_equal(time_seq(start2, start2, seq_type = "duration", by = list("days" = 1)),
                         lubridate::as_datetime(start2))
  # Warning when 4 arguments supplied
  testthat::expect_warning(time_seq(start2, start2, length.out = 10, by = "days", seq_type = "period"))
  testthat::expect_warning(time_seq(start2, start2, length.out = 10, by = "days", seq_type = "duration"))
  # Error with too few arguments
  testthat::expect_error(time_seq(start2, by = "day"))
  testthat::expect_error(time_seq(to = end2, by = "day"))
  testthat::expect_error(time_seq(start2, by = "day"))
  testthat::expect_error(time_seq(start2, length.out = 5))
  testthat::expect_error(time_seq(to = start2, length.out = 5))
  # Error, cannot supply by = 0
  testthat::expect_error(time_seq(start2, start2, seq_type = "period", by = list("days" = 0)))
  testthat::expect_error(time_seq(start2, start2, seq_type = "duration", by = list("days" = 0)))

  # from > to examples

  # Wrong result with wrong by sign, will upgrade this to error in the future
  # testthat::expect_error(time_seq(end2, start2, by = list("days" = -1)))
  testthat::expect_equal(time_seq(end2, start2, length.out = 11, seq_type = "duration"),
                         seq.POSIXt(lubridate::as_datetime(end2),
                                    lubridate::as_datetime(start2), length.out = 11))

  testthat::expect_equal(time_seq(end2, start2, length.out = 11, seq_type = "period"),
                             seq(end2, start2, length.out = 11))
  testthat::expect_equal(time_seq(end2, start2, by = list("days" = -1), seq_type = "duration"),
                         seq.POSIXt(lubridate::as_datetime(end2),
                                    lubridate::as_datetime(start2), by = -86400))
  testthat::expect_equal(time_seq(end2, start2, by = list("days" = -1), seq_type = "period"),
                         seq(end2, start2, by = -1))
})

testthat::test_that("Datetimes", {
  start1 <- lubridate::now()
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  # Dates
  testthat::expect_length(time_seq(start1, end1, by = "day"), 11)
  testthat::expect_equal(time_seq(start1, end1, length.out = 11), # Result is datetime
                         lubridate::as_datetime(seq(start1, by = "day", length.out = 11)))
  testthat::expect_error(time_seq(start1, end1, length.out = 22, seq_type = "period"))
  testthat::expect_error(time_seq(start1, end1, length.out = 10,
                                  seq_type = "period"))
  testthat::expect_identical(time_seq(start1, end1, by = "day"),
                             seq(start1, end1, by = "day"))
  testthat::expect_equal(time_seq(start1, end1,
                                  length.out = 10,
                                  seq_type = "duration"),
                         seq(lubridate::as_datetime(start1),
                             lubridate::as_datetime(end1), length.out = 10))

  # Very basic tests
  ### DATETIMES ###
  testthat::expect_equal(time_seq(start1, end1, by = "day"),
                         seq(start1, end1, by = "day"))
  testthat::expect_equal(time_seq(start1, end1, by = "3 days"),
                         seq(start1, end1, by = "3 days"))
  testthat::expect_equal(time_seq(start1, end1, by = "hour"),
                         seq(start1,
                             end1,
                             by = "hour"))
  testthat::expect_equal(time_seq(start1, end1, by = "min"),
                         seq(start1, end1,
                             by = "min"))
  testthat::expect_equal(time_seq(start1, by = "day", length.out = 3),
                         seq(start1, by = "day", length.out = 3))
  # Extreme cases
  testthat::expect_identical(time_seq(start1, by = "day", length.out = 0),
                             lubridate::with_tz(lubridate::POSIXct(0), tz = ""))
  testthat::expect_identical(time_seq(start1, by = "day", length.out = 1),
                             start1)
  testthat::expect_equal(time_seq(start1, by = "day", length.out = 0,
                                  seq_type = "duration"),
                         lubridate::with_tz(lubridate::POSIXct(0), tz = ""))
  testthat::expect_equal(time_seq(start1, by = "day", length.out = 1,
                                  seq_type = "duration"),
                         start1)
  testthat::expect_equal(time_seq(start1, by = "day", length.out = 1,
                                  seq_type = "duration",
                                  tz = "GB"),
                         lubridate::with_tz(start1, tzone = "GB"))
  # When by isn't specified, the output may be POSIX even if from and to are dates
  testthat::expect_equal(time_seq(start1, end1, length.out = 0),
                         lubridate::with_tz(lubridate::POSIXct(0), tz = ""))
  testthat::expect_equal(time_seq(start1, end1, length.out = 0, seq_type = "period"),
                         lubridate::with_tz(lubridate::POSIXct(0), tz = ""))
  testthat::expect_equal(time_seq(start1, end1,  length.out = 1),
                         start1)
  testthat::expect_equal(time_seq(start1, end1,  length.out = 0, seq_type = "duration"),
                         lubridate::with_tz(lubridate::POSIXct(0), tz = ""))
  testthat::expect_equal(time_seq(start1, end1,  length.out = 1, seq_type = "duration"),
                         start1)
  # Special case where by calculates to 0 seconds, and so the output is a datetime.
  # This is likely a feature that will change in the timechange package
  testthat::expect_equal(time_seq(start1, start1, length.out = 3, seq_type = "period"),
                         rep_len(start1, 3))
  testthat::expect_equal(time_seq(start1, start1, length.out = 3, seq_type = "duration"),
                         rep_len(start1, 3))
  testthat::expect_equal(time_seq(end1, end1, length.out = 3, seq_type = "duration"),
                         rep_len(end1, 3))
  testthat::expect_identical(time_seq(start1, start1, seq_type = "period", by = list("days" = 1)),
                             start1)
  testthat::expect_equal(time_seq(start1, start1, seq_type = "duration", by = list("days" = 1)),
                         start1)
  # Warning when 4 arguments supplied
  testthat::expect_warning(time_seq(start1, start1, length.out = 10, by = "days", seq_type = "period"))
  testthat::expect_warning(time_seq(start1, start1, length.out = 10, by = "days", seq_type = "duration"))
  # Error with too few arguments
  testthat::expect_error(time_seq(start1, by = "day"))
  testthat::expect_error(time_seq(to = end1, by = "day"))
  testthat::expect_error(time_seq(start1, by = "day"))
  testthat::expect_error(time_seq(start1, length.out = 5))
  testthat::expect_error(time_seq(to = start1, length.out = 5))
  # Error, cannot supply by = 0
  testthat::expect_error(time_seq(start1, start1, seq_type = "period", by = list("days" = 0)))
  testthat::expect_error(time_seq(start1, start1, seq_type = "duration", by = list("days" = 0)))

  # from > to examples

  # Wrong result with wrong by sign, will upgrade this to error in the future
  # testthat::expect_error(time_seq(end1, start1, by = list("days" = -1)))
  testthat::expect_equal(time_seq(end1, start1, length.out = 11, seq_type = "duration"),
                         seq.POSIXt(end1,
                                    start1, length.out = 11))

  testthat::expect_equal(time_seq(end1, start1, length.out = 11, seq_type = "period"),
                         seq(end1, start1, length.out = 11))
  testthat::expect_equal(time_seq(end1, start1, by = list("days" = -1), seq_type = "duration"),
                         seq.POSIXt(end1, start1, by = -86400))
  testthat::expect_equal(time_seq(end1, start1, by = list("days" = -1), seq_type = "period"),
                         seq(end1, start1, by = -86400))

  # Testing the vectorized period and duration sequence functions
  y1 <- period_seq_v(lubridate::today(), lubridate::today() + lubridate::days(50),
                     "days", c(2, rep(1, 112), rep(2, 154), 3, 6, 9))
  y2 <- period_seq_v2(lubridate::today(), lubridate::today() + lubridate::days(50),
                      "days", c(2, rep(1, 112), rep(2, 154), 3, 6, 9))
  y3 <- period_seq_v3(lubridate::today(), lubridate::today() + lubridate::days(50),
                      "days", c(2, rep(1, 112), rep(2, 154), 3, 6, 9))
  y4 <- lubridate::as_date(duration_seq_v(lubridate::today(), lubridate::today() + lubridate::days(50),
                                          "days", c(2, rep(1, 112), rep(2, 154), 3, 6, 9)))
  testthat::expect_identical(y1, y2)
  testthat::expect_identical(y1, y3)
  testthat::expect_identical(y1, y4)
  testthat::expect_identical(y2, y4)
  testthat::expect_identical(y2, y3)
  testthat::expect_identical(y3, y4)
})
#
# # # Dates
# time_seq(start2, end2, by = "day")
# time_seq(start2, end2, by = "hour")
# time_seq(start2, end2, by = "min")
# time_seq(start2, end2, by = "3 days")
# time_seq(start2, by = "day", length.out = 3)
# # Extreme cases
# time_seq(start2, by = "day", length.out = 0)
# time_seq(start2, by = "day", length.out = 0)
# time_seq(start2, by = "day", length.out = 1)
# time_seq(start2, by = "day", length.out = 0, seq_type = "duration")
# time_seq(start2, by = "day", length.out = 1, seq_type = "duration")
# time_seq(start2, by = "day", length.out = 1)
# time_seq(start2, end2,  length.out = 0)
# time_seq(start2, end2,  length.out = 1)
# time_seq(start2, end2,  length.out = 0, seq_type = "duration")
# time_seq(start2, end2,  length.out = 1, seq_type = "duration")
# time_seq(start2, start2, length.out = 3)
# time_seq(start2, start2, length.out = 3, seq_type = "duration")
# time_seq(end2, end2, length.out = 3)
#
# time_seq(start2, by = "day", length.out = 3)
# time_seq(to = end2, by = "day", length.out = 3)
# time_seq(to = end2, length.out = 3)
# time_seq(start2, end2, length.out = 3)
# time_seq(start2, end2, by = "day", length.out = 2) # Error
# time_seq(start2, by = "day") # Error
# time_seq(to = end2, by = "day") # Error
#
# # Datetimes
# time_seq(start1, end1) # Error
# time_seq(start1, end1, length.out = 22)
# time_seq(start1, end1, by = "day")
# time_seq(start1, end1, length.out = 241)
# time_seq(start1, end1, by = "hour")
# time_seq(start1, end1, by = "min")
# time_seq(start1, end1, length.out = length(time_seq(start1, end1, by = "min")))
# time_seq(start1, end1, by = "3 days")
# time_seq(start1, end1, by = "day", length.out = 2) # Error
# time_seq(start1, length.out = 3) # Error
# time_seq(start1, by = "day", length.out = 3)
# time_seq(start1, start1, length.out = 3)
# time_seq(to = end1, by = "day", length.out = 3)
# time_seq(to = end1, length.out = 3) # Error
# time_seq(start1, start1, length.out = 3)
# time_seq(end1, end1, length.out = 3)
#
# # Mix of dates and Datetimes
# time_seq(start2, end1) # Error
# time_seq(start1, end2) # Error
# time_seq(start1, end2, length.out = 22)
# time_seq(start2, end1, length.out = 22)
# time_seq(start1, end2, by = "day")
# time_seq(start2, end1, by = "day")
# time_seq(start1, end2, by = "hour")
# time_seq(start2, end1, by = "hour")
# time_seq(start1, end2, by = "min")
# time_seq(start2, end1, by = "min")
# time_seq(start1, end2, by = "3 days")
# time_seq(start2, end1, by = "3 days")
# time_seq(start1, length.out = 3) # Error
# time_seq(start1, by = "day", length.out = 3)
# time_seq(start1, by = "hour", length.out = 3)
# time_seq(to = end1, by = "day", length.out = 3)
# time_seq(to = end2, by = "day", length.out = 3)
# time_seq(to = end1, by = "hour", length.out = 3)
# time_seq(to = end2, by = "hour", length.out = 3)
# time_seq(to = end1, length.out = 3) # Error
# time_seq(start1, end1, length.out = 3)
# time_seq(start1, end2, length.out = 3)
# time_seq(start2, end1, length.out = 3)
