# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("Dates", {
  start1 <- lubridate::now()
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  # Dates
  expect_length(time_seq(start2, end2, time = "day"), 11)
  expect_equal(
    time_seq(start2, end2, length.out = 11), # Result is datetime
    seq(start2, by = "day", length.out = 11)
  )
  expect_equal(
    time_seq(start2, end2, time = "day"),
    seq(start2, end2, by = "day")
  )
  expect_equal(
    time_seq(start2, end2,
      length.out = 10,

    ),
    seq(lubridate::as_datetime(start2),
      lubridate::as_datetime(end2),
      length.out = 10
    )
  )
  # Test for month arithmetic when to is at the end of a month
  # This may be unexpected but at the moment, emphasis is always placed on a start point of any seq
  expect_equal(
    time_seq(to = lubridate::ymd("1946-01-31"), length.out = 2, time = "9 months"),
    lubridate::ymd(c("1945-04-30", "1946-01-30"))
  )

  # Very basic tests
  ### DATES ###
  expect_equal(
    time_seq(lubridate::Date(0), start2, time = "day"),
    lubridate::Date(0)
  )
  expect_equal(
    time_seq(lubridate::Date(0), start2, time = "day"),
    lubridate::Date(0)
  )
  expect_equal(
    time_seq(lubridate::POSIXct(0), start2, time = "day"),
    lubridate::POSIXct(0)
  )
  expect_equal(time_seq(start2, lubridate::Date(0), time = "day"), .Date(numeric()))
  expect_equal(time_seq(start2, lubridate::Date(0), time = "day"), .Date(numeric()))
  expect_equal(time_seq(start2, lubridate::POSIXct(0), time = "day"), lubridate::POSIXct())

  expect_equal(
    time_seq(start2, end2, time = "day"),
    seq(start2, end2, by = "day")
  )
  expect_equal(
    time_seq(start2, end2, time = "3 days"),
    seq(start2, end2, by = "3 days")
  )
  expect_equal(
    time_seq(start2, end2, time = "hour"),
    seq(time_cast(start2, lubridate::origin),
      time_cast(end2, lubridate::origin),
      by = "hour"
    )
  )
  expect_equal(
    time_seq(start2, end2, time = "min"),
    seq(time_cast(start2, lubridate::origin),
      time_cast(end2, lubridate::origin),
      by = "min"
    )
  )
  expect_equal(
    time_seq(start2, time = "day", length.out = 3),
    seq(start2, by = "day", length.out = 3)
  )
  # Extreme cases
  expect_equal(
    time_seq(start2, time = "day", length.out = 0),
    lubridate::Date(0)
  )
  expect_equal(
    time_seq(start2, time = "day", length.out = 1),
    start2
  )
  expect_equal(
    time_seq(start2,
      time = "day", length.out = 0
    ),
    lubridate::Date(0)
  )
  expect_equal(
    time_seq(start2,
      time = "day", length.out = 1
    ),
    start2
  )
  expect_equal(
    time_seq(start2, end2, length.out = 0),
    lubridate::Date(0)
  )
  expect_equal(
    time_seq(start2, end2, length.out = 1),
    start2
  )
  expect_equal(
    time_seq(start2, start2, length.out = 3),
    rep_len(start2, 3)
  )
  expect_equal(
    time_seq(end2, end2, length.out = 3),
    rep_len(end2, 3)
  )
  expect_equal(
    time_seq(start2, start2, 1),
    start2
  )
  expect_equal(
    time_seq(start2, start2, 1),
    start2
  )
  # Warning when 4 arguments supplied
  expect_warning(time_seq(start2, start2, length.out = 10, time = "days"))
  expect_warning(time_seq(start2, start2, length.out = 10, time = "days"))
  # Error with too few arguments
  expect_error(time_seq(start2, time = "day"))
  expect_error(time_seq(to = end2, time = "day"))
  expect_error(time_seq(start2, time = "day"))
  expect_error(time_seq(start2, length.out = 5))
  expect_error(time_seq(to = start2, length.out = 5))
  # Error, cannot supply time = 0
  expect_error(time_seq(start2, end2 = 0))
  expect_error(time_seq(start2, end2 = 0))
  expect_equal(
    time_seq(start2, start2, 0),
    start2
  )

  ################### EXTRA CHECKS FOR UBSAN CLANG SANITIZERS

  expect_error(suppressWarnings(time_seq_v(start2, end2, time = "0 days")))
  expect_error(
    suppressWarnings(
      time_seq_v(start2, end2, time = timespan("days", 0))
    )
  )
  expect_error(suppressWarnings(time_seq_v(start2, end2, time = 0)))
  expect_error(suppressWarnings(time_seq(start2, end2, time = 0)))

  # Wrong result with wrong by sign, will upgrade this to error in the future
  # expect_error(time_seq(end2, start2, time = list("days" = -1)))
  expect_equal(
    time_seq(end2, start2, length.out = 11),
    seq(end2, start2, length.out = 11)
  )
  expect_equal(
    time_seq(end2, start2, timespan("days", -1)),
    seq(end2, start2, by = -1)
  )
})

test_that("Datetimes", {
  start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
    tz = "Europe/London"
  )
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  expect_equal(
    time_seq(3, 100, time = 12.5),
    seq(3, 100, by = 12.5)
  )
  expect_equal(
    time_seq(3, length.out = 100, time = 2.5),
    seq(3, length.out = 100, by = 2.5)
  )
  expect_equal(
    time_seq(start1, end2,
      time = "day",
    ),
    seq(start1, time_cast(end2, start1), by = "day")
  )
  expect_equal(
    time_seq(end1, length.out = 10, time = "day"),
    seq(end1, length.out = 10, by = "day")
  )
  expect_equal(
    time_seq(to = end1, length.out = 10, time = "day"),
    end1 + lubridate::days(-9:0)
  )
  expect_equal(
    time_seq(to = end1, length.out = 10, time = "86400 seconds"),
    end1 + lubridate::ddays(-9:0)
  )

  expect_equal(
    time_seq(start2, end1,
      time = "day"
    ),
    seq(lubridate::as_datetime(start2, tz = lubridate::tz(end1)), end1, by = "day")
  )
  expect_length(time_seq(start1, end1, time = "day"), 11)
  expect_equal(
    time_seq(start1, end1, length.out = 11), # Result is datetime
    lubridate::as_datetime(seq(start1, by = "day", length.out = 11))
  )
  expect_equal(
    time_seq(start1, end1, length.out = 22),
    seq(start1, end1, length.out = 22)
  )
  expect_equal(
    time_seq(end1, start1, length.out = 22),
    seq(end1, start1, length.out = 22)
  )
  expect_equal(
    time_seq(start1, end1, length.out = 33),
    seq(start1, end1, length.out = 33)
  )
  expect_equal(
    time_seq(start1, end1, time = timespan("seconds", 86400)),
    seq(start1, end1, by = "day")
  )
  expect_equal(
    time_seq(start1, end1,
      length.out = 10,

    ),
    seq(lubridate::as_datetime(start1),
      lubridate::as_datetime(end1),
      length.out = 10
    )
  )

  # Very basic tests
  ### DATETIMES ###
  expect_equal(time_seq(start1, lubridate::POSIXct(0), time = "day"), lubridate::POSIXct(tz = "Europe/London"))
  expect_equal(
    time_seq(start1, end1, time = "day"),
    time_add(start1, timespan("days", 0:10))
  )
  expect_equal(
    time_seq(start1, end1, time = "3 days"),
    seq(start1, end1, by = "3 days")
  )
  expect_equal(
    time_seq(start1, end1, time = "hour"),
    seq(start1,
      end1,
      by = "hour"
    )
  )
  expect_equal(
    time_seq(start1, end1, time = "min"),
    seq(start1, end1,
      by = "min"
    )
  )
  expect_equal(
    time_seq(start1,
      time = "day", length.out = 3,

    ),
    seq(start1, by = "day", length.out = 3)
  )
  # Extreme cases
  expect_equal(
    time_seq(start1,
      time = "day", length.out = 0,
    ),
    lubridate::with_tz(lubridate::POSIXct(0), tz = "Europe/London")
  )
  expect_equal(
    time_seq(start1,
      time = "day", length.out = 1,
    ),
    start1
  )
  expect_equal(
    time_seq(start1,
      time = "day", length.out = 0,
    ),
    lubridate::with_tz(lubridate::POSIXct(0), tz = "Europe/London")
  )
  expect_equal(
    time_seq(start1,
      time = "day", length.out = 1,

    ),
    start1
  )
  # When by isn't specified, the output may be POSIX even if from and to are dates
  expect_equal(
    time_seq(start1, end1, length.out = 0),
    lubridate::with_tz(lubridate::POSIXct(0), tz = "Europe/London")
  )
  expect_equal(
    time_seq(start1, end1, length.out = 0),
    lubridate::with_tz(lubridate::POSIXct(0), tz = "Europe/London")
  )
  expect_equal(
    time_seq(start1, end1, length.out = 1),
    start1
  )
  expect_equal(
    time_seq(start1, end1, length.out = 0),
    lubridate::with_tz(lubridate::POSIXct(0), tz = "Europe/London")
  )
  expect_equal(
    time_seq(start1, end1, length.out = 1),
    start1
  )
  # Special case where by calculates to 0 seconds, and so the output is a datetime.
  # This is likely a feature that will change in the timechange package
  expect_equal(
    time_seq(start1, start1, length.out = 3),
    rep_len(start1, 3)
  )
  expect_equal(
    time_seq(start1, start1, length.out = 3),
    rep_len(start1, 3)
  )
  expect_equal(
    time_seq(end1, end1, length.out = 3),
    rep_len(end1, 3)
  )
  # expect_equal(
  #   time_seq(start1, start1 = 1),
  #   start1
  # )
  # expect_equal(
  #   time_seq(start1, start1 = 1),
  #   start1
  # )
  # Warning when 4 arguments supplied
  expect_warning(time_seq(start1, start1, length.out = 10, time = "days"))
  expect_warning(time_seq(start1, start1, length.out = 10, time = "days"))
  # Error with too few arguments
  expect_error(time_seq(start1, time = "day"))
  expect_error(time_seq(to = end1, time = "day"))
  expect_error(time_seq(start1, time = "day"))
  expect_error(time_seq(start1, length.out = 5))
  expect_error(time_seq(to = start1, length.out = 5))
  # Error, cannot supply time = 0
  # expect_error(time_seq(end1, start1 = 0)))
  # expect_error(time_seq(end1, start1 = 0)))
  # expect_equal(
  #   time_seq(start1, start1 = 0),
  #   start1
  # )
  # expect_equal(
  #   time_seq(start1, start1 = 0),
  #   start1
  # )

  # from > to examples

  # Wrong result with wrong by sign, will upgrade this to error in the future
  # expect_error(time_seq(end1, start1, time = list("days" = -1)))
  expect_equal(
    time_seq(end1, start1, length.out = 11),
    seq.POSIXt(end1,
      start1,
      length.out = 11
    )
  )

  expect_equal(
    time_seq(end1, start1, length.out = 11),
    seq(end1, start1, length.out = 11)
  )
  expect_equal(
    time_seq(end1, start1, time = timespan("days", -1)),
    time_subtract(end1, timespan("days", 0:9))
  )
  expect_equal(
    time_seq(end1, start1, time = timespan("hours", -24)),
    seq(end1, start1, by = -86400)
  )

  # Testing the vectorized period and duration sequence functions
  y1 <- period_seq_v(
    lubridate::today(), lubridate::today() + lubridate::days(50),
    "days", c(2, rep(1, 112), rep(2, 154), 3, 6, 9)
  )
  y4 <- lubridate::as_date(duration_seq_v(
    lubridate::today(), lubridate::today() + lubridate::days(50),
    "days", c(2, rep(1, 112), rep(2, 154), 3, 6, 9)
  ))
  expect_equal(y1, y4)
  expect_equal(
    time_seq_v(1, c(50, 100, 100), time = 2:4),
    c(
      seq(1, 50, by = 2),
      seq(1, 100, by = 3),
      seq(1, 100, by = 4)
    )
  )
})
#
# test_that("Time sequence lengths", {
#   start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
#     tz = "Europe/London"
#   )
#   end1 <- start1 + lubridate::ddays(37)
#   end2 <- start1 + lubridate::days(37)
#   expect_equal(
#     time_seq_sizes(lubridate::Date(0), lubridate::Date(0), time = "days"),
#     integer(0)
#   )
#   expect_equal(
#     time_seq_sizes(lubridate::Date(0), lubridate::Date(0), time = "days"),
#     integer(0)
#   )
#   expect_equal(
#     time_seq_sizes(lubridate::Date(0), lubridate::Date(0), time = "days"),
#     integer(0)
#   )
#   expect_equal(
#     time_seq_sizes(lubridate::POSIXct(0), lubridate::Date(0), time = "days"),
#     integer(0)
#   )
#   expect_equal(
#     time_seq_sizes(lubridate::POSIXct(0), lubridate::Date(0), time = "days"),
#     integer(0)
#   )
#   expect_equal(
#     time_seq_sizes(lubridate::POSIXct(0), lubridate::POSIXct(0), time = "days"),
#     integer(0)
#   )
#   expect_equal(
#     time_seq_sizes(lubridate::POSIXct(0), lubridate::POSIXct(0), time = "days"),
#     integer(0)
#   )
#   expect_true(identical(
#     time_seq_sizes(start1, end2, time = "days"),
#     38L
#   ))
#   expect_equal(
#     time_seq_sizes(start1, lubridate::POSIXct(0), time = "days"),
#     integer(0)
#   )
#   expect_equal(
#     time_seq_sizes(start1, lubridate::Date(0), time = "days"),
#     integer(0)
#   )
#   expect_true(identical(
#     time_seq_sizes(start1, end2, time = "days"),
#     37L
#   ))
# })
# test_that("Vectorised time sequences", {
#   start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
#     tz = "Europe/London"
#   )
#   end1 <- start1 + lubridate::ddays(37)
#   end2 <- start1 + lubridate::days(37)
#   expect_equal(
#     time_seq_v(lubridate::Date(0), lubridate::Date(0), time = "days"),
#     lubridate::Date(0)
#   )
#   expect_equal(
#     time_seq_v(lubridate::Date(0), lubridate::Date(0), time = "days"),
#     lubridate::POSIXct(0)
#   )
#   expect_equal(
#     time_seq_v(lubridate::Date(0), lubridate::Date(0), time = "days"),
#     lubridate::Date(0)
#   )
#   expect_equal(
#     time_seq_v(lubridate::POSIXct(0), lubridate::Date(0), time = "days"),
#     lubridate::POSIXct(0)
#   )
#   expect_equal(
#     time_seq_v(lubridate::POSIXct(0), lubridate::Date(0), time = "days"),
#     lubridate::POSIXct(0)
#   )
#   expect_equal(
#     time_seq_v(lubridate::POSIXct(0), lubridate::POSIXct(0), time = "days"),
#     lubridate::POSIXct(0)
#   )
#   expect_equal(
#     time_seq_v(lubridate::POSIXct(0), lubridate::POSIXct(0), time = "days"),
#     lubridate::POSIXct(0)
#   )
#   expect_equal(
#     time_seq_v(start1, end2, time = "days"),
#     time_seq(start1, end2, time = "days")
#   )
#   expect_equal(
#     time_seq_v(start1, end2, time = "days"),
#     time_seq(start1, end2, time = "days")
#   )
#   expect_equal(
#     time_seq_v(start1, lubridate::POSIXct(0), time = "days"),
#     lubridate::with_tz(lubridate::POSIXct(0), tzone = "Europe/London")
#   )
#   expect_equal(
#     time_seq_v(start1, lubridate::Date(0), time = "days"),
#     lubridate::with_tz(lubridate::POSIXct(0), tzone = "Europe/London")
#   )
#   expect_equal(
#     time_seq_v(lubridate::as_date(start1), lubridate::Date(0), time = "days"),
#     lubridate::Date(0)
#   )
#   foo1 <- function(x) time_seq(from = start1, to = end1, time = list("days" = x))
#   expect_equal(
#     time_seq_v(start1, end1, time = list("days" = seq(0.5, 20, 0.5))),
#     do.call(c, lapply(seq(0.5, 20, 0.5), foo1))
#   )
#   foo2 <- function(x) time_seq(from = start1, to = end1, time = list("days" = x))
#   expect_equal(
#     time_seq_v(start1, end1, time = list("days" = seq(1, 20, 1))),
#     do.call(c, lapply(seq(1, 20, 1), foo2))
#   )
# })
# #
# test_that("ftseq compared to time_seq", {
#   start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
#     tz = "Europe/London"
#   )
#   end1 <- start1 + lubridate::ddays(37)
#   end2 <- start1 + lubridate::days(37)
#   expect_equal(
#     time_seq(100, 5, time = 5),
#     seq(100, 5, by = -5)
#   )
#   expect_equal(
#     time_seq(100, 5, time = -5),
#     seq(100, 5, by = -5)
#   )
#
#   expect_equal(
#     time_seq(lubridate::Date(0), lubridate::Date(0), time = "days"),
#     lubridate::POSIXct(0)
#   )
#   expect_equal(
#     time_seq(lubridate::Date(0), lubridate::Date(0), time = "days"),
#     lubridate::Date(0)
#   )
# })
#
# test_that("dates, datetimes and numeric increments", {
#   start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
#     tz = "Europe/London"
#   )
#   end1 <- start1 + lubridate::ddays(10)
#   start2 <- lubridate::as_date(start1)
#   end2 <- lubridate::as_date(end1)
#
#   expect_equal(
#     time_seq_v(start2, end2,
#       time = "3 days"
#     ),
#     time_seq_v(start2, end2,
#       time = 3
#     )
#   )
#   expect_equal(
#     time_seq_v(start1, end1,
#       time = "300 seconds"
#     ),
#     time_seq_v(start1, end1,
#       time = 300
#     )
#   )
#   expect_equal(
#     time_seq_v(start1, end2,
#       time = "338.5 seconds"
#     ),
#     time_seq_v(start1, end2,
#       time = 338.5
#     )
#   )
#   expect_equal(
#     time_seq_v(start2, end1,
#       time = "338.5 seconds"
#     ),
#     time_seq_v(start2, end1,
#       time = 338.5
#     )
#   )
# })

test_that("time_seq_fill", {
  x <- c(1L, NA, NA, NA, NA, 6L, NA, 8L, NA, NA, NA, NA, NA, NA, NA,
    16L, NA, NA, 19L, 20L, NA, NA, NA, 24L, NA, NA, NA, NA, NA, 30L,
    NA, 32L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 43L, 44L, NA,
    46L, NA, NA, NA, 50L, NA, 52L, NA, NA, 55L, NA, 57L, 58L, NA,
    NA, NA, NA, NA, 64L, NA, NA, NA, 68L, 69L, 70L, 71L, NA, NA,
    74L, 75L, 76L, NA, NA, NA, 80L, 81L, NA, NA, NA, NA, NA, 87L,
    NA, NA, 90L, NA, 92L, NA, 94L, NA, 96L, NA, NA, NA, NA)
  expect_equal(time_seq_fill(x), 1:100)
  expect_equal(time_seq_fill(rev(x)), 100:1)
})
