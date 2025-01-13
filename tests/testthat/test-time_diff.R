# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("time diff", {
  start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
    tz = "Europe/London"
  )
  end1 <- start1 + lubridate::ddays(37)
  end2 <- start1 + lubridate::days(37)
  expect_identical(
    as.double(difftime(end1, start1, units = "secs")),
    time_diff(start1, end1)
  )
  expect_identical(
    as.double(difftime(
      lubridate::with_tz(lubridate::as_date(end1), tzone = "Europe/London"),
      start1,
      units = "secs"
    )),
    time_diff(start1, lubridate::as_date(end1), 1)
  )
  expect_identical(
    as.double(difftime(lubridate::as_date(end1),
      lubridate::as_date(start1),
      units = "days"
    )),
    time_diff(lubridate::as_date(start1),
      lubridate::as_date(end1),
      1
    )
  )
  expect_identical(
    time_diff(3, 27, 1),
    27 - 3
  )
  expect_identical(
    time_diff(3, 27, 3.5),
    (27 - 3) / 3.5
  )
  expect_identical(time_diff(3, 27, 0), Inf)
  expect_identical(time_diff(-3, -27, 0), -Inf)
  expect_identical(
    time_diff(start1, end2,
      "2 days"
    ),
    18.5
  )
  expect_identical(
    time_diff(start1, end2, "2 days"),
    18.5
  )
  expect_identical(
    time_diff(start1, end1, "48 hours"),
    18.5
  )
  expect_identical(
    time_diff(start1, end1,
      lubridate::ddays(2)
    ),
    18.5
  )
  expect_equal(
    time_diff(start1, end1,
      lubridate::ddays(-2)
    ),
    -18.5
  )
  expect_equal(
    time_diff(end1, start1,
      lubridate::ddays(2)
    ),
    -18.5
  )
  expect_equal(
    time_diff(end1, start1,
      lubridate::ddays(-2)
    ),
    18.5
  )
  expect_identical(
    time_diff(lubridate::Date(0), lubridate::Date(0),
      "2 days"
    ),
    numeric(0)
  )
  expect_identical(
    time_diff(lubridate::Date(0), lubridate::POSIXct(0),
      "2 days"
    ),
    numeric(0)
  )
  expect_identical(
    time_diff(lubridate::today(), lubridate::POSIXct(0),
      "2 days"
    ),
    numeric(0)
  )
  for (unit in .duration_units) {
    expect_identical(
      time_diff(start1, end1,
        timespan("seconds", unit_to_seconds(unit))
      ),
      lubridate::interval(start1, end1) /
        duration_unit(unit)(1)
    )
  }
  leap1 <- lubridate::dmy("29-02-2020")
  leap2 <- lubridate::dmy("28-02-2021")
  leap3 <- lubridate::dmy("01-03-2021")
  expect_true(floor(
    time_diff(leap1, leap2,
      "year"
    )
  ) == 1)
  expect_true(
    age_years(leap1, leap2) == 0
  )
  expect_true(floor(
    time_diff(leap1, leap3, lubridate::years(1))
  ) == 1)
  expect_true(age_years(leap1, leap3) == 1)

  # Test vectorization
  seq1 <- seq(-10, 10, 2)
  res1 <- time_diff(start1, end1,
    lubridate::ddays(seq1)
  )
  res2 <- numeric(length(res1))
  for (i in seq_along(seq1)) {
    res2[i] <- time_diff(start1, end1,
      timespan("seconds", seq1[i] * 86400)
    )
  }
  expect_equal(res1, res2)

  res3 <- time_diff(start1, end2, timespan("days", seq1))
  res4 <- numeric(length(res3))
  for (i in seq_along(seq1)) {
    res4[i] <- time_diff(start1, end2,
      timespan("days", seq1[i])
    )
  }
  expect_equal(res3, res4)

  res5 <- time_diff(start1, end1 + lubridate::ddays(seq1), "86400 seconds")
  res6 <- numeric(length(res5))
  for (i in seq_along(seq1)) {
    res6[i] <- time_diff(start1, end1 + lubridate::ddays(seq1[i]),
      timespan("seconds", 86400)
    )
  }
  expect_equal(res5, res6)
})

test_that("lubridate period multiplication", {
  x <- months(c(NA, seq(-10, 11, 1), NA))
  expect_equal(
    multiply_single_unit_period_by_number(x, c(Inf, -Inf, NaN, NA, -10:33)),
    x * c(Inf, -Inf, NaN, NA, -10:33)
  )
  expect_equal(
    multiply_single_unit_period_by_number(x, integer()),
    x * integer()
  )
  expect_equal(
    multiply_single_unit_period_by_number(x, numeric()),
    x * numeric()
  )
  expect_equal(
    multiply_single_unit_period_by_number(x, NA),
    x * NA
  )
  expect_equal(
    multiply_single_unit_period_by_number(x, Inf),
    x * Inf
  )
  expect_equal(
    multiply_single_unit_period_by_number(x, NULL),
    x * NULL
  )
})
