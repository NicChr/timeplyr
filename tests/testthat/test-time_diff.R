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


test_that("grid of dates and date-times", {

  date_grid <- lubridate::dmy("01-01-2003") + 0:(ceiling(365.24*2))

  combs <- expand.grid(a = date_grid, b = date_grid)

  # same mday combs for now
  # same_mday_combs <- cheapr::sset(combs, lubridate::mday(combs$b) == lubridate::mday(combs$a))
  no_roll_combs <- cheapr::sset(combs, lubridate::mday(combs$b) <= 28 & lubridate::mday(combs$a) <= 28)
  # roll_combs <- cheapr::sset(combs, lubridate::mday(combs$b) > 28)
  # combs <- cheapr::sset(combs, combs$b >= combs$a)

  a <- no_roll_combs$a
  b <- no_roll_combs$b

  years <- timespan("years", 1)
  months <- timespan("months", 1)
  weeks <- timespan("weeks", 1)
  days <- timespan("days", 1)
  hours <- timespan("hours", 1)
  minutes <- timespan("minutes", 1)
  seconds <- timespan("seconds", 1)


  # Dates

  # expect_true(max(cppdoubles::abs_diff(time_diff(a, b, months), time_diff_original(a, b, months))) < 0.1)
  expect_true(cppdoubles::all_equal(time_diff(a, b, years), time_diff_original(a, b, years)))
  expect_true(cppdoubles::all_equal(time_diff(a, b, years * 3), time_diff_original(a, b, years * 3)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, years), time_diff_original(b, a, years)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, years * 3), time_diff_original(b, a, years * 3)))

  expect_true(cppdoubles::all_equal(time_diff(a, b, months), time_diff_original(a, b, months)))
  expect_true(cppdoubles::all_equal(time_diff(a, b, months * 3), time_diff_original(a, b, months * 3)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, months), time_diff_original(b, a, months)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, months * 3), time_diff_original(b, a, months * 3)))

  expect_true(cppdoubles::all_equal(time_diff(a, b, weeks), time_diff_original(a, b, weeks)))
  expect_true(cppdoubles::all_equal(time_diff(a, b, weeks * 3), time_diff_original(a, b, weeks * 3)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, weeks), time_diff_original(b, a, weeks)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, weeks * 3), time_diff_original(b, a, weeks * 3)))

  expect_true(cppdoubles::all_equal(time_diff(a, b, days), time_diff_original(a, b, days)))
  expect_true(cppdoubles::all_equal(time_diff(a, b, days * 3), time_diff_original(a, b, days * 3)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, days), time_diff_original(b, a, days)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, days * 3), time_diff_original(b, a, days * 3)))

  expect_true(cppdoubles::all_equal(time_diff(a, b, hours), time_diff_original(a, b, hours)))
  expect_true(cppdoubles::all_equal(time_diff(a, b, hours * 3), time_diff_original(a, b, hours * 3)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, hours), time_diff_original(b, a, hours)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, hours * 3), time_diff_original(b, a, hours * 3)))

  expect_true(cppdoubles::all_equal(time_diff(a, b, minutes), time_diff_original(a, b, minutes)))
  expect_true(cppdoubles::all_equal(time_diff(a, b, minutes * 3), time_diff_original(a, b, minutes * 3)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, minutes), time_diff_original(b, a, minutes)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, minutes * 3), time_diff_original(b, a, minutes * 3)))

  expect_true(cppdoubles::all_equal(time_diff(a, b, seconds), time_diff_original(a, b, seconds)))
  expect_true(cppdoubles::all_equal(time_diff(a, b, seconds * 3), time_diff_original(a, b, seconds * 3)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, seconds), time_diff_original(b, a, seconds)))
  expect_true(cppdoubles::all_equal(time_diff(b, a, seconds * 3), time_diff_original(b, a, seconds * 3)))

  # Date-times

  # set.seed(71243)
  # n_seconds <- unit_to_seconds(duration_unit("years")(2))
  # start <- lubridate::dmy_hms("01-02-2003 00:00:00", tz = "Europe/London")
  # end <- time_add(start, months * 14)
  #
  # datetime_grid <- time_seq(start, end, minutes * 180)
  #
  # combs <- expand.grid(a = datetime_grid, b = datetime_grid)
  #
  # no_roll_combs <- cheapr::sset(combs, lubridate::mday(combs$b) <= 28 & lubridate::mday(combs$a) <= 28)
  # roll_combs <- cheapr::sset(combs, lubridate::mday(combs$b) > 28)
  #
  # a <- no_roll_combs$a
  # b <- no_roll_combs$b
  #
  # expect_true(cppdoubles::all_equal(time_diff(a, b, years), time_diff_original(a, b, years)))
  # expect_true(cppdoubles::all_equal(time_diff(a, b, years * 3), time_diff_original(a, b, years * 3)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, years), time_diff_original(b, a, years)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, years * 3), time_diff_original(b, a, years * 3)))
  #
  # expect_true(cppdoubles::all_equal(time_diff(a, b, months), time_diff_original(a, b, months)))
  # expect_true(cppdoubles::all_equal(time_diff(a, b, months * 3), time_diff_original(a, b, months * 3)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, months), time_diff_original(b, a, months)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, months * 3), time_diff_original(b, a, months * 3)))
  #
  # expect_true(cppdoubles::all_equal(time_diff(a, b, weeks), time_diff_original(a, b, weeks)))
  # expect_true(cppdoubles::all_equal(time_diff(a, b, weeks * 3), time_diff_original(a, b, weeks * 3)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, weeks), time_diff_original(b, a, weeks)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, weeks * 3), time_diff_original(b, a, weeks * 3)))
  #
  # expect_true(cppdoubles::all_equal(time_diff(a, b, days), time_diff_original(a, b, days)))
  # expect_true(cppdoubles::all_equal(time_diff(a, b, days * 3), time_diff_original(a, b, days * 3)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, days), time_diff_original(b, a, days)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, days * 3), time_diff_original(b, a, days * 3)))
  #
  # expect_true(cppdoubles::all_equal(time_diff(a, b, hours), time_diff_original(a, b, hours)))
  # expect_true(cppdoubles::all_equal(time_diff(a, b, hours * 3), time_diff_original(a, b, hours * 3)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, hours), time_diff_original(b, a, hours)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, hours * 3), time_diff_original(b, a, hours * 3)))
  #
  # expect_true(cppdoubles::all_equal(time_diff(a, b, minutes), time_diff_original(a, b, minutes)))
  # expect_true(cppdoubles::all_equal(time_diff(a, b, minutes * 3), time_diff_original(a, b, minutes * 3)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, minutes), time_diff_original(b, a, minutes)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, minutes * 3), time_diff_original(b, a, minutes * 3)))
  #
  # expect_true(cppdoubles::all_equal(time_diff(a, b, seconds), time_diff_original(a, b, seconds)))
  # expect_true(cppdoubles::all_equal(time_diff(a, b, seconds * 3), time_diff_original(a, b, seconds * 3)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, seconds), time_diff_original(b, a, seconds)))
  # expect_true(cppdoubles::all_equal(time_diff(b, a, seconds * 3), time_diff_original(b, a, seconds * 3)))

  # res1 <- time_diff(a, b, months)
  # res2 <- time_diff_original(a, b, months)

  # res1 <- time_diff(a, b, months * 3)
  # res2 <- time_diff_original(a, b, months * 3)
#
#   res1 <- time_diff(a, b, years)
#   res2 <- time_diff_original(a, b, years)

  # c <- a[which(!double_equal(res1, res2))[1]]
  # d <- b[which(!double_equal(res1, res2))[1]]
  #
  # c;d
  # time_diff(c, d, months);time_diff_original(c, d, months)

})
