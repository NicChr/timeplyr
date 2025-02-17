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

  time_diff_lubridate <- function(int, time){
    int / lubridate::period(timespan_num(time), plural_unit_to_single(timespan_unit(time)))
  }

  test_all <- function(a, b, use_lubridate = FALSE, na.rm = FALSE){

    if (use_lubridate){
      int1 <- interval(a, b)
      int2 <- interval(b, a)

      # Only doing this cause re-writing all the function calls below would take time
      if (na.rm){
        all_equal <- function(x, y){
          isTRUE(all.equal(x, y))
        }
      } else {
        all_equal <- function(x, y){
          cppdoubles::all_equal(x, y, na.rm = FALSE)
        }
      }
      expect_true(all_equal(time_diff(a, b, years), time_diff_lubridate(int1, years)))
      expect_true(all_equal(time_diff(a, b, years * 3), time_diff_lubridate(int1, years * 3)))
      expect_true(all_equal(time_diff(b, a, years), time_diff_lubridate(int2, years)))
      expect_true(all_equal(time_diff(b, a, years * 3), time_diff_lubridate(int2, years * 3)))

      expect_true(all_equal(time_diff(a, b, months), time_diff_lubridate(int1, months)))
      expect_true(all_equal(time_diff(a, b, months * 3), time_diff_lubridate(int1, months * 3)))
      expect_true(all_equal(time_diff(b, a, months), time_diff_lubridate(int2, months)))
      expect_true(all_equal(time_diff(b, a, months * 3), time_diff_lubridate(int2, months * 3)))

      expect_true(all_equal(time_diff(a, b, weeks), time_diff_lubridate(int1, weeks)))
      expect_true(all_equal(time_diff(a, b, weeks * 3), time_diff_lubridate(int1, weeks * 3)))
      expect_true(all_equal(time_diff(b, a, weeks), time_diff_lubridate(int2, weeks)))
      expect_true(all_equal(time_diff(b, a, weeks * 3), time_diff_lubridate(int2, weeks * 3)))

      expect_true(all_equal(time_diff(a, b, days), time_diff_lubridate(int1, days)))
      expect_true(all_equal(time_diff(a, b, days * 3), time_diff_lubridate(int1, days * 3)))
      expect_true(all_equal(time_diff(b, a, days), time_diff_lubridate(int2, days)))
      expect_true(all_equal(time_diff(b, a, days * 3), time_diff_lubridate(int2, days * 3)))

      expect_true(all_equal(time_diff(a, b, hours), time_diff_lubridate(int1, hours)))
      expect_true(all_equal(time_diff(a, b, hours * 3), time_diff_lubridate(int1, hours * 3)))
      expect_true(all_equal(time_diff(b, a, hours), time_diff_lubridate(int2, hours)))
      expect_true(all_equal(time_diff(b, a, hours * 3), time_diff_lubridate(int2, hours * 3)))

      expect_true(all_equal(time_diff(a, b, minutes), time_diff_lubridate(int1, minutes)))
      expect_true(all_equal(time_diff(a, b, minutes * 3), time_diff_lubridate(int1, minutes * 3)))
      expect_true(all_equal(time_diff(b, a, minutes), time_diff_lubridate(int2, minutes)))
      expect_true(all_equal(time_diff(b, a, minutes * 3), time_diff_lubridate(int2, minutes * 3)))

      expect_true(all_equal(time_diff(a, b, seconds), time_diff_lubridate(int1, seconds)))
      expect_true(all_equal(time_diff(a, b, seconds * 3), time_diff_lubridate(int1, seconds * 3)))
      expect_true(all_equal(time_diff(b, a, seconds), time_diff_lubridate(int2, seconds)))
      expect_true(all_equal(time_diff(b, a, seconds * 3), time_diff_lubridate(int2, seconds * 3)))
    } else {
      expect_true(all_equal(time_diff(a, b, years), time_diff_original(a, b, years)))
      expect_true(all_equal(time_diff(a, b, years * 3), time_diff_original(a, b, years * 3)))
      expect_true(all_equal(time_diff(b, a, years), time_diff_original(b, a, years)))
      expect_true(all_equal(time_diff(b, a, years * 3), time_diff_original(b, a, years * 3)))

      expect_true(all_equal(time_diff(a, b, months), time_diff_original(a, b, months)))
      expect_true(all_equal(time_diff(a, b, months * 3), time_diff_original(a, b, months * 3)))
      expect_true(all_equal(time_diff(b, a, months), time_diff_original(b, a, months)))
      expect_true(all_equal(time_diff(b, a, months * 3), time_diff_original(b, a, months * 3)))

      expect_true(all_equal(time_diff(a, b, weeks), time_diff_original(a, b, weeks)))
      expect_true(all_equal(time_diff(a, b, weeks * 3), time_diff_original(a, b, weeks * 3)))
      expect_true(all_equal(time_diff(b, a, weeks), time_diff_original(b, a, weeks)))
      expect_true(all_equal(time_diff(b, a, weeks * 3), time_diff_original(b, a, weeks * 3)))

      expect_true(all_equal(time_diff(a, b, days), time_diff_original(a, b, days)))
      expect_true(all_equal(time_diff(a, b, days * 3), time_diff_original(a, b, days * 3)))
      expect_true(all_equal(time_diff(b, a, days), time_diff_original(b, a, days)))
      expect_true(all_equal(time_diff(b, a, days * 3), time_diff_original(b, a, days * 3)))

      expect_true(all_equal(time_diff(a, b, hours), time_diff_original(a, b, hours)))
      expect_true(all_equal(time_diff(a, b, hours * 3), time_diff_original(a, b, hours * 3)))
      expect_true(all_equal(time_diff(b, a, hours), time_diff_original(b, a, hours)))
      expect_true(all_equal(time_diff(b, a, hours * 3), time_diff_original(b, a, hours * 3)))

      expect_true(all_equal(time_diff(a, b, minutes), time_diff_original(a, b, minutes)))
      expect_true(all_equal(time_diff(a, b, minutes * 3), time_diff_original(a, b, minutes * 3)))
      expect_true(all_equal(time_diff(b, a, minutes), time_diff_original(b, a, minutes)))
      expect_true(all_equal(time_diff(b, a, minutes * 3), time_diff_original(b, a, minutes * 3)))

      expect_true(all_equal(time_diff(a, b, seconds), time_diff_original(a, b, seconds)))
      expect_true(all_equal(time_diff(a, b, seconds * 3), time_diff_original(a, b, seconds * 3)))
      expect_true(all_equal(time_diff(b, a, seconds), time_diff_original(b, a, seconds)))
      expect_true(all_equal(time_diff(b, a, seconds * 3), time_diff_original(b, a, seconds * 3)))
    }
  }

  date_grid <- lubridate::dmy("01-01-2003") + 0:(ceiling(365.24*3))

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

  test_all(a, b)

  # Date-times (no DST rolling)

  set.seed(71243)
  start <- lubridate::dmy_hms("01-06-2003 00:00:00", tz = "Europe/London")
  end <- time_add(start, months * 1)

  datetime_grid <- time_seq(start, end, minutes * 180)
  datetime_grid <- time_add(datetime_grid, years * sample.int(10, length(datetime_grid), TRUE) - 1L)

  combs <- expand.grid(a = datetime_grid, b = datetime_grid)

  no_roll_combs <- cheapr::sset(combs, lubridate::mday(combs$b) <= 28 & lubridate::mday(combs$a) <= 28)

  a <- no_roll_combs$a
  b <- no_roll_combs$b

  test_all(a, b)

  # Date-times (DST rolling)

  set.seed(71243)
  start <- lubridate::dmy_hms("24-10-2003 00:00:00", tz = "Europe/London")
  end <- time_add(start, days * 5)

  datetime_grid <- time_seq(start, end, minutes * 30)
  datetime_grid <- time_add(datetime_grid, years * sample.int(10, length(datetime_grid), TRUE) - 1L)

  combs <- expand.grid(a = datetime_grid, b = datetime_grid)

  no_roll_combs <- cheapr::sset(combs, lubridate::mday(combs$b) <= 28 & lubridate::mday(combs$a) <= 28)

  a <- no_roll_combs$a
  b <- no_roll_combs$b

  test_all(a, b, na.rm = TRUE)

  # Manual testing and checking
  # unit <- years
  #
  # res1 <- time_diff(a, b, unit)
  # res2 <- time_diff_lubridate(int1, unit)
  # res3 <- time_diff_original(a, b, unit)
  #
  # target <- res3
  #
  # neq <- which(!double_equal(res1, target))
  # c <- a[neq][1]
  # d <- b[neq][1]
  #
  # c;d
  # time_diff(c, d, unit);time_diff_original(c, d, unit);time_diff_lubridate(interval(c, d), unit)
})
