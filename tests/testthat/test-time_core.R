# Set number of data.table threads to 2
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("Tests for time_countv2", {
  flights2 <- nycflights13::flights
  flights2 <- flights2 |>
    dplyr::slice_sample(n = nrow(flights2)) |>
    dplyr::mutate(date = lubridate::as_date(time_hour))
  from <- lubridate::as_datetime(lubridate::dmy(02042013)) +
    lubridate::minutes(35)
  to <- lubridate::dmy(08092013)
  from2 <- time_cast(from, flights2$time_hour)
  to2 <- time_cast(to, flights2$time_hour)
  nrow_flights2 <- flights2 |>
    dplyr::filter(dplyr::between(time_hour, from2, to2)) |>
    nrow()

  expect_equal(
    table(
      time_cut_width(flights2$time_hour, "month", from = from, to = to)
    ),
    structure(c(
      `[2013-04-01 20:35:00, +1M)` = 28329L, `[2013-05-01 20:35:00, +1M)` = 28605L,
      `[2013-06-01 20:35:00, +1M)` = 28431L, `[2013-07-01 20:35:00, +1M)` = 29454L,
      `[2013-08-01 20:35:00, +1M)` = 29066L, `[2013-09-01 20:35:00, +1M)` = 5452L
    ), dim = 6L, dimnames = structure(list(c(
      "[2013-04-01 20:35:00, +1M)",
      "[2013-05-01 20:35:00, +1M)", "[2013-06-01 20:35:00, +1M)", "[2013-07-01 20:35:00, +1M)",
      "[2013-08-01 20:35:00, +1M)", "[2013-09-01 20:35:00, +1M)"
    )), names = ""), class = "table")
  )
  tbreaks <- lubridate::with_tz(
    time_seq(from, to, "month"),
    lubridate::tz(flights2$time_hour)
  )
})

test_that("Tests for time_span", {
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
    tz = "Europe/London"
  )
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  x <- nycflights13::flights$time_hour
  y <- lubridate::as_date(x)

  expect_equal(
    time_grid(x),
    time_seq(min(x), max(x), "hour")
  )
  expect_equal(
    time_grid(y),
    time_seq(min(y), max(y), "day")
  )
  expect_equal(
    time_grid(x, from = start1, to = end1),
    lubridate::with_tz(
      time_seq(start1, end1, "hour"),
      "America/New_York"
    )
  )
  expect_equal(
    time_grid(x, from = start2, to = end2),
    lubridate::with_tz(
      time_seq(start2, end2, "hour"),
      "America/New_York"
    )
  )
  expect_equal(
    time_grid(x, from = lubridate::floor_date(start2, "week"), "week"),
    lubridate::with_tz(
      time_seq(lubridate::floor_date(start2, "week"), max(x), "week"),
      "America/New_York"
    )
  )
  expect_equal(
    time_grid(x,
      from = lubridate::floor_date(min(x), "week"),
      to = end2, "week"
    ),
    lubridate::with_tz(
      time_seq(
        lubridate::floor_date(min(x), "week"), end2,
        "week"
      ),
      "America/New_York"
    )
  )
})

test_that("Tests for time_completev", {
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
    tz = "Europe/London"
  )
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  x <- nycflights13::flights$time_hour
  y <- lubridate::as_date(x)

  tseq <- time_grid(x, "hour")
  x_missed <- time_cast(setdiff(tseq, x), tseq)
  expect_identical(time_complete_missing(x, "hour"), c(x, x_missed))
})
test_that("Tests for time_summarisev", {
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
    tz = "Europe/London"
  )
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  x <- nycflights13::flights$time_hour
  y <- lubridate::as_date(x)
  x_max <- max(x)
  tseq <- time_grid(x, "hour")
  x_missed <- time_cast(setdiff(tseq, x), tseq)
  # Without interval
  expect_identical(
    sort(unique(interval_start(time_cut_width(x, "2 weeks")))),
    time_grid(x, "2 weeks")
  )
  expect_identical(
    interval_start(time_cut_width(x, "2 weeks")),
    cut_time(x, time_grid(x, "2 weeks"))
  )
})
