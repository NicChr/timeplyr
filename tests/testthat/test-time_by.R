# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("time_by", {
  flights <- nycflights13::flights
  start <- lubridate::ymd_hms("2013-03-16 11:43:48",
                               tz = "Europe/London")
  end <- start + lubridate::ddays(10)
  expect_snapshot(
    flights %>%
      time_by(time_hour, time_by = "3 days",
              from = start, to = end,
              time_type = "period") %>%
      fcount()
  )
  expect_snapshot(
    flights %>%
      fslice(0) %>%
      time_by(time_hour)
  )
  flights_weekly <- flights %>%
    time_by(time_hour, time_by = "week")
  flights_bi_weekly <- flights %>%
    time_by(time_hour, time_by = "2 weeks")
  testthat::expect_equal(time_by_units(flights_weekly),
                         list(weeks = 1))
  testthat::expect_equal(time_by_var(flights_weekly),
                         "time_hour")
  testthat::expect_equal(time_by_units(flights_bi_weekly),
                         list(weeks = 2))
  testthat::expect_equal(time_by_var(flights_bi_weekly),
                         "time_hour")

  testthat::expect_equal(flights_weekly$time_hour,
                         time_summarisev(flights$time_hour,
                                         time_by = "week"))
  testthat::expect_equal(flights_bi_weekly$time_hour,
                         time_summarisev(flights$time_hour,
                                         time_by = "2 weeks"))

  testthat::expect_equal(group_data(flights_weekly)$time_hour,
                         time_summarisev(flights$time_hour,
                                         time_by = "week",
                                         sort = TRUE, unique = TRUE))
  testthat::expect_equal(group_data(flights_bi_weekly)$time_hour,
                         time_summarisev(flights$time_hour,
                                         time_by = "2 weeks",
                                         sort = TRUE, unique = TRUE))

  printed_out <- capture.output(print(flights_weekly))
  printed_out2 <- capture.output(print(flights_bi_weekly))
  testthat::expect_equal(gsub(" ", "", printed_out[1]),
                         "#Atibble:336,776x19")
  testthat::expect_equal(gsub(" ", "", printed_out[2]),
                         "#Time:time_hour[53]")
  testthat::expect_equal(gsub(" ", "", printed_out[3]),
                         "#By:week")
  testthat::expect_equal(gsub(" ", "", printed_out2[1]),
                         "#Atibble:336,776x19")
  testthat::expect_equal(gsub(" ", "", printed_out2[2]),
                         "#Time:time_hour[27]")
  testthat::expect_equal(gsub(" ", "", printed_out2[3]),
                         "#By:2weeks")
})
