# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("time_by", {
  flights <- nycflights13::flights
  start <- lubridate::ymd_hms("2013-03-16 11:43:48",
    tz = "Europe/London"
  )
  end <- start + lubridate::ddays(10)
  expect_snapshot(
    flights %>%
      dplyr::filter(dplyr::between(time_hour, start, end)) %>%
      time_by(time_hour, "3 days") %>%
      fastplyr::f_count()
  )
  expect_snapshot(
    flights %>%
      fastplyr::f_slice(0) %>%
      time_by(time_hour)
  )
  flights_weekly <- flights %>%
    time_by(time_hour, "week")
  flights_bi_weekly <- flights %>%
    time_by(time_hour, "2 weeks")
  expect_equal(time_tbl_time_col(flights_weekly), "time_hour")
  expect_equal(time_tbl_time_col(flights_bi_weekly), "time_hour")

  expect_equal(
    flights_weekly$time_hour,
    time_cut_width(flights$time_hour, "week")
  )
  expect_equal(
    flights_bi_weekly$time_hour,
    time_cut_width(flights$time_hour, "2 weeks")
  )

  expect_equal(
    group_data(flights_weekly)$time_hour,
    time_cut_width(flights$time_hour, "week") %>%
      unique() %>%
      sort()
  )
  expect_equal(
    group_data(flights_bi_weekly)$time_hour,
    time_cut_width(flights$time_hour, "2 weeks") %>%
      unique() %>%
      sort()
  )

  printed_out <- capture.output(print(flights_weekly))
  printed_out2 <- capture.output(print(flights_bi_weekly))
  expect_equal(
    gsub(" ", "", printed_out[1]),
    "#Atibble:336,776x19"
  )
  expect_equal(
    gsub(" ", "", printed_out[2]),
    "#Time:time_hour[53]"
  )
  expect_equal(
    gsub(" ", "", printed_out[3]),
    "#Width:week"
  )
  expect_equal(
    gsub(" ", "", printed_out2[1]),
    "#Atibble:336,776x19"
  )
  expect_equal(
    gsub(" ", "", printed_out2[2]),
    "#Time:time_hour[27]"
  )
  expect_equal(
    gsub(" ", "", printed_out2[3]),
    "#Width:2weeks"
  )
})
