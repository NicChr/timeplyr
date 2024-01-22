# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("General tests", {
  flights <- nycflights13::flights
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)

  testthat::expect_equal(
    flights %>%
      dplyr::group_by(origin, dest) %>%
      time_summarise(time = NULL,
                     across(dplyr::where(is.numeric),
                            ~ mean(.x, na.rm = TRUE))),
    flights %>%
      dplyr::group_by(origin, dest) %>%
      dplyr::summarise(across(dplyr::where(is.numeric),
                            ~ mean(.x, na.rm = TRUE)),
                       .groups = "keep")
  )
  res1 <- flights %>%
    dplyr::group_by(origin, dest) %>%
    time_summarise(across(dplyr::where(is.numeric),
                          ~ mean(.x, na.rm = TRUE)),
                   time = time_hour,
                   time_by = "month")
  res2 <- flights %>%
    dplyr::group_by(origin, dest) %>%
    dplyr::mutate(time_hour = cut_time2(time_hour,
                                    time_span(time_hour,
                                              time_by = "month"))) %>%
    dplyr::group_by(time_hour, .add = TRUE) %>%
    dplyr::summarise(across(dplyr::where(is.numeric),
                            ~ mean(.x, na.rm = TRUE)),
                     .groups = "keep") %>%
    dplyr::group_by(origin, dest)
  res3 <- flights %>%
    dplyr::group_by(origin, dest) %>%
    time_summarise(across(dplyr::where(is.numeric),
                          ~ mean(.x, na.rm = TRUE)),
                   time = time_hour,
                   time_by = "month")
  testthat::expect_true(nrow(dplyr::anti_join(res1, res3)) == 0L)
  testthat::expect_true(nrow(dplyr::anti_join(res3, res1)) == 0L)

  # Intervals
  # testthat::expect_equal(
  #   flights %>%
  #     time_summarise(time = time_hour, time_by = "2 weeks",
  #                include_interval = TRUE, time_type = "period") %>%
  #     dplyr::mutate(n_days = interval / lubridate::days(1)) %>%
  #     fcount(n_days),
  #   dplyr::tibble(n_days = c(0.75, 14),
  #                 n = c(1, 26))
  # )
  # testthat::expect_equal(
  #   flights %>%
  #     time_summarise(time = time_hour, time_by = "hour",
  #                include_interval = TRUE) %>%
  #     dplyr::mutate(n_hrs = interval / lubridate::dhours(1)) %>%
  #     fcount(n_hrs),
  #   dplyr::tibble(n_hrs = c(0, 1),
  #                 n = c(1, 6935))
  # )
  # testthat::expect_identical(
  #   flights %>%
  #     time_summarise(time = time_hour, time_by = "3.5 hours", include_interval = TRUE) %>%
  #     dplyr::filter(interval / duration_unit("hours")(1) > 3.5) %>%
  #     nrow(),
  #   0L
  # )
  # testthat::expect_identical(
  #   flights %>%
  #     time_summarise(time = time_hour,
  #                include_interval = TRUE, time_by = "3.5 hours") %>%
  #     fcount(n_hrs = interval/ duration_unit("hours")(1)),
  #   dplyr::tibble(n_hrs = c(0.5, 3.5),
  #                 n = c(1L, 2242L))
  # )
  # testthat::expect_equal(
  #   flights %>%
  #     time_summarise(time = time_hour,
  #                include_interval = TRUE, time_by = "3.5 weeks",
  #                time_type = "duration") %>%
  #     fcount(n_hrs = round(interval/ duration_unit("weeks")(1),
  #                          2)),
  #   dplyr::tibble(n_hrs = c(3.11, 3.5),
  #                 n = c(1L, 14L))
  # )
  #
  # testthat::expect_snapshot(
  #   flights %>%
  #     fgroup_by(carrier, order = FALSE) %>%
  #     time_summarise(n = dplyr::n(), time = time_hour, time_by = "3 months",
  #                    from = min(time_hour) + weeks(1),
  #                    include_interval = TRUE) %>%
  #     print(n = 10^3, width = Inf)
  # )
})

testthat::test_that("Test intervals", {
  testthat::expect_equal(
    new_tbl(x = 1:10) %>%
      time_summarise(x, time_by = 3, as_interval = TRUE,
                     .name = "int"),
    new_tbl(int = time_interval(c(1, 4, 7, 10), c(1, 4, 7, 10) + 3))
  )
  x <- time_cast(seq(1, 10, 1), Sys.Date())
  int1 <- time_summarisev(x, time_by = "37 seconds", as_interval = TRUE)
  int2 <- time_aggregate_left(x, time_by = "37 seconds", as_interval = TRUE)
  # int3 <- time_aggregate_expand(x, time_by = "37 seconds", as_interval = TRUE)
  testthat::expect_equal(int1, int2)
  # testthat::expect_equal(int1, int3)
})
