
testthat::test_that("General tests", {
  flights <- nycflights13::flights
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
                               tz = "GB")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)

  testthat::expect_identical(
    flights %>%
      dplyr::group_by(origin, dest) %>%
      time_summarise(across(dplyr::where(is.numeric),
                            ~ mean(.x, na.rm = TRUE))),
    flights %>%
      dplyr::group_by(origin, dest) %>%
      dplyr::summarise(across(dplyr::where(is.numeric),
                            ~ mean(.x, na.rm = TRUE)),
                       .groups = "keep")
  )
  testthat::expect_identical(
    flights %>%
      dplyr::group_by(origin, dest) %>%
      time_reframe(across(dplyr::where(is.numeric),
                            ~ mean(.x, na.rm = TRUE))),
    flights %>%
      dplyr::group_by(origin, dest) %>%
      dplyr_summarise(across(dplyr::where(is.numeric),
                              ~ mean(.x, na.rm = TRUE))) %>%
      safe_ungroup()
  )
  res1 <- flights %>%
    dplyr::group_by(origin, dest) %>%
    time_summarise(across(dplyr::where(is.numeric),
                          ~ mean(.x, na.rm = TRUE)),
                   time = time_hour,
                   by = "month")
  res2 <- flights %>%
    dplyr::group_by(origin, dest) %>%
    dplyr::mutate(time_hour = cut_time2(time_hour,
                                    time_span(time_hour,
                                              by = "month"))) %>%
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
                   by = "month",
                   keep_class = FALSE)
  testthat::expect_true(nrow(dplyr::anti_join(res1, res3)) == 0L)
  testthat::expect_true(nrow(dplyr::anti_join(res3, res1)) == 0L)

  # Intervals
  testthat::expect_equal(
    flights %>%
      time_summarise(time = time_hour, by = "2 weeks",
                 include_interval = TRUE, seq_type = "period") %>%
      dplyr::mutate(n_days = interval / lubridate::days(1)) %>%
      fcount(n_days),
    dplyr::tibble(n_days = c(0.75, 14),
                  n = c(1, 26))
  )
  testthat::expect_equal(
    flights %>%
      time_summarise(time = time_hour, by = "hour",
                 include_interval = TRUE) %>%
      dplyr::mutate(n_hrs = interval / lubridate::dhours(1)) %>%
      fcount(n_hrs),
    dplyr::tibble(n_hrs = c(0, 1),
                  n = c(1, 6935))
  )
  testthat::expect_identical(
    flights %>%
      time_summarise(time = time_hour, by = "3.5 hours", include_interval = TRUE) %>%
      dplyr::filter(interval / duration_unit("hours")(1) > 3.5) %>%
      nrow(),
    0L
  )
  testthat::expect_identical(
    flights %>%
      time_summarise(time = time_hour,
                 include_interval = TRUE, by = "3.5 hours") %>%
      fcount(n_hrs = interval/ duration_unit("hours")(1)),
    dplyr::tibble(n_hrs = c(0.5, 3.5),
                  n = c(1L, 2242L))
  )
  testthat::expect_equal(
    flights %>%
      time_summarise(time = time_hour,
                 include_interval = TRUE, by = "3.5 weeks",
                 seq_type = "duration") %>%
      fcount(n_hrs = round(interval/ duration_unit("weeks")(1),
                           2)),
    dplyr::tibble(n_hrs = c(3.11, 3.5),
                  n = c(1L, 14L))
  )
})
