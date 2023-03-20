
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
})
