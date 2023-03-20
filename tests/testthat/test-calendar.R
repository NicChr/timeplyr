testthat::test_that("calendar", {
  x <- time_seq(lubridate::today(),
                length.out = 100,
                by = "2 hours")
  y <- time_seq(lubridate::today(),
                length.out = 100,
                by = "2 days")
  cal1 <- calendar(x)
  cal2 <- calendar(y)
  testthat::expect_identical(cal1,
                             dplyr::tibble(time = x) %>%
                               add_calendar(time))
  testthat::expect_identical(cal1,
                             create_calendar(lubridate::today(),
                                             length.out = 100,
                                             by = "2 hours"))
  testthat::expect_identical(calendar(x, label = FALSE),
                             cal1 %>%
                               dplyr::select(-all_of(c("month_l", "wday_l"))))

  testthat::expect_identical(cal2,
                             dplyr::tibble(time = y) %>%
                               add_calendar(time))
  testthat::expect_identical(cal2,
                             create_calendar(lubridate::today(),
                                             length.out = 100,
                                             by = "2 days"))
  testthat::expect_identical(calendar(y, label = FALSE),
                             cal2 %>%
                               dplyr::select(-all_of(c("month_l", "wday_l"))))
})
