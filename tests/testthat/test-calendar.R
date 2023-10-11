# Set number of data.table threads to 1
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("calendar", {
  x <- time_seq(lubridate::today(),
                length.out = 100,
                time_by = "2 hours")
  y <- time_seq(lubridate::today(),
                length.out = 100,
                time_by = "2 days")
  cal1 <- calendar(x)
  cal2 <- calendar(y)
  testthat::expect_equal(cal1,
                             list_to_tibble(list(time = x)) %>%
                               add_calendar(time))
  testthat::expect_equal(calendar(x, label = FALSE),
                             cal1 %>%
                               dplyr::select(-all_of(c("month_l", "wday_l"))))

  testthat::expect_equal(cal2,
                             dplyr::tibble(time = y) %>%
                               add_calendar(time))
  testthat::expect_equal(calendar(y, label = FALSE),
                             cal2 %>%
                               dplyr::select(-all_of(c("month_l", "wday_l"))))
})
