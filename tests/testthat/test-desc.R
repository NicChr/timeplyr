testthat::test_that("Test desc/asc", {
  flights <- nycflights13::flights

  testthat::expect_equal(farrange(flights, asc(flight),
                                  desc(tailnum),
                                  origin, dest,
                                  desc(time_hour),
                                  asc(arr_time)),
                         dplyr::arrange(flights, asc(flight),
                                  desc(tailnum),
                                  origin, dest,
                                  desc(time_hour),
                                  asc(arr_time)))
})
