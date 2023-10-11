# Set number of data.table threads to 1
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Test desc/asc", {
  flights <- nycflights13::flights

  testthat::expect_equal(farrange(flights, asc(flight),
                                  desc(tailnum),
                                  origin, dest,
                                  desc(time_hour),
                                  asc(arr_time)),
                         dplyr::arrange(flights, flight,
                                  desc(tailnum),
                                  origin, dest,
                                  desc(time_hour),
                                  arr_time))
})
