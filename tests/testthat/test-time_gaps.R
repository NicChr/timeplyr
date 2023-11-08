# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("time gaps", {
  x <- c(-1:10, NA, 13:20)
  y <- time_cast(x, tomorrow())

  testthat::expect_equal(time_gaps(x), c(11, 12))
  testthat::expect_equal(time_num_gaps(x, time_by = 1, na.rm = TRUE), 2)
  testthat::expect_equal(time_num_gaps(x, time_by = 1, na.rm = FALSE), NA_integer_)
  testthat::expect_equal(time_has_gaps(x, time_by = 1, na.rm = TRUE), TRUE)
  testthat::expect_equal(time_has_gaps(x, time_by = 1, na.rm = FALSE), NA)
  testthat::expect_equal(time_gaps(x, time_by = 0.5),
                         setdiff(seq(-1, 20, 0.5), x))
  testthat::expect_equal(time_gaps(y), time_cast(c(11, 12), tomorrow()))

  testthat::expect_equal(time_gaps(y, time_by = "hours"),
                         time_cast(setdiff(time_span(y, time_by = "hours"), as_datetime2(y)),
                                   as_datetime2(y)))

  testthat::expect_equal(time_num_gaps(y, time_by = "days", na.rm = TRUE), 2)
  testthat::expect_equal(time_num_gaps(y, time_by = 1, na.rm = FALSE), NA_integer_)
  testthat::expect_equal(time_has_gaps(y, time_by = 1, na.rm = TRUE), TRUE)
  testthat::expect_equal(time_has_gaps(y, time_by = "days", na.rm = FALSE), NA)
})
