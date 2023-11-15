# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("time regularity", {
  x <- c(-10:20)
  x1 <- c(-1:10, 10, NA, 13:20)
  x2 <- c(-1:10, NA, 9, 13:20)
  y1 <- time_cast(x1, tomorrow())

  # Strictly regular x
  testthat::expect_equal(suppressMessages(time_is_regular(x)), TRUE)
  testthat::expect_equal(time_is_regular(x, time_by = 1, allow_dups = FALSE), TRUE)
  testthat::expect_equal(time_is_regular(x, time_by = 1, allow_gaps = FALSE), TRUE)
  testthat::expect_equal(time_is_regular(x, time_by = 1,
                                         allow_gaps = FALSE,
                                         allow_dups = FALSE), TRUE)

  testthat::expect_equal(suppressMessages(time_is_regular(x, na.rm = FALSE)), TRUE)
  testthat::expect_equal(time_is_regular(x, time_by = 1, allow_dups = FALSE,
                                         na.rm = FALSE), TRUE)
  testthat::expect_equal(time_is_regular(x, time_by = 1,
                                         allow_gaps = FALSE, na.rm = FALSE), TRUE)
  testthat::expect_equal(time_is_regular(x, time_by = 1,
                                         allow_gaps = FALSE,
                                         allow_dups = FALSE,
                                         na.rm = FALSE), TRUE)


  testthat::expect_equal(suppressMessages(time_is_regular(x1)), TRUE)
  testthat::expect_equal(time_is_regular(x1, time_by = 1, allow_dups = FALSE), FALSE)
  testthat::expect_equal(time_is_regular(x1, time_by = 1, allow_gaps = FALSE), FALSE)
  testthat::expect_equal(time_is_regular(x1, time_by = 1,
                                         allow_gaps = FALSE,
                                         allow_dups = FALSE), FALSE)

  testthat::expect_equal(suppressMessages(time_is_regular(x1, na.rm = FALSE)), NA)
  # testthat::expect_equal(time_is_regular(x1, time_by = 1,
  #                                        allow_dups = FALSE,
  #                                        na.rm = FALSE), FALSE)
  # testthat::expect_equal(time_is_regular(x1, time_by = 1,
  #                                        allow_gaps = FALSE,
  #                                        na.rm = FALSE), FALSE)
  # testthat::expect_equal(time_is_regular(x1, time_by = 1,
  #                                        allow_gaps = FALSE,
  #                                        allow_dups = FALSE), FALSE)

#

  testthat::expect_equal(time_num_gaps(x, time_by = 1, na.rm = TRUE), 0)
  testthat::expect_equal(time_num_gaps(x1, time_by = 1, na.rm = TRUE), 2)
  testthat::expect_equal(time_num_gaps(x1, time_by = 1, na.rm = FALSE), NA_integer_)
  testthat::expect_equal(time_has_gaps(x1, time_by = 1, na.rm = TRUE), TRUE)
  testthat::expect_equal(time_has_gaps(x1, time_by = 1, na.rm = FALSE), NA)
  testthat::expect_equal(time_gaps(x1, time_by = 0.5),
                         setdiff(seq(-1, 20, 0.5), x1))
  testthat::expect_equal(time_gaps(y1, time_by = "days"), time_cast(c(11, 12), tomorrow()))

  testthat::expect_equal(time_gaps(y1, time_by = "hours"),
                         time_cast(setdiff(time_span(y1, time_by = "hours"), as_datetime2(y1)),
                                   as_datetime2(y1)))

  testthat::expect_equal(time_num_gaps(y1, time_by = "days", na.rm = TRUE), 2)
  testthat::expect_equal(time_num_gaps(y1, time_by = 1, na.rm = FALSE), NA_integer_)
  testthat::expect_equal(time_has_gaps(y1, time_by = 1, na.rm = TRUE), TRUE)
  testthat::expect_equal(time_has_gaps(y1, time_by = "days", na.rm = FALSE), NA)
})
