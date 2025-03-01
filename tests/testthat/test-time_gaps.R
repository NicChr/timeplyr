# Set number of data.table threads to 2
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("time gaps", {
  x <- c(-1:10, NA, 13:20)
  y <- time_cast(x, lubridate::today())

  expect_equal(time_gaps(x), c(11, 12))
  expect_equal(time_num_gaps(x, 1, na.rm = TRUE), 2)
  expect_equal(time_num_gaps(x, 1, na.rm = FALSE), NA_integer_)
  expect_equal(time_has_gaps(x, 1, na.rm = TRUE), TRUE)
  expect_equal(time_has_gaps(x, 1, na.rm = FALSE), NA)
  expect_equal(
    time_gaps(x, 0.5),
    setdiff(seq(-1, 20, 0.5), x)
  )
  expect_equal(time_gaps(y), time_cast(c(11, 12), lubridate::today()))

  expect_equal(
    time_gaps(y, "hours"),
    time_cast(
      setdiff(time_grid(y, "hours"), as_datetime2(y)),
      as_datetime2(y)
    )
  )

  expect_equal(time_num_gaps(y, "days", na.rm = TRUE), 2)
  expect_equal(time_num_gaps(y, 1, na.rm = FALSE), NA_integer_)
  expect_equal(time_has_gaps(y, 1, na.rm = TRUE), TRUE)
  expect_equal(time_has_gaps(y, "days", na.rm = FALSE), NA)
})
