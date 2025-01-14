# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("time regularity", {
  x <- c(-10:20)
  x1 <- c(-1:10, 10, NA, 13:20)
  x2 <- c(-1:10, NA, 9, 13:20)
  y1 <- time_cast(x1, tomorrow())

  # Strictly regular x
  expect_true(suppressMessages(time_is_regular(x)))
  expect_true(time_is_regular(x, 1, allow_dups = FALSE, allow_gaps = TRUE))
  expect_true(time_is_regular(x, 1, allow_gaps = FALSE, allow_dups = TRUE))
  expect_true(time_is_regular(x,
    1,
    allow_gaps = FALSE,
    allow_dups = FALSE
  ), TRUE)

  expect_true(suppressMessages(time_is_regular(x, na.rm = FALSE)))
  expect_equal(time_is_regular(x,
    1, allow_dups = FALSE,
    na.rm = FALSE
  ), TRUE)
  expect_equal(time_is_regular(x,
    1,
    allow_gaps = FALSE, na.rm = FALSE
  ), TRUE)
  expect_equal(time_is_regular(x,
    1,
    allow_gaps = FALSE,
    allow_dups = FALSE,
    na.rm = FALSE
  ), TRUE)


  expect_equal(suppressMessages(time_is_regular(x1, allow_gaps = TRUE, allow_dups = TRUE)), TRUE)
  expect_equal(time_is_regular(x1, 1, allow_gaps = FALSE, allow_dups = TRUE), FALSE)
  expect_equal(time_is_regular(x1, 1, allow_gaps = TRUE, allow_dups = FALSE), FALSE)
  expect_equal(time_is_regular(x1,
    1,
    allow_gaps = FALSE,
    allow_dups = FALSE
  ), FALSE)

  expect_equal(suppressMessages(time_is_regular(x1, na.rm = FALSE, allow_gaps = TRUE, allow_dups = TRUE)), NA)

  expect_equal(time_num_gaps(x, 1, na.rm = TRUE), 0)
  expect_equal(time_num_gaps(x1, 1, na.rm = TRUE), 2)
  expect_equal(time_num_gaps(x1, 1, na.rm = FALSE), NA_integer_)
  expect_equal(time_has_gaps(x1, 1, na.rm = TRUE), TRUE)
  expect_equal(time_has_gaps(x1, 1, na.rm = FALSE), NA)
  expect_equal(
    time_gaps(x1, 0.5),
    setdiff(seq(-1, 20, 0.5), x1)
  )
  expect_equal(time_gaps(y1, "days"), time_cast(c(11, 12), tomorrow()))

  expect_equal(
    time_gaps(y1, "hours"),
    time_cast(
      setdiff(time_grid(y1, "hours"), as_datetime2(y1)),
      as_datetime2(y1)
    )
  )

  expect_equal(time_num_gaps(y1, "days", na.rm = TRUE), 2)
  expect_equal(time_num_gaps(y1, 1, na.rm = FALSE), NA_integer_)
  expect_equal(time_has_gaps(y1, 1, na.rm = TRUE), TRUE)
  expect_equal(time_has_gaps(y1, "days", na.rm = FALSE), NA)
})
