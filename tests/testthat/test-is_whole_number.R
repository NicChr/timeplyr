# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("Whole numbers", {
  set.seed(42)
  x <- rnorm(10^2)
  y <- round(x)
  z <- trunc(x)
  a <- ceiling(x)
  b <- floor(x)
  c <- cheapr::na_insert(y, prop = 0.2)
  expect_equal(is_whole_number(NA_real_, na.rm = FALSE), NA)
  expect_equal(is_whole_number(NA_real_), TRUE)
  expect_true(is_whole_number(0))
  # expect_true(is_whole_number(c(NA_integer_, 1:10)))

  ## Not vectorised against tolerance
  expect_true(is_whole_number(5))
  expect_error(is_whole_number(5, c(0.01, -Inf)))

  # Even with NAs, if x is an integer we always return TRUE
  expect_true(is_whole_number(c(NA_integer_, 1:10), na.rm = FALSE))
  expect_true(is.na(is_whole_number(c(NA_real_, 1:10), na.rm = FALSE)))

  ## Different types
  expect_false(is_whole_number(letters))
  expect_true(is_whole_number(Sys.Date()))

  # expect_error(is_whole_number(1, tol = 1:2))
  expect_true(is_whole_number(numeric()))
  expect_false(is_whole_number(x))
  expect_true(is_whole_number(y))
  expect_true(is_whole_number(z))
  expect_true(is_whole_number(a))
  expect_true(is_whole_number(b))
  expect_true(is_whole_number(c))
  expect_true(is.na(is_whole_number(c, na.rm = FALSE)))
  expect_false(is_whole_number(c(Inf, y)))
  expect_false(is_whole_number(c(-Inf, y)))

  expect_true(
    is_whole_number(c(110 * 10^200, 1.1 * 100 * 10^200, 10^9 + 2))
  )
  expect_false(is_whole_number(10^9 + 0.02))

  # 0 length tolerance should always return true
  # because there's nothing to compare against, and therefore
  # we can't determine that x is not a whole number
  expect_false(is_whole_number(1.5))
  expect_error(is_whole_number(1.5, tol = NULL))
  expect_error(is_whole_number(1.5, tol = numeric()))
  expect_error(is_whole_number(1.5, tol = integer()))
})
