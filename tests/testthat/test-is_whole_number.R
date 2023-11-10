# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Whole numbers", {
  set.seed(42)
  x <- rnorm(10^2)
  y <- round(x)
  z <- trunc(x)
  a <- ceiling(x)
  b <- floor(x)
  c <- na_fill(y, prop = 0.2)
  testthat::expect_equal(is_whole_number(NA_real_, na.rm = FALSE), NA)
  testthat::expect_equal(is_whole_number(NA_real_), TRUE)
  testthat::expect_true(is_whole_number(0))
  # testthat::expect_true(is_whole_number(c(NA_integer_, 1:10)))

  ## Internally vectorised against tolerance
  testthat::expect_true(is_whole_number(5))
  testthat::expect_error(is_whole_number(5, c(0.01, -Inf)))

  # Even with NAs, if x is an integer we always return TRUE
  testthat::expect_true(is_whole_number(c(NA_integer_, 1:10), na.rm = FALSE))
  testthat::expect_true(is.na(is_whole_number(c(NA_real_, 1:10), na.rm = FALSE)))

  ## Different types
  testthat::expect_false(is_whole_number(letters))
  testthat::expect_false(is_whole_number(Sys.Date()))

  # testthat::expect_error(is_whole_number(1, tol = 1:2))
  testthat::expect_true(is_whole_number(numeric()))
  testthat::expect_false(is_whole_number(x))
  testthat::expect_true(is_whole_number(y))
  testthat::expect_true(is_whole_number(z))
  testthat::expect_true(is_whole_number(a))
  testthat::expect_true(is_whole_number(b))
  testthat::expect_true(is_whole_number(c))
  testthat::expect_true(is.na(is_whole_number(c, na.rm = FALSE)))
  testthat::expect_false(is_whole_number(c(Inf, y)))
  testthat::expect_false(is_whole_number(c(-Inf, y)))

  testthat::expect_true(
    is_whole_number(c(110 * 10^200, 1.1 * 100 * 10^200, 10^9 + 2))
  )
  testthat::expect_false(is_whole_number(10^9 + 0.02))

  # 0 length tolerance should always return true
  # because there's nothing to compare against, and therefore
  # we can't determine that x is not a whole number
  testthat::expect_false(is_whole_number(1.5))
  testthat::expect_error(is_whole_number(1.5, tol = NULL))
  testthat::expect_error(is_whole_number(1.5, tol = numeric()))
  testthat::expect_error(is_whole_number(1.5, tol = integer()))
})
