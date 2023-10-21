testthat::test_that("Whole numbers", {
  set.seed(42)
  x <- rnorm(10^2)
  y <- round(x)
  z <- trunc(x)
  a <- ceiling(x)
  b <- floor(x)
  c <- fill_with_na(y, prop = 0.2)
  testthat::expect_true(is_whole_number(0))
  testthat::expect_true(is_whole_number(c(NA_integer_, 1:10)))

  # Even with NAs, if x is an integer we always return TRUE
  testthat::expect_true(is_whole_number(c(NA_integer_, 1:10), na.rm = FALSE))
  testthat::expect_true(is.na(is_whole_number(c(NA_real_, 1:10), na.rm = FALSE)))

  ## Different types
  testthat::expect_false(is_whole_number(letters))
  testthat::expect_false(is_whole_number(Sys.Date()))

  testthat::expect_error(is_whole_number(1, tol = 1:2))
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
    is_whole_number(c(110 * 10^200, 1.1 * 100 * 10^200, sqrt(.Machine$double.eps)/10),10^9 + 0.02,
                    10^9 + 2)
  )
  testthat::expect_false(is_whole_number(10^9 + 0.02))
  # The above is more strict than:
  # double_equal(10^9 + 0.02, round(10^9 + 0.02))
})
