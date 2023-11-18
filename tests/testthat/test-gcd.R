# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("GCD AND SCM scalars", {
  testthat::expect_identical(gcd(1), 1)
  testthat::expect_identical(gcd(1L), 1L)
  testthat::expect_identical(gcd(0), 0)
  testthat::expect_identical(gcd(0L), 0L)
  testthat::expect_identical(gcd(NA_real_), NA_real_)
  testthat::expect_identical(gcd(NaN), NaN)
  testthat::expect_identical(gcd(Inf), Inf)
  testthat::expect_identical(gcd(-Inf), -Inf)
  testthat::expect_identical(gcd(NA_integer_), NA_integer_)

  testthat::expect_identical(scm(1), 1)
  testthat::expect_identical(scm(1L), 1L)
  testthat::expect_identical(scm(0), 0)
  testthat::expect_identical(scm(0L), 0L)
  testthat::expect_identical(scm(NA_real_), NA_real_)
  testthat::expect_identical(scm(NaN), NaN)
  testthat::expect_identical(scm(Inf), Inf)
  testthat::expect_identical(scm(-Inf), -Inf)
  testthat::expect_identical(scm(NA_integer_), NA_integer_)
})

testthat::test_that("Small vectors (integers)", {
  x <- c(NA_integer_, 9L, 0L, 0L, 3L, 21L)
  y <- c(9L, NA_integer_, NA_integer_, 0L, 0L, 0L, 3L, 21L)
  testthat::expect_identical(gcd(x, na_rm = TRUE), 3L)
  testthat::expect_identical(gcd(x, na_rm = FALSE), NA_integer_)
  testthat::expect_identical(gcd(x, tol = 0, na_rm = TRUE), 3L)
  testthat::expect_identical(gcd(x, tol = 0, na_rm = FALSE), NA_integer_)
  testthat::expect_identical(gcd(x, tol = 0, na_rm = TRUE, round = TRUE), 3L)

  testthat::expect_identical(gcd(y, na_rm = TRUE), 3L)
  testthat::expect_identical(gcd(y, na_rm = FALSE), NA_integer_)
  testthat::expect_identical(gcd(y, tol = 0, na_rm = TRUE), 3L)
  testthat::expect_identical(gcd(y, tol = 0, na_rm = FALSE), NA_integer_)
  testthat::expect_identical(gcd(y, tol = 0, na_rm = TRUE, round = TRUE), 3L)
})

testthat::test_that("Small vectors (doubles)", {
  x <- c(NA_real_, 9, 0, 0, 3, 21)
  y <- c(9, NA_real_, NA_real_, 0, 0, 0, NaN, NaN, NaN, 3, Inf, -Inf, 21)
  testthat::expect_identical(gcd(x, na_rm = TRUE), 3)
  testthat::expect_identical(gcd(x, na_rm = FALSE), NA_real_)
  testthat::expect_identical(gcd(x, tol = 0, na_rm = TRUE), 3)
  testthat::expect_identical(gcd(x, tol = 0, na_rm = FALSE), NA_real_)
  testthat::expect_identical(gcd(x, tol = 0, na_rm = TRUE, round = TRUE), 3)

  testthat::expect_identical(gcd(y, na_rm = TRUE), 3)
  testthat::expect_identical(gcd(y, na_rm = FALSE), NA_real_)
  testthat::expect_identical(gcd(y, tol = 0, na_rm = TRUE), 3)
  testthat::expect_identical(gcd(y, tol = 0, na_rm = FALSE), NA_real_)
  testthat::expect_identical(gcd(y, tol = 0, na_rm = TRUE, round = TRUE), 3)
})

testthat::test_that("Small vectors (doubles)", {
  set.seed(94773)
  x <- rnorm(100)
  x <- abs(x)
  tol <- sqrt(.Machine$double.eps)
  testthat::expect_equal(gcd(x, tol = tol, break_early = TRUE), tol)
  testthat::expect_equal(gcd(x, tol = tol, break_early = FALSE), tol)
  testthat::expect_equal(gcd(x, tol = tol, round = TRUE), round(tol, 9))
  testthat::expect_equal(gcd(x, tol = tol, round = TRUE, break_early = TRUE),
                         round(tol, 9))
  testthat::expect_true(gcd(x, tol = 0) < tol/10)

  x <- as.double(time(AirPassengers))

  testthat::expect_equal(gcd(x, tol = tol), 1/12)
  testthat::expect_false(isTRUE(all.equal(gcd(x, tol = 0), 1/12)))
  testthat::expect_false(isTRUE(all.equal(gcd(diff(x), tol = 0), 1/12)))
  testthat::expect_equal(gcd(x, round = TRUE, tol = tol), 1/12)
  testthat::expect_equal(gcd(diff(x), tol = tol), 1/12)
  testthat::expect_equal(gcd(diff(x), round = TRUE, tol = tol), 1/12)
})

testthat::test_that("signs", {
  a <- c(0L, 0L, seq.int(25L, 0L, by = -5L))
  b <- c(0, 0, seq.int(25, 0, by = -5))
  c <- -a
  d <- -b
  testthat::expect_identical(gcd(a), 5L)
  testthat::expect_identical(gcd(b), 5)
  testthat::expect_identical(gcd(c), -5L)
  testthat::expect_identical(gcd(d), -5)

  # testthat::expect_identical(gcd(c(2, -4, 2)), 2)
  # testthat::expect_identical(gcd(c(2, -4, 2, -8)), -2)
})

testthat::test_that("Overflow", {
  testthat::expect_equal(scm(1:22), 232792560)
  testthat::expect_equal(scm(-(1:22)), -232792560)
  testthat::expect_warning(scm(1:23))
  testthat::expect_warning(scm(-(1:23)))
})
