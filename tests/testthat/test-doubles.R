testthat::test_that("Double floating point precision", {
  set.seed(1849127)

  x <- abs(rnorm(5 * (10^6)))
  y <- sqrt(x)^2
  z <- sample(x)

  testthat::expect_equal(all(double_gt(x, y) == FALSE),
                         TRUE)
  testthat::expect_equal(all(double_gte(x, y) == TRUE),
                         TRUE)
  testthat::expect_equal(all(double_lte(x, y) == TRUE),
                         TRUE)
  testthat::expect_equal(all(double_lt(x, y) == FALSE),
                         TRUE)
  testthat::expect_equal(all(double_equal(x, y) == TRUE),
                         TRUE)

  testthat::expect_equal(all.equal(x >= z, double_gte(x, z)), TRUE)
  testthat::expect_equal(all.equal(x > z, double_gt(x, z)), TRUE)
  testthat::expect_equal(all.equal(x <= z, double_lte(x, z)), TRUE)
  testthat::expect_equal(all.equal(x < z, double_lt(x, z)), TRUE)
  testthat::expect_equal(all.equal(x == z, double_equal(x, z)), TRUE)

  x <- seq(-10, 10, 0.2)
  testthat::expect_true(all(double_gte(x + 0.2, x - 0.2 + sqrt(0.2)^2 + 0.2)))
  testthat::expect_true(all(double_gt(x + 0.2, x - 0.2 + sqrt(0.2)^2 + 0.2)) == FALSE)
  testthat::expect_true(all(double_lt(x + 0.2, x - 0.2 + sqrt(0.2)^2 + 0.2)) == FALSE)
  testthat::expect_true(all(double_lte(x + 0.2, x - 0.2 + sqrt(0.2)^2 + 0.2)))

  testthat::expect_true(all(double_equal(diff(x) - 0.2, 0)))
})
