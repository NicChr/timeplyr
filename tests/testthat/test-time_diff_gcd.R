# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("time diff GCD", {
  testthat::expect_identical(time_diff_gcd(numeric()),
                             numeric())
  testthat::expect_identical(time_diff_gcd(NA),
                             NA_real_)
  testthat::expect_equal(time_diff_gcd(7), 1)
  testthat::expect_equal(time_diff_gcd(c(7, 7, 7)), 1)
  testthat::expect_equal(time_diff_gcd(c(7, 8, 9)), 1)
  testthat::expect_equal(time_diff_gcd(c(9, 8, 7)), 1)
  # Where diff() produces consecutive zeros, this should still work
  testthat::expect_equal(time_diff_gcd(c(7, 8, 9, 9, 9, 8, 7)), 1)
  # Mix of 0s and NA
  testthat::expect_equal(time_diff_gcd(c(7, NA, NA, 8, 9, 9, 9, 8, NA, 7)), 1)
  testthat::expect_equal(time_diff_gcd(c(7, NA, NA, 8, -9, 0, 0, 9, 8, NA, 7)), 1)
  testthat::expect_equal(cppdoubles::abs_diff(
    time_diff_gcd(as.vector(stats::time(AirPassengers)),
                  tol = sqrt(.Machine$double.eps)),
    1/12
  ), 1e-9/3)
  testthat::expect_equal(cppdoubles::abs_diff(
    time_diff_gcd(as.vector(stats::time(AirPassengers)),
                  tol = sqrt(.Machine$double.eps)),
    1/12
  ), 4e-09)
  testthat::expect_true(cppdoubles::double_equal(
    time_diff_gcd(as.vector(stats::time(AirPassengers)),
                  tol = sqrt(.Machine$double.eps)),
    1/12,
    tol = sqrt(.Machine$double.eps)
  ))
  testthat::expect_false(cppdoubles::double_equal(
    time_diff_gcd(as.vector(stats::time(AirPassengers)),
                  tol = sqrt(.Machine$double.eps)),
    1/12,
    tol = sqrt(.Machine$double.eps)/10
  ))
})
