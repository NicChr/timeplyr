# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Number of NAs", {
  testthat::expect_equal(num_na(1), 0)
  testthat::expect_equal(num_na(numeric()), 0)
  testthat::expect_equal(num_na(character()), 0)
  testthat::expect_equal(num_na(NA), 1)
  testthat::expect_equal(num_na(NaN), 1)
  testthat::expect_equal(num_na(c(NA, Inf, -Inf, NaN)), 2)
  testthat::expect_equal(num_na(-.Machine$integer.max), 0)
  testthat::expect_equal(num_na(NULL), 0)
  testthat::expect_equal(num_na(rnorm(100)), 0)
  testthat::expect_error(num_na(iris))
  testthat::expect_equal(num_na(fill_with_na(seq(1 + 10i, length.out = 20, by = 2), n = 11)), 11)
  testthat::expect_equal(num_na(fill_with_na(logical(20), n = 11)), 11)
  testthat::expect_equal(num_na(raw(20)), 0)
})

