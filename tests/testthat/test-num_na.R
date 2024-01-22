# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("Number of NAs", {
  expect_equal(num_na(1), 0)
  expect_equal(num_na(numeric()), 0)
  expect_equal(num_na(character()), 0)
  expect_equal(num_na(NA), 1)
  expect_equal(num_na(NaN), 1)
  expect_equal(num_na(c(NA, Inf, -Inf, NaN)), 2)
  expect_equal(num_na(-.Machine$integer.max), 0)
  expect_equal(num_na(NULL), 0)
  expect_equal(num_na(rnorm(100)), 0)
  # The below line causes a segfault and I'm not sure why
  # Because it should error gracefully
  ### Update ###
  # Replacing Rcpp::stop() with Rf_error() seems to have fixed it???
  expect_equal(num_na(iris), 0)
  expect_equal(num_na(df_init(iris, 10)), 10)
  expect_equal(num_na(na_fill(seq(1 + 10i, length.out = 20, by = 2), n = 11)), 11)
  expect_equal(num_na(na_fill(logical(20), n = 11)), 11)
  expect_equal(num_na(na_fill(character(20), n = 11)), 11)
  expect_equal(num_na(raw(20)), 0)
})

