# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("c++ which", {
  set.seed(42)
  x <- sample(c(TRUE, FALSE), size = 10^3, replace = TRUE)
  x <- na_fill(x, prop = 0.1)

  testthat::expect_identical(which(TRUE), cpp_which(TRUE))
  testthat::expect_identical(which(FALSE), cpp_which(FALSE))
  testthat::expect_identical(which(logical()), cpp_which(logical()))
  testthat::expect_identical(integer(), cpp_which(logical(), invert = TRUE))
  testthat::expect_error(cpp_which(1))
  testthat::expect_identical(which(x), cpp_which(x))
  testthat::expect_identical(which(!x %in% TRUE), cpp_which(x, invert = TRUE))

  x <- c(TRUE, NA, TRUE, NA, NA, rep_len(FALSE, 100))
  testthat::expect_identical(cpp_which(x), c(1L, 3L))
  testthat::expect_identical(cpp_which(x, invert = TRUE), c(2L, 4L, 5L, 6:length(x)))
})
