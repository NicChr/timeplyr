# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("sequence", {
  testthat::expect_equal(sequence(1:3),
                         sequence2(1:3))

  testthat::expect_equal(c(1, 1, 1.1, 1, 1.1, 1.2),
                         sequence2(1:3, by = 0.1))

  testthat::expect_equal(sequence(c(3, 2), by = c(-1, 1)),
                         sequence2(c(3, 2), by = c(-1, 1)))

  set.seed(98376234)
  x <- sample(0:99)
  from <- sample(11:20)
  by <- sample(1:5)
  testthat::expect_equal(sequence(x, from, by),
                         sequence2(x, from, by))
})
