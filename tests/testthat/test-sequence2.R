# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("sequence", {
  testthat::expect_identical(sequence2(numeric()), integer())
  testthat::expect_identical(sequence(1:3), sequence2(1:3))

  testthat::expect_identical(c(1, 1, 1.1, 1, 1.1, 1.2),
                         sequence2(1:3, by = 0.1))

  # Numeric vs integer
  testthat::expect_equal(sequence(c(3, 2), by = c(-1, 1)),
                         sequence2(c(3, 2), by = c(-1, 1)))

  set.seed(98376234)
  x <- sample(0:99)
  from <- sample(11:20)
  by <- sample(1:5)
  testthat::expect_identical(sequence(x, from, by), sequence2(x, from, by))
  testthat::expect_equal(sequence(x, from, by), sequence2(x, as.double(from), as.double(by)))

  testthat::expect_identical(sequence(123, from = 1L, by = 0L),
                             sequence2(123, from = 1L, by = 0L))

  testthat::expect_identical(sequence2(0, from = 1L, by = 0L),
                             integer())
  testthat::expect_identical(sequence2(integer(), from = 1L, by = 0L),
                             integer())

  testthat::expect_equal(sequence2(c(2, 3, 0, 4, 0, 10),
                                       from = Sys.Date() + c(0, 11),
                                       by = 2),
                             time_seq_v2(c(2, 3, 0, 4, 0, 10),
                                         from = Sys.Date() + c(0, 11),
                                         time_by = "2 days"))
  testthat::expect_equal(sequence2(c(2, 3, 0, 4, 0, 10),
                                   from = Sys.Date() + c(0, 11),
                                   by = c(2, 7, 8)),
                         time_seq_v2(c(2, 3, 0, 4, 0, 10),
                                     from = Sys.Date() + c(0, 11),
                                     time_by = c(2, 7, 8)))

  testthat::expect_equal(seq_v(from = Sys.Date() + c(0, 11),
                               to = Sys.Date() + c(0, 12, 13, 23),
                               by = c(2, 7)),
                         time_seq_v(from = Sys.Date() + c(0, 11),
                                    to = Sys.Date() + c(0, 12, 13, 23),
                                    time_by = list("days" = c(2, 7))))
})

