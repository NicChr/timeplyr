# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("sequence", {
  expect_error(seq_v(1, 0, by = 1))
  expect_error(seq_size(1, 0, by = 1))
  expect_identical(sequence2(numeric()), integer())
  expect_identical(sequence(1:3), sequence2(1:3))
  expect_identical(c(1, 1, 1.1, 1, 1.1, 1.2),
                         sequence2(1:3, by = 0.1))

  # Numeric vs integer
  expect_equal(sequence(c(3, 2), by = c(-1, 1)),
                         sequence2(c(3, 2), by = c(-1, 1)))

  set.seed(98376234)
  x <- sample(0:99)
  from <- sample(11:20)
  by <- sample(1:5)
  expect_identical(sequence(x, from, by), sequence2(x, from, by))
  expect_equal(sequence(x, from, by), sequence2(x, as.double(from), as.double(by)))

  expect_identical(sequence(123, from = 1L, by = 0L),
                             sequence2(123, from = 1L, by = 0L))

  expect_identical(sequence2(0, from = 1L, by = 0L),
                             integer())
  expect_identical(sequence2(integer(), from = 1L, by = 0L),
                             integer())

  expect_equal(sequence2(c(2, 3, 0, 4, 0, 10),
                                       from = Sys.Date() + c(0, 11),
                                       by = 2),
                             time_seq_v2(c(2, 3, 0, 4, 0, 10),
                                         from = Sys.Date() + c(0, 11),
                                         time_by = "2 days"))
  expect_equal(sequence2(c(2, 3, 0, 4, 0, 10),
                                   from = Sys.Date() + c(0, 11),
                                   by = c(2, 7, 8)),
                         time_seq_v2(c(2, 3, 0, 4, 0, 10),
                                     from = Sys.Date() + c(0, 11),
                                     time_by = c(2, 7, 8)))

  expect_equal(seq_v(from = Sys.Date() + c(0, 11),
                               to = Sys.Date() + c(0, 12, 13, 23),
                               by = c(2, 7)),
                         time_seq_v(from = Sys.Date() + c(0, 11),
                                    to = Sys.Date() + c(0, 12, 13, 23),
                                    time_by = list("days" = c(2, 7))))

  expect_identical(seq_v(-1L, 10L, 1L), seq.int(-1L, 10L, 1L))
  expect_equal(seq_v(0, 20, 0.2), seq.int(0, 20, 0.2))

  # Integer dates
  expect_identical(
    sequence2(5, as_int_date(as.Date("2000-01-01")), by = 1L),
    as_int_date(as.Date("2000-01-01")) + 0:4
  )
  expect_equal(
    sequence2(c(2, 0, 3), as.Date("2000-01-01"), by = c(1, 0, -1)),
   c(as.Date("2000-01-01") + 0:1,
      as.Date("2000-01-01") - 0:2)
  )

})

