# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("bincode", {
  x <- sample(-10:11, 100, TRUE)
  breaks <- seq(0L, 7L, by = 1L)

  .bin <- function(x, breaks, ...){
    breaks[.bincode(x, breaks, ...)]
  }

  expect_equal(
    .bincode(x, breaks),
    bin(x, breaks)
    )
  expect_equal(
    .bincode(x, breaks, right = FALSE),
    bin(x, breaks, right = FALSE)
    )
  expect_equal(
    .bincode(x, breaks, include.lowest = TRUE),
    bin(x, breaks, include_lowest = TRUE)
    )
  expect_equal(
    .bincode(x, breaks, right = TRUE, include.lowest = TRUE),
    bin(x, breaks, right = TRUE, include_lowest = TRUE)
  )
  expect_equal(
    .bincode(x, breaks, right = TRUE, include.lowest = FALSE),
    bin(x, breaks, right = TRUE, include_lowest = FALSE)
  )

  breaks <- seq(0, max(x), by = 0.5)

  expect_equal(
    .bincode(x, breaks),
    bin(x, breaks)
  )
  expect_equal(
    .bincode(x, breaks, right = FALSE),
    bin(x, breaks, right = FALSE)
  )
  expect_equal(
    .bincode(x, breaks, include.lowest = TRUE),
    bin(x, breaks, include_lowest = TRUE)
  )
  expect_equal(
    .bincode(x, breaks, right = TRUE, include.lowest = TRUE),
    bin(x, breaks, right = TRUE, include_lowest = TRUE)
  )
  expect_equal(
    .bincode(x, breaks, right = TRUE, include.lowest = FALSE),
    bin(x, breaks, right = TRUE, include_lowest = FALSE)
  )

  breaks <- seq(min(x), 5, by = 0.5)

  ### TO-DO: When x is integer here, this might not work
  x <- as.double(x)

  expect_equal(
    .bin(x, breaks),
    bin(x, breaks, codes = FALSE)
  )
  expect_equal(
    .bin(x, breaks, right = FALSE),
    bin(x, breaks, right = FALSE, codes = FALSE)
  )
  expect_equal(
    .bin(x, breaks, include.lowest = TRUE),
    bin(x, breaks, include_lowest = TRUE, codes = FALSE)
  )
  expect_equal(
    .bin(x, breaks, right = TRUE, include.lowest = TRUE),
    bin(x, breaks, right = TRUE, include_lowest = TRUE, codes = FALSE)
  )
  expect_equal(
    .bin(x, breaks, right = TRUE, include.lowest = FALSE),
    bin(x, breaks, right = TRUE, include_lowest = FALSE, codes = FALSE)
  )

  x <- as.double(-1:10)
  breaks <- 0:11

  expect_equal(
    bin(x, breaks, include_lowest = TRUE),
    c(NA, 1, 1:10)
  )
  expect_equal(
    bin(x, breaks, include_oob = TRUE),
    c(1, 1, 1:10)
  )
  expect_equal(
    bin(x, breaks, include_oob = TRUE, right = FALSE),
    c(NA, 1:11)
  )

  x <- seq(0, 10, 0.5)
  breaks <- seq(1, 9, 0.25)

  expect_equal(
    .bincode(x, breaks),
    bin(x, breaks)
  )
  expect_equal(
    .bincode(x, breaks, right = FALSE),
    bin(x, breaks, right = FALSE)
  )
  expect_equal(
    .bincode(x, breaks, include.lowest = TRUE),
    bin(x, breaks, include_lowest = TRUE)
  )
  expect_equal(
    .bincode(x, breaks, right = TRUE, include.lowest = TRUE),
    bin(x, breaks, right = TRUE, include_lowest = TRUE)
  )
  expect_equal(
    .bincode(x, breaks, right = TRUE, include.lowest = FALSE),
    bin(x, breaks, right = TRUE, include_lowest = FALSE)
  )

  expect_equal(
    .bincode(x, breaks),
    bin(x, breaks)
  )
  expect_equal(
    .bincode(x, breaks, right = FALSE),
    bin(x, breaks, right = FALSE)
  )
  expect_equal(
    .bincode(x, breaks, include.lowest = TRUE),
    bin(x, breaks, include_lowest = TRUE)
  )
  expect_equal(
    .bincode(x, breaks, right = TRUE, include.lowest = TRUE),
    bin(x, breaks, right = TRUE, include_lowest = TRUE)
  )
  expect_equal(
    .bincode(x, breaks, right = TRUE, include.lowest = FALSE),
    bin(x, breaks, right = TRUE, include_lowest = FALSE)
  )

  x <- as.double(-1:10)
  breaks <- 0:11

  expect_equal(
    cut_time(x, 0:10),
    c(NA, 0:9, 9)
  )
  expect_equal(
    cut_time(x, 0:9),
    c(NA, 0:8, 8, NA)
  )
  expect_equal(
    cut_time(x, c(0:8, 99, 100), include_oob = TRUE),
    c(NA, 0:8, 8, 8)
  )
  expect_equal(
    cut_time(c(x, 1000), c(0:8, 99, 100), include_oob = TRUE),
    c(NA, 0:8, 8, 8, 100)
  )
  expect_equal(
    cut_time(x, breaks),
    c(NA, 0:10)
  )
})
