# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Lags", {
  x <- as.integer(c(NA, 2, 1, 1, NA, 3, 10))
  y <- c(NA, 2, 1, 1, NA, 3, 10:15)
  z <- c(NA, TRUE, FALSE, NA, TRUE, TRUE, FALSE, TRUE)

  lag <- dplyr::lag
  lead <- dplyr::lead
  expect_identical(roll_diff(x, n = 1), as.integer(x - lag(x)))
  expect_identical(roll_diff(x, n = 3), as.integer(x - lag(x, 3)))
  expect_identical(roll_diff(x, n = 100), as.integer(x - lag(x, 100)))
  expect_identical(roll_diff(x, n = -1), as.integer(x - lead(x)))
  expect_identical(roll_diff(x, n = -3), as.integer(x - lead(x, 3)))

  expect_identical(roll_lag(x, n = 1), as.integer(lag(x)))
  expect_identical(roll_lag(x, n = 3), as.integer(lag(x, 3)))
  expect_identical(roll_lag(x, n = 100), as.integer(lag(x, 100)))
  expect_identical(roll_lag(x, n = -1), as.integer(lead(x)))
  expect_identical(roll_lag(x, n = -3), as.integer(lead(x, 3)))

  expect_identical(roll_diff(y, n = 1), y - lag(y))
  expect_identical(roll_diff(y, n = 3), y - lag(y, 3))
  expect_identical(roll_diff(y, n = 100), y - lag(y, 100))
  expect_identical(roll_diff(y, n = -1), y - lead(y))
  expect_identical(roll_diff(y, n = -3), y - lead(y, 3))

  expect_identical(roll_lag(y, n = 1), lag(y))
  expect_identical(roll_lag(y, n = 3), lag(y, 3))
  expect_identical(roll_lag(y, n = 100), lag(y, 100))
  expect_identical(roll_lag(y, n = -1), lead(y))
  expect_identical(roll_lag(y, n = -3), lead(y, 3))

  expect_identical(roll_diff(z, n = 1), as.integer(z - lag(z)))
  expect_identical(roll_diff(z, n = 3), as.integer(z - lag(z, 3)))
  expect_identical(roll_diff(z, n = 100), as.integer(z - lag(z, 100)))
  expect_identical(roll_diff(z, n = -1), as.integer(z - lead(z)))
  expect_identical(roll_diff(z, n = -3), as.integer(z - lead(z, 3)))

  expect_identical(roll_lag(z, n = 1), lag(z))
  expect_identical(roll_lag(z, n = 3), lag(z, 3))
  expect_identical(roll_lag(z, n = 100), lag(z, 100))
  expect_identical(roll_lag(z, n = -1), lead(z))
  expect_identical(roll_lag(z, n = -3), lead(z, 3))
})

testthat::test_that("Grouped lags", {
  set.seed(99)
  x <- sample(-11:37, size = 100, replace = TRUE)
  y <- round(rnorm(100), 2)
  z <- as.logical(sample.int(2, 100, TRUE) - 1L)

  x <- na_fill(x, prop = 1/4)
  y <- na_fill(y, prop = 1/3)
  z <- na_fill(z, prop = 1/5)

  gx <- sample.int(5, length(x), TRUE)
  gy <- sample.int(5, length(y), TRUE) - 3L
  gz <- sample(c(-99, NA_real_, 10), length(z), TRUE)

  lag2 <- function(x, g, n = 1L){
    dplyr::pull(
      dplyr::mutate(
        dplyr::tibble(x = x, g = g), lag = dplyr::lag(x, n = n), .by = g
        ), lag
    )
  }
  lead2 <- function(x, g, n = 1L){
    dplyr::pull(
      dplyr::mutate(
        dplyr::tibble(x = x, g = g), lead = dplyr::lead(x, n = n), .by = g
      ), lead
    )
  }

  diff3 <- function(x, g, n = 1L){
    dplyr::pull(
      dplyr::mutate(
        dplyr::tibble(x = x, g = g), diff = x - dplyr::lag(x, n = n), .by = g
      ), diff
    )
  }

  diff4 <- function(x, g, n = 1L){
    dplyr::pull(
      dplyr::mutate(
        dplyr::tibble(x = x, g = g), diff = x - dplyr::lead(x, n = n), .by = g
      ), diff
    )
  }

  expect_identical(roll_lag(x, g = gx, n = 1), lag2(x, g = gx, n = 1))
  expect_identical(roll_lag(x, g = gx, n = 3), lag2(x, g = gx, n = 3))
  expect_identical(roll_lag(x, g = gx, n = 100), lag2(x, g = gx, n = 100))
  expect_identical(roll_lag(x, g = gx, n = 300), lag2(x, g = gx, n = 300))
  expect_identical(roll_lag(x, g = gx, n = -1), lead2(x, g = gx, n = 1))
  expect_identical(roll_lag(x, g = gx, n = -3), lead2(x, g = gx, n = 3))

  expect_equal(roll_lag(y, g = gy, n = 1), lag2(y, g = gy, n = 1))
  expect_equal(roll_lag(y, g = gy, n = 3), lag2(y, g = gy, n = 3))
  expect_equal(roll_lag(y, g = gy, n = 100), lag2(y, g = gy, n = 100))
  expect_equal(roll_lag(y, g = gy, n = 300), lag2(y, g = gy, n = 300))
  expect_equal(roll_lag(y, g = gy, n = -1), lead2(y, g = gy, n = 1))
  expect_equal(roll_lag(y, g = gy, n = -3), lead2(y, g = gy, n = 3))

  expect_equal(roll_lag(z, g = gz, n = 1), lag2(z, g = gz, n = 1))
  expect_equal(roll_lag(z, g = gz, n = 3), lag2(z, g = gz, n = 3))
  expect_equal(roll_lag(z, g = gz, n = 100), lag2(z, g = gz, n = 100))
  expect_equal(roll_lag(z, g = gz, n = 300), lag2(z, g = gz, n = 300))
  expect_equal(roll_lag(z, g = gz, n = -1), lead2(z, g = gz, n = 1))
  expect_equal(roll_lag(z, g = gz, n = -3), lead2(z, g = gz, n = 3))


  expect_identical(roll_diff(x, g = gx, n = 1), diff3(x, g = gx, n = 1))
  expect_identical(roll_diff(x, g = gx, n = 3), diff3(x, g = gx, n = 3))
  expect_identical(roll_diff(x, g = gx, n = 100), diff3(x, g = gx, n = 100))
  expect_identical(roll_diff(x, g = gx, n = 300), diff3(x, g = gx, n = 300))
  expect_identical(roll_diff(x, g = gx, n = -1), diff4(x, g = gx, n = 1))
  expect_identical(roll_diff(x, g = gx, n = -3), diff4(x, g = gx, n = 3))

  expect_equal(roll_diff(y, g = gy, n = 1), diff3(y, g = gy, n = 1))
  expect_equal(roll_diff(y, g = gy, n = 3), diff3(y, g = gy, n = 3))
  expect_equal(roll_diff(y, g = gy, n = 100), diff3(y, g = gy, n = 100))
  expect_equal(roll_diff(y, g = gy, n = 300), diff3(y, g = gy, n = 300))
  expect_equal(roll_diff(y, g = gy, n = -1), diff4(y, g = gy, n = 1))
  expect_equal(roll_diff(y, g = gy, n = -3), diff4(y, g = gy, n = 3))

  expect_equal(roll_diff(z, g = gz, n = 1), diff3(z, g = gz, n = 1))
  expect_equal(roll_diff(z, g = gz, n = 3), diff3(z, g = gz, n = 3))
  expect_equal(roll_diff(z, g = gz, n = 100), diff3(z, g = gz, n = 100))
  expect_equal(roll_diff(z, g = gz, n = 300), diff3(z, g = gz, n = 300))
  expect_equal(roll_diff(z, g = gz, n = -1), diff4(z, g = gz, n = 1))
  expect_equal(roll_diff(z, g = gz, n = -3), diff4(z, g = gz, n = 3))

  gxo <- order(gx)
  gyo <- order(gy)
  gzo <- order(gz)

  x <- x[gxo]
  gx <- gx[gxo]
  y <- y[gyo]
  gy <- gy[gyo]
  z <- z[gzo]
  gz <- gz[gzo]
  expect_equal(roll_lag(x, g = gx, n = 3, fill = 99),
               collapse::flag(x, g = gx, n = 3, fill = 99))
  expect_true(is.integer(roll_lag(x, g = gx, n = 3, fill = 99)))
  expect_equal(roll_lag(y, g = gy, n = 3, fill = 99),
               collapse::flag(y, g = gy, n = 3, fill = 99))
  expect_equal(roll_lag(z, g = gz, n = 3, fill = TRUE),
               collapse::flag(z, g = gz, n = 3, fill = TRUE))

  expect_equal(roll_lag(x, g = gx, n = -3, fill = 99),
               collapse::flag(x, g = gx, n = -3, fill = 99))
  expect_true(is.integer(roll_lag(x, g = gx, n = -3, fill = 99)))
  expect_equal(roll_lag(y, g = gy, n = -3, fill = 99),
               collapse::flag(y, g = gy, n = -3, fill = 99))
  expect_equal(roll_lag(z, g = gz, n = -3, fill = TRUE),
               collapse::flag(z, g = gz, n = -3, fill = TRUE))
})
