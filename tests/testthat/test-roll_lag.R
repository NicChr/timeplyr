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

  x <- cheapr::na_insert(x, prop = 1/4)
  y <- cheapr::na_insert(y, prop = 1/3)
  z <- cheapr::na_insert(z, prop = 1/5)

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


testthat::test_that("More complicated diff tests", {
  x <- rnorm(10^3)
  y <- as.integer(x)
  z <- x > 0
  o1 <- seq_along(x)
  o2 <- rev(o1)
  o3 <- sample.int(length(x))
  rl1 <- length(x)
  rl2 <- rep(50, 20)
  lags <- sample(-3:3, length(x), TRUE)

  my_diff <- function(x, ...){
    x - cheapr::lag2_(x, ...)
  }
  my_diff2 <- function(x, ...){
    out <- x - cheapr::lag2_(x, ...)
    out - cheapr::lag2_(out, ...)
  }
  my_diff3 <- function(x, ...){
    out <- x - cheapr::lag2_(x, ...)
    out <- out - cheapr::lag2_(out, ...)
    out - cheapr::lag2_(out, ...)
  }

  # Multiple lags
  expect_equal(
    lapply(-3:3, function(lag) diff_(x, lag)),
    lapply(-3:3, function(lag) my_diff(x, lag))
  )
  expect_equal(
    lapply(-3:3, function(lag) diff_(y, lag)),
    lapply(-3:3, function(lag) my_diff(y, lag))
  )
  expect_equal(
    lapply(-3:3, function(lag) diff_(z, lag)),
    lapply(-3:3, function(lag) my_diff(z, lag))
  )

  # double differencing
  expect_equal(
    lapply(-3:3, function(lag) diff_(x, lag, differences = 2)),
    lapply(-3:3, function(lag) my_diff2(x, lag))
  )
  expect_equal(
    lapply(-3:3, function(lag) diff_(y, lag, differences = 2)),
    lapply(-3:3, function(lag) my_diff2(y, lag))
  )
  expect_equal(
    lapply(-3:3, function(lag) diff_(z, lag, differences = 2)),
    lapply(-3:3, function(lag) my_diff2(z, lag))
  )

  # Triple differencing
  expect_equal(
    lapply(-3:3, function(lag) diff_(x, lag, differences = 3)),
    lapply(-3:3, function(lag) my_diff3(x, lag))
  )
  expect_equal(
    lapply(-3:3, function(lag) diff_(y, lag, differences = 3)),
    lapply(-3:3, function(lag) my_diff3(y, lag))
  )
  expect_equal(
    lapply(-3:3, function(lag) diff_(z, lag, differences = 3)),
    lapply(-3:3, function(lag) my_diff3(z, lag))
  )

  # Custom vector of lags
  expect_equal(diff_(x, lags), my_diff(x, lags))
  expect_equal(diff_(y, lags), my_diff(y, lags))
  expect_equal(diff_(z, lags), my_diff(z, lags))

  # Vector of lags, double differencing
  expect_equal(diff_(x, lags, differences = 2), my_diff2(x, lags))
  expect_equal(diff_(y, lags, differences = 2), my_diff2(y, lags))
  expect_equal(diff_(z, lags, differences = 2), my_diff2(z, lags))

  # Custom order
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(x, order = o)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(x, order = o))
  )
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(y, order = o)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(y, order = o))
  )
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(z, order = o)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(z, order = o))
  )

  # Custom order and negative lags
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(x, -3, order = o)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(x, -3, order = o))
  )
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(y, -3, order = o)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(y, -3, order = o))
  )
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(z, -3, order = o)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(z, -3, order = o))
  )

  # Custom order and triple differencing
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(x, order = o, differences = 3)),
    lapply(list(o1, o2, o3),
           function(o) my_diff3(x, order = o))
  )
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(y, order = o, differences = 3)),
    lapply(list(o1, o2, o3),
           function(o) my_diff3(y, order = o))
  )
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(z, order = o, differences = 3)),
    lapply(list(o1, o2, o3),
           function(o) my_diff3(z, order = o))
  )

  # Custom order and run_lengths
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(x, order = o, run_lengths = rl2)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(x, order = o, run_lengths = rl2))
  )
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(z, order = o, run_lengths = rl2)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(z, order = o, run_lengths = rl2))
  )
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(z, order = o, run_lengths = rl2)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(z, order = o, run_lengths = rl2))
  )

  # Custom order, negative lags and run_lengths
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(x, -2, order = o, run_lengths = rl2)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(x, -2, order = o, run_lengths = rl2))
  )
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(z, -2, order = o, run_lengths = rl2)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(z, -2, order = o, run_lengths = rl2))
  )
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(z, -2, order = o, run_lengths = rl2)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(z, -2, order = o, run_lengths = rl2))
  )

  # Custom order, lag vector and run_lengths
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(x, lags, order = o, run_lengths = rl2)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(x, lags, order = o, run_lengths = rl2))
  )
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(z, lags, order = o, run_lengths = rl2)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(z, lags, order = o, run_lengths = rl2))
  )
  expect_equal(
    lapply(list(o1, o2, o3),
           function(o) diff_(z, lags, order = o, run_lengths = rl2)),
    lapply(list(o1, o2, o3),
           function(o) my_diff(z, lags, order = o, run_lengths = rl2))
  )
})

