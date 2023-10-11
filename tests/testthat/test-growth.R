# Set number of data.table threads to 1
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Normal cases", {
  x <- 1:10
  testthat::expect_identical(growth(x, 3.75),
                             3.75/mean(x))
  testthat::expect_identical(growth(x, 3.75, log = TRUE),
                             3.75/mean(x))
  testthat::expect_identical(growth(0, 0),
                             1)
  testthat::expect_identical(growth(0, 1),
                             Inf)
  testthat::expect_identical(growth(0, 1, inf_fill = NA),
                             NA_real_)
  testthat::expect_identical(growth(0, 1, inf_fill = 99),
                             99)
  testthat::expect_identical(rolling_growth(c(0, x), n = 1, lag = 1),
                             c(0, x)/dplyr::lag(c(0, x)))
  testthat::expect_identical(rolling_growth(c(0, x), n = 1, lag = 1,
                                            inf_fill = 99),
                             {
                               y <- c(0, x)/dplyr::lag(c(0, x))
                               y[is.infinite(y)] <- 99
                               y
                               })
  testthat::expect_identical(rolling_growth(c(0, x), n = 1, lag = 1,
                                            inf_fill = NA),
                             {
                               y <- c(0, x)/dplyr::lag(c(0, x))
                               y[is.infinite(y)] <- NA
                               y
                             })
  testthat::expect_identical(rolling_growth(x, n = 1, lag = 0),
                             rep(1, length(x)))
  testthat::expect_identical(rolling_growth(x, n = 1, lag = 1),
                             x/dplyr::lag(x))
  testthat::expect_identical(rolling_growth(x, n = 1, lag = 2),
                             x/dplyr::lag(x, n = 2))
  testthat::expect_identical(rolling_growth(x, n = 1, lag = 3),
                             x/dplyr::lag(x, n = 3))
  testthat::expect_identical(rolling_growth(x, n = 3, lag = 3, partial = FALSE),
                             roll_mean(x, window = 3, partial = FALSE)/
                               roll_mean(dplyr::lag(x, n = 3), window = 3, partial = FALSE,
                                        na.rm = FALSE))
  testthat::expect_identical(rolling_growth(x, n = 3, lag = 2, partial = FALSE),
                             roll_mean(x, window = 3, partial = FALSE, na.rm = FALSE)/
                               roll_mean(dplyr::lag(x, n = 2), window = 3, partial = FALSE,
                                        na.rm = FALSE))
})

testthat::test_that("Weights and offsets", {
  set.seed(61092)
  x <- sample(1:10^5, size = 10)
  x1 <- x[1:5]
  x2 <- x[6:10]
  w <- rnorm(10)^2
  o <- rnorm(10)^2
  o1 <- o[1:5]
  o2 <- o[6:10]
  w1 <- w[1:5]
  w2 <- w[6:10]
  sum1 <- sum(x1)
  sum2 <- sum(x2)
  wsum1 <- collapse::fsum(x1, w = w1)
  wsum2 <- collapse::fsum(x2, w = w2)
  cp <- c(rep(0, length(x1)), rep(1, length(x2)))
  # No weights/offset
  mod1 <- stats::glm(x ~ cp, family = poisson)
  sum(x2) / sum(x1)
  testthat::expect_equal(exp(stats::coef(mod1))[[2]], rolling_growth(x, n = 5)[10])

  # Weights and offset
  mod2 <- stats::glm(x ~ cp + offset(log(o)), family = poisson, weights = w)
  ( ( sum(x2 * w2)/sum(o2 * w2) ) / (sum(x1 * w1)/sum(o1 * w1))  )
  ( ( stats::weighted.mean(x2, w = w2)/stats::weighted.mean(o2, w2) ) /
      (stats::weighted.mean(x1, w1)/stats::weighted.mean(o1, w1))  )
  testthat::expect_equal(exp(stats::coef(mod2))[[2]],
                         rolling_growth(x, n = 5, weights = w, offset = o)[10])
  testthat::expect_equal(exp(stats::coef(mod2))[[2]],
                         rolling_growth(x, n = 5, weights = w, offset = o,
                                        log = TRUE)[10])
  testthat::expect_equal(exp(stats::coef(mod2))[[2]],
                         rolling_growth(x, n = 5, weights = w, offset = o,
                                        log = FALSE, partial = FALSE)[10])

  # Weights only
  mod3 <- stats::glm(x ~ cp, family = poisson, weights = w)
  ( ( collapse::fmean(x2, w = w2)) / (collapse::fmean(x1, w = w1))  )
  testthat::expect_equal(exp(stats::coef(mod3))[[2]],
                         rolling_growth(x, n = 5, weights = w)[10])
  # Offset only
  mod4 <- stats::glm(x ~ cp + offset(log(o)), family = poisson)
  ( sum(x2)/sum(o2) ) / ( sum(x1)/sum(o1) )
  testthat::expect_equal(exp(stats::coef(mod4))[[2]],
                         rolling_growth(x, n = 5, offset = o, na.rm = TRUE)[10])

  # WITH NAs

  set.seed(61092)
  x <- sample(1:10^5, size = 100)
  x[sample(1:100, size = 30)] <- NA_integer_
  # x <- x[!is.na(x)]
  x1 <- x[1:50]
  x2 <- x[51:100]
  w <- rnorm(100)^2
  o <- rnorm(100)^2
  o1 <- o[1:50]
  o2 <- o[51:100]
  w1 <- w[1:50]
  w2 <- w[51:100]
  sum1 <- sum(x1, na.rm = TRUE)
  sum2 <- sum(x2, na.rm = TRUE)
  wsum1 <- collapse::fsum(x1, w = w1, na.rm = TRUE)
  wsum2 <- collapse::fsum(x2, w = w2, na.rm = TRUE)
  cp <- c(rep(0, length(x1)), rep(1, length(x2)))
  # No weights/offset
  mod1 <- stats::glm(x ~ cp, family = poisson)
  sum(x2) / sum(x1)
  testthat::expect_equal(exp(stats::coef(mod1))[[2]], rolling_growth(x, n = 50, na.rm = TRUE)[100])

  # # Weights and offset
  mod2 <- stats::glm(x ~ cp + offset(log(o)), family = poisson, weights = w)
  ( ( sum(x2 * w2, na.rm = TRUE)/sum(o2 * w2, na.rm = TRUE) ) / (sum(x1 * w1, na.rm = TRUE)/sum(o1 * w1, na.rm = TRUE))  )
  ( ( stats::weighted.mean(x2, w = w2, na.rm = TRUE)/stats::weighted.mean(o2, w2, na.rm = TRUE) ) /
      (stats::weighted.mean(x1, w1, na.rm = TRUE)/stats::weighted.mean(o1, w1, na.rm = TRUE))  )
  testthat::expect_equal(exp(stats::coef(mod2))[[2]],
                         rolling_growth(x, n = 50, weights = w, offset = o, na.rm = TRUE)[100])

  # Weights only
  mod3 <- stats::glm(x ~ cp, family = poisson, weights = w)
  ( ( collapse::fmean(x2, w = w2)) / (collapse::fmean(x1, w = w1))  )
  testthat::expect_equal(exp(stats::coef(mod3))[[2]],
                         rolling_growth(x, n = 50, weights = w, na.rm = TRUE)[100])
  testthat::expect_equal(exp(stats::coef(mod3))[[2]],
                         rolling_growth(x, n = 50, log = TRUE,
                                        weights = w, na.rm = TRUE)[100])
  # Offset only
  mod4 <- stats::glm(x ~ cp + offset(log(o)), family = poisson)
  ( sum(x2)/sum(o2) ) / ( sum(x1)/sum(o1) )
  testthat::expect_equal(exp(stats::coef(mod4))[[2]],
                         rolling_growth(x, n = 50, offset = o, na.rm = TRUE)[100])
})
