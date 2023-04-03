testthat::test_that("Expect error", {
  testthat::expect_error(frollsum2(NA_character_))
  testthat::expect_error(frollmean2(NA_character_))
})


testthat::test_that("Expect NA", {
  x <- list(NA, NA_real_, NA_integer_, NaN, Inf, -Inf)
  testthat::expect_identical(lapply(x, function(y) frollsum2(y)),
                             lapply(1:length(x), function(x) NA_real_))
  testthat::expect_identical(lapply(x, function(y) frollmean2(y)),
                             lapply(1:length(x), function(x) NA_real_))
})

testthat::test_that("Expected outputs", {
  x <- seq(-5, 5, 0.25)
  testthat::expect_identical(frollsum2(x, n = length(x)),
                             as.numeric(cumsum(x)))
  testthat::expect_identical(frollmean2(x, n = length(x)),
                             as.numeric(dplyr::cummean(x)))
  testthat::expect_identical(frollsum2(x, n = length(x), partial = FALSE),
                             data.table::frollsum(x, n = length(x)))
  testthat::expect_identical(frollmean2(x, n = length(x), partial = FALSE),
                             data.table::frollmean(x, n = length(x)))
  testthat::expect_identical(frollsum2(x, n = 6, partial = FALSE),
                             data.table::frollsum(x, n = 6))
  testthat::expect_identical(frollmean2(x, n = 6, partial = FALSE),
                             data.table::frollmean(x, n = 6))
  x[sample(1:length(x), size = 10)] <- NA_real_
  testthat::expect_identical(frollsum2(x, n = 5, na.rm = TRUE, partial = FALSE),
                             data.table::frollsum(x, n = 5, na.rm = TRUE))
  testthat::expect_identical(frollsum2(x, n = 5, na.rm = FALSE, partial = FALSE),
                             data.table::frollsum(x, n = 5, na.rm = FALSE))
  testthat::expect_identical(frollmean2(x, n = 5, na.rm = TRUE, partial = FALSE),
                             data.table::frollmean(x, n = 5, na.rm = TRUE))
  testthat::expect_identical(frollmean2(x, n = 5, na.rm = FALSE, partial = FALSE),
                             data.table::frollmean(x, n = 5, na.rm = FALSE))
})

testthat::test_that("Expected outputs", {
  # frollmean4 <- timetk::slidify(weighted.mean, .period = 4, .align = "right",
  #                               .partial = TRUE, na.rm = FALSE)

  # frollmean4 <- timetk::slidify(function(x, w) weighted.mean(x, w = w, na.rm = TRUE), .period = 4, .align = "right",
  #                               .partial = TRUE)
  # frollmean5 <- timetk::slidify(function(x, w) weighted.mean(x, w = w, na.rm = FALSE), .period = 4, .align = "right",
  #                               .partial = TRUE)
  # frollmean6 <- timetk::slidify(function(x, w) weighted.mean(x, w = w, na.rm = TRUE), .period = 4, .align = "right",
  #                               .partial = FALSE)
  # frollmean7 <- timetk::slidify(function(x, w) weighted.mean(x, w = w, na.rm = FALSE), .period = 4, .align = "right",
  #                               .partial = FALSE)
  set.seed(112029431)
  x <- c(1, 2, NA, NA, seq(6, 10, 0.5))
  w <- rnorm(length(x))^4
  # wmeans <- c()
  # for (i in seq_len(length(x) - 3)){
  #   start <- i
  #   end <- start + 3
  #   end <- min(end, length(x))
  #   wmeans <- c(wmeans, weighted.mean(x[start:end], w = w[start:end]))
  #   # print(c(start, end))
  # }
  window <- window_seq(4, length(x), partial = TRUE)

  # Weighted sums
  wsums1 <- frollsum3(x, weights = w, n = window, adaptive = TRUE,
                      na.rm = FALSE)
  wsums2 <- frollsum2(x, weights = w, n = 4, partial = TRUE,
                      na.rm = FALSE)
  wsums3 <- frollsum3(x, weights = w, n = window, adaptive = TRUE, na.rm = TRUE)
  wsums4 <- frollsum2(x, weights = w, n = 4, partial = TRUE, na.rm = TRUE)

  wsums5 <- frollsum3(x, weights = w, n = 4, adaptive = FALSE,
                      na.rm = FALSE)
  wsums6 <- frollsum2(x, weights = w, n = 4, partial = FALSE,
                      na.rm = FALSE)
  wsums7 <- frollsum3(x, weights = w, n = 4, adaptive = FALSE,
                        na.rm = TRUE)
  wsums8 <- frollsum2(x, weights = w, n = 4, partial = FALSE,
                        na.rm = TRUE)

  testthat::expect_equal(wsums1, wsums2)
  testthat::expect_equal(wsums3, wsums4)
  testthat::expect_equal(wsums5, wsums6)
  testthat::expect_equal(wsums7, wsums8)
  # Weighted means
  wmeans1 <- frollmean3(x, weights = w, n = window, adaptive = TRUE,
                        na.rm = FALSE)
  wmeans2 <- frollmean2(x, weights = w, n = 4, partial = TRUE,
                        na.rm = FALSE)
  wmeans3 <- frollmean3(x, weights = w, n = window, adaptive = TRUE,
                        na.rm = TRUE)
  wmeans4 <- frollmean2(x, weights = w, n = 4, partial = TRUE,
                        na.rm = TRUE)

  wmeans5 <- frollmean3(x, weights = w, n = 4, adaptive = FALSE,
                        na.rm = FALSE)
  wmeans6 <- frollmean2(x, weights = w, n = 4, partial = FALSE,
                        na.rm = FALSE)
  wmeans7 <- frollmean3(x, weights = w, n = 4, adaptive = FALSE,
                        na.rm = TRUE)
  wmeans8 <- frollmean2(x, weights = w, n = 4, partial = FALSE,
                        na.rm = TRUE)

  testthat::expect_equal(wmeans1, wmeans2)
  testthat::expect_equal(wmeans3, wmeans4)
  testthat::expect_equal(wmeans5, wmeans6)
  testthat::expect_equal(wmeans7, wmeans8)
})

