
# testthat::test_that("Expect zero length vector", {
#   x <- list(NULL, numeric(0), integer(0), logical(0))
#   testthat::expect_identical(lapply(x, function(y) frollsum2(y, n = 1)),
#                              list(NULL, numeric(0), integer(0), logical(0)))
#   testthat::expect_identical(lapply(x, function(y) frollmean2(y, n = 1)),
#                              list(NULL, numeric(0), integer(0), logical(0)))
# })

testthat::test_that("Expect error", {
  testthat::expect_error(frollsum2(NULL))
  testthat::expect_error(frollmean2(NULL))
  testthat::expect_error(frollsum2(character(0)))
  testthat::expect_error(frollmean2(character(0)))
  testthat::expect_error(frollsum2(1:10, partial = c(TRUE, FALSE)))
  testthat::expect_error(frollmean2(1:10, partial = c(TRUE, FALSE)))
  # testthat::expect_error(frollsum2(Sys.Date()))
  # testthat::expect_error(frollmean2(Sys.Date()))
  testthat::expect_error(frollsum2(1:10, n = 0))
  testthat::expect_error(frollmean2(1:10, n = 0))
  testthat::expect_error(frollsum2(1:10, n = -1))
  testthat::expect_error(frollmean2(1:10, n = -1))
  testthat::expect_error(frollsum2(1:10, n = -2.4))
  testthat::expect_error(frollmean2(1:10, n = -2.4))
  testthat::expect_error(frollsum2(1:10, n = 0.99))
  testthat::expect_error(frollmean2(1:10, n = 0.99))
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
