testthat::test_that("Normal cases", {
  x <- seq(0.5, 25, 0.5)
  y <- 2 * 1.075^(0:100)
  z <- c(1:3, NA, NA, 4, 5, 6, 7, 8, NA, 9, 10, NA)
  set.seed(98078123)
  a <- sample(c(0, 1), size = 50, replace = TRUE)
  testthat::expect_error(rolling_growth_rate(x, n = 1:3))
  testthat::expect_equal(growth_rate(c(0, 0)), 1)
  testthat::expect_equal(growth_rate(c(0, 0), log = TRUE), 1)
  testthat::expect_equal(growth_rate(c(0, 1), log = FALSE), Inf)
  testthat::expect_equal(growth_rate(c(0, -1), log = FALSE), -Inf)
  testthat::expect_equal(growth_rate(c(0, 1), log = TRUE), Inf)
  testthat::expect_equal(growth_rate(c(0, 1), log = TRUE, inf_fill = NA), NA_real_)
  testthat::expect_warning(growth_rate(c(0, -1), log = TRUE))
  testthat::expect_equal(growth_rate(c(0, 1), inf_fill = 99), 99)
  testthat::expect_equal(( 0.5 * (growth_rate(x))^(length(x) - 1) ),
                         25)
  testthat::expect_equal(growth_rate(z, na.rm = FALSE), NA_real_)
  testthat::expect_equal(growth_rate(z, na.rm = TRUE),
                         growth_rate(z[!is.na(z)]))

  # testthat::expect_warning(rolling_growth_rate(y, na.rm = TRUE))
  # testthat::expect_warning(rolling_growth_rate(y, na.rm = FALSE))
  testthat::expect_equal(rolling_growth_rate(y, n = 1),
                         seq_ones(length(y)))
  testthat::expect_equal(rolling_growth_rate(y, n = 1, partial = FALSE),
                         seq_ones(length(y)))
  testthat::expect_equal(rolling_growth_rate(y, n = 2),
                         c(1, rep_len(1.075, length(y) - 1)))
  testthat::expect_equal(rolling_growth_rate(y, n = 2, partial = FALSE),
                         c(NA, rep_len(1.075, length(y) - 1)))
  testthat::expect_equal(rolling_growth_rate(y, n = 3),
                         c(1, rep_len(1.075, length(y) - 1)))
  testthat::expect_equal(rolling_growth_rate(y, n = 3, partial = FALSE),
                         c(NA, NA, rep_len(1.075, length(y) - 2)))
  testthat::expect_equal(rolling_growth_rate(y, n = 3, partial = FALSE, log = TRUE),
                         c(NA, NA, rep_len(1.075, length(y) - 2)))


  # testthat::expect_equal(rolling_growth_rate(z, n = 3, na.rm = TRUE),
  #                        rolling_growth_rate(z[!is.na(z)], n = 3, na.rm = FALSE))

  gr <- numeric(length(z))
  for (i in seq_along(z)){
    gr[i] <- growth_rate(z[seq_len(i)])
  }
  testthat::expect_equal(rolling_growth_rate(z),
                         gr)
  # testthat::expect_equal(rolling_growth_rate(x, n = 1:length(x)),
  #                        rolling_growth_rate(x, n = length(x)))
  # testthat::expect_equal(rolling_growth_rate(x, n = 1:length(x),
  #                                            partial = FALSE),
  #                        rolling_growth_rate(x, n = length(x),
  #                                            partial = TRUE))

  a_gr <- rolling_growth_rate(a, n = 2)
  a_gr[is.infinite(a_gr)] <- 99
  testthat::expect_equal(a_gr,
                         rolling_growth_rate(a, n = 2,
                                             inf_fill = 99))
  a_gr[a_gr == 99] <- NA_real_
  testthat::expect_equal(a_gr,
                         rolling_growth_rate(a, n = 2,
                                             inf_fill = NA))

  testthat::expect_true(all(a_gr[a == 0 & dplyr::lag(a) == 0] == 1,
                            na.rm = TRUE))

})
