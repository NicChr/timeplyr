# Set number of data.table threads to 2
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("Normal cases", {
  x <- seq(0.5, 25, 0.5)
  y <- 2 * 1.075^(0:100)
  z <- c(1:3, NA, NA, 4, 5, 6, 7, 8, NA, 9, 10, NA)
  zz <- c(z, 11)
  set.seed(98078123)
  a <- sample(c(0, 1), size = 50, replace = TRUE)
  expect_error(roll_growth_rate(x, window = 1:3))
  expect_equal(growth_rate(c(0, 0)), 1)
  expect_equal(growth_rate(c(0, 0), log = TRUE), 1)
  expect_equal(growth_rate(c(0, 1), log = FALSE), Inf)
  expect_equal(growth_rate(c(0, -1), log = FALSE), -Inf)
  expect_equal(growth_rate(c(0, 1), log = TRUE), Inf)
  expect_equal(growth_rate(c(0, 1), log = TRUE, inf_fill = NA), NA_real_)
  expect_warning(growth_rate(c(0, -1), log = TRUE))
  expect_equal(growth_rate(c(0, 1), inf_fill = 99), 99)
  expect_equal(
    (0.5 * (growth_rate(x))^(length(x) - 1)),
    25
  )
  expect_equal(growth_rate(z, na.rm = FALSE), NA_real_)
  expect_equal(growth_rate(z, na.rm = TRUE), NA_real_)
  expect_equal(
    growth_rate(zz, na.rm = TRUE),
    growth_rate(zz[!is.na(zz)])
  )

  # expect_warning(roll_growth_rate(y, na.rm = TRUE))
  # expect_warning(roll_growth_rate(y, na.rm = FALSE))
  expect_equal(
    roll_growth_rate(y, window = 1),
    rep_len(1L, length(y))
  )
  expect_equal(
    roll_growth_rate(y, window = 1, partial = FALSE),
    rep_len(1L, length(y))
  )
  expect_equal(
    roll_growth_rate(y, window = 2),
    c(1, rep_len(1.075, length(y) - 1))
  )
  expect_equal(
    roll_growth_rate(y, window = 2, partial = FALSE),
    c(NA, rep_len(1.075, length(y) - 1))
  )
  expect_equal(
    roll_growth_rate(y, window = 3),
    c(1, rep_len(1.075, length(y) - 1))
  )
  expect_equal(
    roll_growth_rate(y, window = 3, partial = FALSE),
    c(NA, NA, rep_len(1.075, length(y) - 2))
  )
  expect_equal(
    roll_growth_rate(y, window = 3, partial = FALSE, log = TRUE),
    c(NA, NA, rep_len(1.075, length(y) - 2))
  )


  # expect_equal(roll_growth_rate(z, window = 3, na.rm = TRUE),
  #                        roll_growth_rate(z[!is.na(z)], window = 3, na.rm = FALSE))

  gr <- numeric(length(z))
  for (i in seq_along(z)) {
    gr[i] <- growth_rate(z[seq_len(i)])
  }
  expect_equal(
    roll_growth_rate(z),
    gr
  )
  expect_error(roll_growth_rate(x, window = 1:length(x)))
  expect_error(roll_growth_rate(x, window = 0))
  # expect_equal(roll_growth_rate(x, window = 1:length(x)),
  #                        roll_growth_rate(x, window = length(x)))
  # expect_equal(roll_growth_rate(x, window = 1:length(x),
  #                                            partial = FALSE),
  #                        roll_growth_rate(x, window = length(x),
  #                                            partial = TRUE))

  a_gr <- roll_growth_rate(a, window = 2)
  a_gr[is.infinite(a_gr)] <- 99
  expect_equal(
    a_gr,
    roll_growth_rate(a,
      window = 2,
      inf_fill = 99
    )
  )
  a_gr[a_gr == 99] <- NA_real_
  expect_equal(
    a_gr,
    roll_growth_rate(a,
      window = 2,
      inf_fill = NA
    )
  )

  expect_true(all(a_gr[a == 0 & dplyr::lag(a) == 0] == 1,
    na.rm = TRUE
  ))
})
