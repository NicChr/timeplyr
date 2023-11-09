# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Expect error", {
  testthat::expect_error(roll_sum(NA_character_))
  testthat::expect_error(roll_mean(NA_character_))
})


testthat::test_that("Expect NA", {
  x <- list(NA, NA_real_, NA_integer_, NaN, Inf, -Inf)
  testthat::expect_identical(lapply(x, function(y) roll_sum(y, na.rm = FALSE)),
                             lapply(1:length(x), function(x) NA_real_))
  testthat::expect_identical(lapply(x, function(y) roll_mean(y, na.rm = FALSE)),
                             lapply(1:length(x), function(x) NA_real_))
})

testthat::test_that("Expected outputs", {
  x <- seq(-5, 5, 0.25)
  testthat::expect_identical(roll_sum(x, window = length(x)),
                             as.numeric(cumsum(x)))
  testthat::expect_identical(roll_mean(x, window = length(x)),
                             as.numeric(dplyr::cummean(x)))
  testthat::expect_identical(roll_sum(x, window = length(x), partial = FALSE),
                             data.table::frollsum(x, n = length(x)))
  testthat::expect_identical(roll_mean(x, window = length(x), partial = FALSE),
                             data.table::frollmean(x, n = length(x)))
  testthat::expect_identical(roll_sum(x, window = 6, partial = FALSE),
                             data.table::frollsum(x, n = 6))
  testthat::expect_identical(roll_mean(x, window = 6, partial = FALSE),
                             data.table::frollmean(x, n = 6))
  x[sample(1:length(x), size = 10)] <- NA_real_
  testthat::expect_identical(roll_sum(x, window = 5, na.rm = TRUE, partial = FALSE),
                             data.table::frollsum(x, n = 5, na.rm = TRUE))
  testthat::expect_identical(roll_sum(x, window = 5, na.rm = FALSE, partial = FALSE),
                             data.table::frollsum(x, n = 5, na.rm = FALSE))
  testthat::expect_identical(roll_mean(x, window = 5, na.rm = TRUE, partial = FALSE),
                             data.table::frollmean(x, n = 5, na.rm = TRUE))
  testthat::expect_identical(roll_mean(x, window = 5, na.rm = FALSE, partial = FALSE),
                             data.table::frollmean(x, n = 5, na.rm = FALSE))
})

# testthat::test_that("Expected outputs2", {
#  flights <- nycflights13::flights
#  x <- flights$arr_delay
#  g <- flights$dest
#  t <- seq_along(x)
#  dt <- data.table::data.table(x, g, t)
#
#  expected <- dt[, mu2 := data.table::frollmean(x, n = 5, na.rm = TRUE),
#                 by = "g"]$mu2
#  testthat::expect_true(all.equal(dt[, mu1 := roll_mean(x, g = g, window = 5, partial = FALSE)]$mu1,
#                                  expected))
#
#  dt[, mu1 := time_roll_mean(x, g = g, window = 5,
#                             time = t,
#                             close_left_boundary = TRUE,
#                             partial = FALSE,
#                             na.rm = TRUE)]
#  dt[, mu2 := slider::slide_index_mean(x, i = t,
#                                       before = 5,
#                                       complete = TRUE,
#                                       na_rm = TRUE),
#     by = "g"]
#  testthat::expect_equal(dt$mu1, dt$mu2)
#  dt[, mu1 := time_roll_mean(x, g = g, window = 5,
#                             time = frowid(x, g = g),
#                             close_left_boundary = TRUE,
#                             partial = FALSE,
#                             na.rm = TRUE)]
#  dt[, mu2 := slider::slide_index_mean(x, i = seq_along(x),
#                                       before = 5,
#                                       complete = TRUE,
#                                       na_rm = TRUE),
#     by = "g"]
#  testthat::expect_equal(dt$mu1, dt$mu2)
# })
#
# t <- time_seq_v2(100, Sys.Date(), time_by = lubridate::days(1))
#
# t <- sample(t, size = 10^3, replace = TRUE)
#
# x <- rnorm(length(t))
#
# t <- sort(t)
#
#
# # With dups ---------------------------------------------------------------
#
#
# z1 <- time_roll_mean(x, time = t, days(11), close_left_boundary = TRUE)
# z2 <- slider::slide_index_mean(x, i = t, before = days(11), na_rm = TRUE,
#                          complete = FALSE)
# z3 <- time_roll_mean(x, time = t, days(11), close_left_boundary = FALSE)
# z4 <- runner::mean_run(x, idx = t, k = 11)
# all.equal(z1, z2)
# # all.equal(z3, z4)
#
#
# # With NA -----------------------------------------------------------------
#
#
# x <- na_fill(x, prop = 0.3)
# z1 <- time_roll_mean(x, time = t, days(11), close_left_boundary = TRUE)
# z2 <- slider::slide_index_mean(x, i = t, before = days(11), na_rm = TRUE,
#                                complete = FALSE)
# all.equal(z1, z2)
#
#
# # With gaps ---------------------------------------------------------------
#
# t <- sort(sample(t, size = 20, FALSE))
# x <- rnorm(length(t))
# z1 <- time_roll_mean(x, time = t, days(11), close_left_boundary = TRUE)
# z2 <- slider::slide_index_mean(x, i = t, before = days(11), na_rm = TRUE,
#                                complete = FALSE)
# all.equal(z1, z2)
#
#
# # With gaps and dups ------------------------------------------------------
#
# t <- time_seq_v2(100, Sys.Date(), time_by = lubridate::days(1))
# t <- sort(sample(t, size = 30, TRUE))
# x <- rnorm(length(t))
# z1 <- time_roll_mean(x, time = t, days(11), close_left_boundary = TRUE)
# z2 <- slider::slide_index_mean(x, i = t, before = days(11), na_rm = TRUE,
#                                complete = FALSE)
# all.equal(z1, z2)
#
# # By group
#
#
#
# flights2 <- nycflights13::flights %>%
#   farrange(time_hour)
#
# t <- flights2$time_hour
# x <- rnorm(length(t))
# z2 <- time_roll_mean(x, dhours(2), time = t, partial = FALSE, close_left_boundary = TRUE)
# z3 <- slider::slide_index_mean(x, i = t, before = dhours(2), na_rm = TRUE,
#                                complete = TRUE)
# all.equal(z2, z3)
#
# z1 <- flights2 %>%
#   # farrange(origin, dest, time_hour) %>%
#   fgroup_by(origin, dest) %>%
#   dplyr::mutate(mean = slider::slide_index_mean(arr_delay, i = time_hour,
#                                                   before = lubridate::dhours(2.5),
#                                                   na_rm = TRUE)) %>%
#   dplyr::pull(mean)
# z2 <- flights2 %>%
#   add_group_id(origin, dest) %>%
#   dplyr::mutate(mean = time_roll_mean(arr_delay, time = time_hour,
#                                       lubridate::dhours(2.5),
#                                       close_left_boundary = TRUE,
#                                       g = group_id)) %>%
#   dplyr::pull(mean)
# all.equal(z1, z2)

testthat::test_that("simple tests", {
  testthat::expect_equal(
    time_roll_mean(c(10, 20, 30), time = lubridate::today() + lubridate::days(0:2),
                   window = lubridate::days(1),
                   close_left_boundary = FALSE),
    c(10, 20, 30))
  testthat::expect_equal(
    time_roll_mean(c(10, 20, 30), time = lubridate::today() + lubridate::days(0:2),
                   window = lubridate::days(1),
                   close_left_boundary = TRUE),
    c(10, 15, 25))


  # x <- rnorm(10^6)
  # t <- sample(time_seq(today(), today() + weeks(23), time_by = days(9)),
  #             size = 10^6, TRUE)
  # g <- sample.int(10^5, 10^6, TRUE)
  #
  # mark(e1 = time_roll_sum(x, time = t, window = 11, g = g))
  #      # e2 = time_roll_sum2(x, time = t, window = 11, g = g))
  # mark(e1 = time_roll_sum(x, time = t, window = 11, g = g,
  #                         close_left_boundary = TRUE),
  #      e2 = time_roll_sum2(x, time = t, window = 11, g = g,
  #                          close_left_boundary = TRUE))
  # mark(e1 = time_roll_sum(x, time = t, window = 7, g = g),
  #      e2 = time_roll_sum2(x, time = t, window = 7, g = g))
})


