testthat::test_that("Test gmin and gmax", {
flights <- nycflights13::flights
g1 <- group_id(flights, origin, dest)
g2 <- group_id(flights, origin, dest, sort = FALSE)
testthat::expect_identical(flights %>%
                             dplyr::mutate(min = min(time_hour),
                                           max = max(time_hour),
                                           mean = mean(time_hour),
                                           sum = sum(sched_arr_time, na.rm = TRUE)),
                           flights %>%
                             dplyr::mutate(min = gmin(time_hour),
                                           max = gmax(time_hour),
                                           mean = gmean(time_hour),
                                           sum = gsum(sched_arr_time, na.rm = TRUE)))
testthat::expect_identical(flights %>%
                             dplyr::mutate(min = min(time_hour),
                                           max = max(time_hour),
                                           mean = mean(time_hour),
                                           sum = sum(sched_arr_time, na.rm = TRUE),
                                           .by = c(origin, dest)),
                           flights %>%
                             dplyr::mutate(min = gmin(time_hour, g = g1),
                                           max = gmax(time_hour, g = g1),
                                           mean = gmean(time_hour, g = g1),
                                           sum = gsum(sched_arr_time, na.rm = TRUE,
                                                      g = g1)))
testthat::expect_identical(flights %>%
                             dplyr::mutate(min = gmin(time_hour, g = g1),
                                           max = gmax(time_hour, g = g1),
                                           mean = gmean(time_hour, g = g1),
                                           sum = gsum(sched_arr_time, na.rm = TRUE,
                                                      g = g1)),
                           flights %>%
                             dplyr::mutate(min = gmin(time_hour, g = g2),
                                           max = gmax(time_hour, g = g2),
                                           mean = gmean(time_hour, g = g2),
                                           sum = gsum(sched_arr_time, na.rm = TRUE,
                                                      g = g2)))
})
