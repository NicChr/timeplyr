testthat::test_that("Test gmin and gmax", {
flights <- nycflights13::flights
g1 <- group_id(flights, origin, dest, tailnum)
g2 <- group_id(flights, origin, dest, tailnum, sort = FALSE)
testthat::expect_identical(flights %>%
                             dplyr::mutate(min = min(time_hour),
                                           max = max(time_hour)),
                           flights %>%
                             dplyr::mutate(min = gmin(time_hour),
                                           max = gmax(time_hour)))
testthat::expect_identical(flights %>%
                             dplyr::mutate(min = min(time_hour),
                                           max = max(time_hour),
                                           .by = c(origin, dest, tailnum)),
                           flights %>%
                             dplyr::mutate(min = gmin(time_hour, g = g1),
                                           max = gmax(time_hour, g = g1)))
testthat::expect_identical(flights %>%
                             dplyr::mutate(min = gmin(time_hour, g = g1),
                                           max = gmax(time_hour, g = g1)),
                           flights %>%
                             dplyr::mutate(min = gmin(time_hour, g = g2),
                                           max = gmax(time_hour, g = g2)))
})
