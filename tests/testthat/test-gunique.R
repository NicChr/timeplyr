testthat::test_that("Test grouped unique", {
flights <- nycflights13::flights
g1 <- group_id(flights, origin, dest)
g2 <- group_id(flights, origin, dest, sort = FALSE)


testthat::expect_equal(gunique(numeric(0)),
                       numeric(0))
x <- sample(1:20, size = 10^5, replace = TRUE)
g <- sample(sample(letters, size = 5), size = 10^5, replace = TRUE)
testthat::expect_equal(gunique(x),
                       unique(x))
testthat::expect_equal(gunique(x, g = g),
                       dplyr::tibble(x, g) %>%
                         dplyr::group_by(g) %>%
                         dplyr::distinct(x) %>%
                         dplyr::pull(x))
# testthat::expect_equal(gunique(dplyr::select(iris)),
#                        dplyr::distinct(dplyr::select(iris)))
testthat::expect_equal(flights,
                       gunique(flights))
testthat::expect_equal(flights %>%
                         dplyr::mutate(g1) %>%
                         dplyr::group_by(g1) %>%
                         dplyr::distinct(year, month, day) %>%
                         dplyr::ungroup() %>%
                         dplyr::select(year, month, day),
                       flights %>%
                         dplyr::select(year, month, day) %>%
                         gunique(g = g1))
testthat::expect_equal(flights %>%
                         dplyr::mutate(g1) %>%
                         dplyr::group_by(g1) %>%
                         dplyr::distinct(year, month, day) %>%
                         dplyr::ungroup() %>%
                         dplyr::select(year, month, day),
                       flights %>%
                         dplyr::select(year, month, day) %>%
                         gunique(g = g2))

})