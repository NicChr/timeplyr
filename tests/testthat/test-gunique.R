# Set number of data.table threads to 1
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Test grouped unique", {
flights <- nycflights13::flights
g1 <- group_id(flights, origin, dest)
g2 <- group_id(flights, origin, dest, order = FALSE)


testthat::expect_equal(gunique(numeric(0)),
                       numeric(0))
set.seed(42)
x <- sample(1:20, size = 10^5, replace = TRUE)
g <- sample(sample(letters, size = 5), size = 10^5, replace = TRUE)
testthat::expect_equal(gunique(x),
                       unique(x))
testthat::expect_equal(gunique(x, g = g, use.g.names = FALSE),
                       dplyr::tibble(x, g) %>%
                         dplyr::group_by(g) %>%
                         dplyr::distinct(x) %>%
                         dplyr::pull(x))
# testthat::expect_equal(gunique(dplyr::select(iris)),
#                        dplyr::distinct(dplyr::select(iris)))
testthat::expect_equal(flights,
                       gunique(flights))
# testthat::expect_error(flights %>%
#                          dplyr::select(year, month, day) %>%
#                          gunique(g = g1))
# testthat::expect_error(flights %>%
#                          dplyr::select(year, month, day) %>%
#                          gunique(g = g2))
testthat::expect_equal(flights %>%
                         dplyr::mutate(g1) %>%
                         dplyr::group_by(g1) %>%
                         dplyr::distinct(year, month, day) %>%
                         dplyr::ungroup() %>%
                         dplyr::select(year, month, day),
                       flights %>%
                         dplyr::select(year, month, day) %>%
                         gunique(g = g1, use.g.names = FALSE))
testthat::expect_equal(flights %>%
                         dplyr::mutate(g1) %>%
                         dplyr::group_by(g1) %>%
                         dplyr::distinct(year, month, day) %>%
                         dplyr::ungroup() %>%
                         dplyr::select(year, month, day),
                       flights %>%
                         dplyr::select(year, month, day) %>%
                         gunique(g = g2, use.g.names = FALSE))
})
