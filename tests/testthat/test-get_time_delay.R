
testthat::test_that("edf", {
x <- sample(seq(-10, 10, 0.5), size = 10^5, replace = TRUE)
testthat::expect_equal(edf(x), stats::ecdf(x)(x))
testthat::expect_equal(edf(x), dplyr::cume_dist(x))
g <- sample(letters[1:5], size = 10^5, replace = TRUE)

edf1 <- dplyr::tibble(x, g) %>%
  dplyr::mutate(edf = dplyr::cume_dist(x),
         .by = g) %>%
  dplyr::pull(edf)
edf2 <- edf(x, g = g)
testthat::expect_equal(edf1, edf2)

x[sample(seq_len(length(x)), size = 10^3)] <- NA

testthat::expect_equal(edf(x), stats::ecdf(x)(x))
testthat::expect_equal(edf(x), dplyr::cume_dist(x))

edf1 <- dplyr::tibble(x, g) %>%
  dplyr::mutate(edf = dplyr::cume_dist(x),
                .by = g) %>%
  dplyr::pull(edf)
edf2 <- edf(x, g = g)
testthat::expect_equal(edf1, edf2)
})

x <- sample(seq(-10, 10, 0.5), size = 10^7, replace = TRUE)
g <- sample(1:10^6, size = 10^7, replace = TRUE)
edf(x)
library(dplyr)
cume_dist(x)
