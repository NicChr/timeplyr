
testthat::test_that("edf", {
set.seed(819384556)
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

# With weights
df1 <- dplyr::tibble(x) %>%
  dplyr::mutate(edf1 = edf(x)) %>%
  fcount(x, edf1) %>%
  dplyr::slice_sample(n = nrow2(.)) %>%
  dplyr::mutate(edf2 = edf(x, wt = n))
testthat::expect_equal(df1$edf1,
                       df1$edf2)

df2 <- dplyr::tibble(x, g) %>%
  dplyr::mutate(edf1 = edf(x, g = g)) %>%
  fcount(x, g, edf1) %>%
  dplyr::slice_sample(n = nrow2(.)) %>%
  dplyr::mutate(edf2 = edf(x, wt = n, g = g))

testthat::expect_equal(df2$edf1,
                       df2$edf2)

x[sample(seq_len(length(x)), size = 10^3)] <- NA

testthat::expect_equal(edf(x), stats::ecdf(x)(x))
testthat::expect_equal(edf(x), dplyr::cume_dist(x))

edf1 <- dplyr::tibble(x, g) %>%
  dplyr::mutate(edf = dplyr::cume_dist(x),
                .by = g) %>%
  dplyr::pull(edf)
edf2 <- edf(x, g = g)
testthat::expect_equal(edf1, edf2)

# With weights
df3 <- dplyr::tibble(x) %>%
  dplyr::mutate(edf1 = edf(x)) %>%
  fcount(x, edf1) %>%
  dplyr::slice_sample(n = nrow2(.)) %>%
  dplyr::mutate(edf2 = edf(x, wt = n))
testthat::expect_equal(df3$edf1,
                       df3$edf2)

df4 <- dplyr::tibble(x, g) %>%
  dplyr::mutate(edf1 = edf(x, g = g)) %>%
  fcount(x, g, edf1) %>%
  dplyr::slice_sample(n = nrow2(.)) %>%
  dplyr::mutate(edf2 = edf(x, wt = n, g = g))
testthat::expect_equal(df4$edf1,
                       df4$edf2)

testthat::expect_equal(edf(x, g = g, wt = 1),
                       edf(x, g = g))
testthat::expect_equal(edf(x, wt = 1),
                       edf(x))

testthat::expect_error(edf(x, wt = 1:3))
})
