
testthat::test_that("fduplicates", {
  test_df <- dplyr::tibble(x1 = c("a", "a", "b", "c", "d", "e", "a"),
                           x2 = c("a", "a", "b", "c", "a", "b", "a"),
                           x3 = c("a", "a", "b", "a", "b", "e", "a"),
                           x4 = c("a", "a", "a", "a", "b", "b", "a"),
                           group = c(1, 1, 1, 2, 2, 3, 3))

  test_df2 <- dplyr::tibble(x = c("a", NA, "c", NA, NA),
                            y = c(NA, "b", "c", NA, NA),
                            z = c(NA, "b", "c", NA, NA))
  # This doesn't work because data.table can't handle 0-columns.
  # testthat::expect_identical(fduplicates(test_df %>% dplyr::select()),
  #                            test_df %>% dplyr::select() %>% dplyr::slice(0))
  testthat::expect_identical(nrow(fduplicates(test_df, .both_ways = TRUE)), 2L)
  testthat::expect_identical(nrow(fduplicates(test_df2, .both_ways = TRUE,
                                                 .keep_na = FALSE)), 0L)
  testthat::expect_identical(fduplicates(iris, Sepal.Length, Species,
                                            .keep_all = TRUE,
                                            .both_ways = TRUE),
                             iris %>%
                               dplyr::group_by(Sepal.Length, Species) %>%
                               dplyr::filter(dplyr::n() > 1) %>%
                               safe_ungroup() %>%
                               as.data.frame())
  testthat::expect_identical(nrow(fduplicates(test_df, .both_ways = FALSE)), 1L)
  testthat::expect_identical(test_df %>%
                               fduplicates(x1, x2, x3, x4, .both_ways = TRUE) %>%
                               nrow(), 3L)
  testthat::expect_identical(test_df %>%
                               fduplicates(x1, x2, x3, x4, .both_ways = TRUE) %>%
                               nrow(), 3L)
  testthat::expect_identical(test_df %>%
                               dplyr::group_by(group) %>%
                               fduplicates(x2, .both_ways = TRUE) %>%
                               nrow(), 2L)
  testthat::expect_identical(test_df %>%
                               dplyr::group_by(group) %>%
                               fduplicates(.both_ways = TRUE) %>%
                               nrow(), 2L)
  testthat::expect_identical(test_df %>%
                               fduplicates(x2, .both_ways = TRUE) %>%
                               nrow(), 6L)

  testthat::expect_identical(test_df %>%
                               fduplicates(x1, x2, .both_ways = TRUE,
                                           .keep_all = TRUE,
                                           .add_count = TRUE),
                               test_df %>%
                               dplyr::add_count(x1, x2) %>%
                               dplyr::filter(n > 1))
  testthat::expect_identical(test_df %>%
                               fduplicates(x1, x2, .both_ways = TRUE,
                                           .keep_all = FALSE,
                                           .add_count = TRUE),
                             test_df %>%
                               dplyr::select(x1, x2) %>%
                               dplyr::add_count(x1, x2) %>%
                               dplyr::filter(n > 1))
})
testthat::test_that("fduplicates2", {
  test_df <- dplyr::tibble(x1 = c("a", "a", "b", "c", "d", "e", "a"),
                           x2 = c("a", "a", "b", "c", "a", "b", "a"),
                           x3 = c("a", "a", "b", "a", "b", "e", "a"),
                           x4 = c("a", "a", "a", "a", "b", "b", "a"),
                           group = c(1, 1, 1, 2, 2, 3, 3))

  test_df2 <- dplyr::tibble(x = c("a", NA, "c", NA, NA),
                            y = c(NA, "b", "c", NA, NA),
                            z = c(NA, "b", "c", NA, NA))
  testthat::expect_identical(fduplicates2(test_df %>% dplyr::select(), .both_ways = TRUE),
                             test_df %>% dplyr::select())
  testthat::expect_identical(nrow(fduplicates2(test_df, .both_ways = TRUE)), 2L)
  testthat::expect_identical(nrow(fduplicates2(test_df2, .both_ways = TRUE,
                                              .keep_na = FALSE)), 0L)
  testthat::expect_identical(fduplicates2(iris, Sepal.Length, Species,
                                         .keep_all = TRUE,
                                         .both_ways = TRUE),
                             iris %>%
                               dplyr::group_by(Sepal.Length, Species) %>%
                               dplyr::filter(dplyr::n() > 1) %>%
                               safe_ungroup() %>%
                               as.data.frame())
  testthat::expect_identical(nrow(fduplicates2(test_df, .both_ways = FALSE)), 1L)
  testthat::expect_identical(test_df %>%
                               fduplicates2(x1, x2, x3, x4, .both_ways = TRUE) %>%
                               nrow(), 3L)
  testthat::expect_identical(test_df %>%
                               fduplicates2(x1, x2, x3, x4, .both_ways = TRUE) %>%
                               nrow(), 3L)
  testthat::expect_identical(test_df %>%
                               dplyr::group_by(group) %>%
                               fduplicates2(x2, .both_ways = TRUE) %>%
                               nrow(), 2L)
  testthat::expect_identical(test_df %>%
                               dplyr::group_by(group) %>%
                               fduplicates2(.both_ways = TRUE) %>%
                               nrow(), 2L)
  testthat::expect_identical(test_df %>%
                               fduplicates2(x2, .both_ways = TRUE) %>%
                               nrow(), 6L)

  testthat::expect_identical(test_df %>%
                               fduplicates2(x1, x2, .both_ways = TRUE,
                                           .keep_all = TRUE,
                                           .add_count = TRUE),
                             test_df %>%
                               dplyr::add_count(x1, x2) %>%
                               dplyr::filter(n > 1))
  testthat::expect_identical(test_df %>%
                               fduplicates2(x1, x2, .both_ways = TRUE,
                                           .keep_all = FALSE,
                                           .add_count = TRUE),
                             test_df %>%
                               dplyr::select(x1, x2) %>%
                               dplyr::add_count(x1, x2) %>%
                               dplyr::filter(n > 1))
})
