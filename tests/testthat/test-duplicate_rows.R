
testthat::test_that("duplicate_rows", {
  test_df <- dplyr::tibble(x1 = c("a", "a", "b", "c", "d", "e", "a"),
                           x2 = c("a", "a", "b", "c", "a", "b", "a"),
                           x3 = c("a", "a", "b", "a", "b", "e", "a"),
                           x4 = c("a", "a", "a", "a", "b", "b", "a"),
                           group = c(1, 1, 1, 2, 2, 3, 3))

  test_df2 <- dplyr::tibble(x = c("a", NA, "c", NA, NA),
                            y = c(NA, "b", "c", NA, NA),
                            z = c(NA, "b", "c", NA, NA))
  # This doesn't work because data.table can't handle 0-columns.
  # testthat::expect_identical(duplicate_rows(test_df %>% dplyr::select()),
  #                            test_df %>% dplyr::select() %>% dplyr::slice(0))
  testthat::expect_identical(nrow(duplicate_rows(test_df, .both_ways = TRUE)), 2L)
  testthat::expect_identical(nrow(duplicate_rows(test_df2, .both_ways = TRUE,
                                                 .drop_empty = TRUE)), 0L)
  testthat::expect_identical(duplicate_rows(iris, Sepal.Length, Species,
                                            .keep_all = TRUE,
                                            .both_ways = TRUE),
                             iris %>%
                               dplyr::group_by(Sepal.Length, Species) %>%
                               dplyr::filter(dplyr::n() > 1) %>%
                               safe_ungroup() %>%
                               as.data.frame())
  testthat::expect_identical(nrow(duplicate_rows(test_df, .both_ways = FALSE)), 1L)
  testthat::expect_identical(test_df %>%
                               duplicate_rows(x1, x2, x3, x4, .both_ways = TRUE) %>%
                               nrow(), 3L)
  testthat::expect_identical(test_df %>%
                               duplicate_rows(x1, x2, x3, x4, .both_ways = TRUE) %>%
                               nrow(), 3L)
  testthat::expect_identical(test_df %>%
                               dplyr::group_by(group) %>%
                               duplicate_rows(x2, .both_ways = TRUE) %>%
                               nrow(), 2L)
  testthat::expect_identical(test_df %>%
                               dplyr::group_by(group) %>%
                               duplicate_rows(.both_ways = TRUE) %>%
                               nrow(), 2L)
  testthat::expect_identical(test_df %>%
                               duplicate_rows(x2, .both_ways = TRUE) %>%
                               nrow(), 6L)

  testthat::expect_identical(test_df %>%
                               duplicate_rows(x1, x2, .both_ways = TRUE,
                                           .keep_all = TRUE,
                                           .add_count = TRUE),
                               test_df %>%
                               dplyr::add_count(x1, x2) %>%
                               dplyr::filter(n > 1))
  testthat::expect_identical(test_df %>%
                               duplicate_rows(x1, x2, .both_ways = TRUE,
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
                                              .drop_empty = TRUE)), 0L)
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

  # Check that .drop_na works as intended
  testthat::expect_equal(
    duplicate_rows(
      dplyr::tibble(x = c(NA, NA, NA, NA), y = c(NA, 1, NA, 1)),
      .drop_empty = TRUE
    ),
    new_tbl(x = NA, y = 1)
  )
  testthat::expect_equal(
    fduplicates2(
      dplyr::tibble(x = c(NA, NA, NA, NA), y = c(NA, 1, NA, 1)),
      .drop_empty = TRUE
    ),
    new_tbl(x = NA, y = 1)
  )
})
