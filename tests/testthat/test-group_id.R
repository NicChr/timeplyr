testthat::test_that("Group IDs", {
  base1 <- iris %>%
    dplyr::group_by(Species) %>%
    dplyr::group_indices()
  base2 <- iris %>%
    dplyr::group_by(Species, Sepal.Length) %>%
    dplyr::group_indices()

  iris2 <- data.table::as.data.table(iris)
  base3 <- iris2[, ("id") := .GRP, by = c("Species", "Sepal.Length")][["id"]]

  testthat::expect_identical(rep_len(1L, nrow(iris)),
                             group_id(iris))
  testthat::expect_error(iris %>%
                           dplyr::group_by(Species) %>%
                               group_id(.by = all_of("Sepal.Length"),
                                        sort = TRUE))
  testthat::expect_identical(base1,
                             iris %>%
                               group_id(Species, sort = TRUE))
  testthat::expect_identical(base2,
                             iris %>%
                               dplyr::group_by(Species) %>%
                               group_id(Sepal.Length, sort = TRUE))
  testthat::expect_identical(base2,
                             iris %>%
                               dplyr::group_by(Species, Sepal.Length) %>%
                               group_id())
  testthat::expect_identical(base2,
                             iris %>%
                               group_id(Species, Sepal.Length, sort = TRUE))
  testthat::expect_identical(base2,
                             iris %>%
                               group_id(.by = all_of(c("Species", "Sepal.Length")),
                                        sort = TRUE))
  testthat::expect_identical(base2,
                             iris %>%
                               dplyr::group_by(Sepal.Width) %>%
                               group_id(.by = all_of(c("Species", "Sepal.Length")),
                                        sort = TRUE,
                                        .overwrite = TRUE))
  testthat::expect_identical(base2,
                             iris %>%
                               dplyr::group_by(Species) %>%
                               group_id(Sepal.Length, sort = TRUE,
                                        as_qg = TRUE) %>%
                               as.integer())
  # Unsorted
  testthat::expect_identical(base3,
                             iris %>%
                               dplyr::group_by(Species) %>%
                               group_id(Sepal.Length, sort = FALSE))
  testthat::expect_identical(base3,
                             iris %>%
                               group_id(Species, Sepal.Length, sort = FALSE))
  testthat::expect_identical(base3,
                             iris %>%
                               group_id(.by = all_of(c("Species", "Sepal.Length")),
                                        sort = FALSE))
  testthat::expect_identical(base3,
                             iris %>%
                               dplyr::group_by(Sepal.Width) %>%
                               group_id(.by = all_of(c("Species", "Sepal.Length")),
                                        sort = FALSE,
                                        .overwrite = TRUE))
  testthat::expect_identical(base3,
                             iris %>%
                               dplyr::group_by(Species) %>%
                               group_id(Sepal.Length, sort = FALSE,
                                        as_qg = TRUE) %>%
                               as.integer())
})
testthat::test_that("Adding group IDs", {
  base1 <- iris %>%
    dplyr::group_by(Species, Sepal.Length) %>%
    dplyr::mutate(id = dplyr::cur_group_id()) %>%
    df_reconstruct(iris)

  iris2 <- data.table::as.data.table(iris)
  base2 <- iris2[, ("id") := .GRP, by = c("Species", "Sepal.Length")]
  base2 <- df_reconstruct(base2, iris)

  testthat::expect_identical(base1,
                             iris %>%
                               add_group_id(Species, Sepal.Length, sort = TRUE,
                                            .name = "id"))
  testthat::expect_identical(base2,
                             iris %>%
                               add_group_id(Species, Sepal.Length, sort = FALSE,
                                            .name = "id"))

  testthat::expect_identical(base1,
                             iris %>%
                               add_group_id(Species, Sepal.Length, sort = TRUE) %>%
                               dplyr::rename(id = group_id))
})
