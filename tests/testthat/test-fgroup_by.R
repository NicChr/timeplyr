testthat::test_that("groups", {
  testthat::expect_equal(iris %>%
                            dplyr::group_by(Species) %>%
                            dplyr::group_by(max(Sepal.Length), .add = TRUE),
                          iris %>%
                            fgroup_by(Species) %>%
                            fgroup_by(max(Sepal.Length), .add = TRUE))
})
