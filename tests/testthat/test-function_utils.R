testthat::test_that("utils select", {
  testthat::expect_equal(select2(iris),
                         dplyr::select(iris))
  testthat::expect_equal(select2(iris, NULL),
                         dplyr::select(iris, NULL))
  testthat::expect_equal(select2(iris, dplyr::any_of(c("okay", "Species"))),
                         dplyr::select(iris, dplyr::any_of(c("okay", "Species"))))
  testthat::expect_error(select2(iris, dplyr::all_of(c("okay", "Species"))))
  testthat::expect_equal(select2(iris, dplyr::all_of(rev(names(iris)))),
                         dplyr::select(iris, dplyr::all_of(rev(names(iris)))))

  testthat::expect_equal(select2(iris, c("okay" = 3, 3, 4)),
                         dplyr::select(iris, c("okay" = 3, 3, 4)))
  testthat::expect_equal(select2(iris, c(3, "okay" = 3, 4)),
                         dplyr::select(iris, c(3, "okay" = 3, 4)))

  testthat::expect_equal(select2(iris, c(1, NULL, "okay" = "Species", "Sepal.Width")),
                         dplyr::select(iris, c(1, NULL, "okay" = "Species", "Sepal.Width")))

  iris2 <- dplyr::group_by(iris, Sepal.Length, Species)

  testthat::expect_equal(select2(iris2),
                         dplyr::select(iris2))
  testthat::expect_equal(select2(iris2, NULL),
                         dplyr::select(iris2, NULL))
  testthat::expect_equal(select2(iris2, dplyr::any_of(c("okay", "Species"))),
                         dplyr::select(iris2, dplyr::any_of(c("okay", "Species"))))
  testthat::expect_error(select2(iris2, dplyr::all_of(c("okay", "Species"))))
  testthat::expect_equal(select2(iris2, dplyr::all_of(rev(names(iris2)))),
                         dplyr::select(iris2, dplyr::all_of(rev(names(iris2)))))

  testthat::expect_equal(select2(iris2, c("okay" = 3, 3, 4)),
                         dplyr::select(iris2, c("okay" = 3, 3, 4)))
  testthat::expect_equal(select2(iris2, c(3, "okay" = 3, 4)),
                         dplyr::select(iris2, c(3, "okay" = 3, 4)))

  testthat::expect_equal(select2(iris2, c(1, NULL, "okay" = "Species", "Sepal.Width")),
                         dplyr::select(iris2, c(1, NULL, "okay" = "Species", "Sepal.Width")))

  testthat::expect_equal(select2(iris2, okay = Species, ok2 = Sepal.Length),
                         dplyr::select(iris2, okay = Species, ok2 = Sepal.Length))

})

testthat::test_that("utils mutate", {
  testthat::expect_equal(mutate2(iris),
                         dplyr::mutate(iris))
  testthat::expect_equal(mutate2(iris, NULL),
                         dplyr::mutate(iris, NULL))
  testthat::expect_equal(mutate2(iris, across(dplyr::any_of(c("okay", "Species")))),
                         dplyr::mutate(iris, across(dplyr::any_of(c("okay", "Species")))))
  testthat::expect_error(mutate2(iris, across(dplyr::all_of(c("okay", "Species")))))
  testthat::expect_equal(mutate2(iris, across(dplyr::all_of(rev(names(iris))))),
                         dplyr::mutate(iris, across(dplyr::all_of(rev(names(iris))))))

  testthat::expect_equal(mutate2(iris, okay = 3),
                         dplyr::mutate(iris, "okay" = 3))

  testthat::expect_equal(mutate2(iris, NULL, Petal.Width = NULL, "okay" = Species, Sepal.Width),
                         dplyr::mutate(iris, NULL, Petal.Width = NULL, "okay" = Species, Sepal.Width))

  iris2 <- dplyr::group_by(iris, Sepal.Length, Species)

  testthat::expect_equal(mutate2(iris2),
                         dplyr::mutate(iris2))
  testthat::expect_equal(mutate2(iris2, NULL),
                         dplyr::mutate(iris2, NULL))

  testthat::expect_equal(mutate2(iris2, Sepal.Length, Petal.Length),
                         dplyr::mutate(iris2, Sepal.Length, Petal.Length))

  testthat::expect_equal(mutate2(iris2, ok = Species),
                         dplyr::mutate(iris2, ok = Species))

})
