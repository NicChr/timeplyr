testthat::test_that("fselect", {
  testthat::expect_equal(fselect(iris),
                         dplyr::select(iris))
  testthat::expect_equal(fselect(iris, NULL),
                         dplyr::select(iris, NULL))
  testthat::expect_equal(fselect(iris, .cols = character(0)),
                         dplyr::select(iris, all_of(character(0))))
  testthat::expect_equal(fselect(iris, .cols = integer(0)),
                         dplyr::select(iris, all_of(character(0))))
  testthat::expect_equal(fselect(iris, dplyr::any_of(c("ok", names(iris)))),
                         dplyr::select(iris, dplyr::any_of(c("ok", names(iris)))))
  testthat::expect_error(fselect(iris, dplyr::all_of(c("ok", names(iris)))))
  testthat::expect_equal(fselect(iris, dplyr::any_of(c("okay", "Species"))),
                         dplyr::select(iris, dplyr::any_of(c("okay", "Species"))))
  testthat::expect_error(fselect(iris, dplyr::all_of(c("okay", "Species"))))
  testthat::expect_equal(fselect(iris, dplyr::all_of(rev(names(iris)))),
                         dplyr::select(iris, dplyr::all_of(rev(names(iris)))))

  testthat::expect_equal(fselect(iris, c("okay" = 3, 3, 4)),
                         dplyr::select(iris, c("okay" = 3, 3, 4)))
  testthat::expect_equal(fselect(iris, c(3, "okay" = 3, 4)),
                         dplyr::select(iris, c(3, "okay" = 3, 4)))

  testthat::expect_equal(fselect(iris, c(1, NULL, "okay" = "Species", "Sepal.Width")),
                         dplyr::select(iris, c(1, NULL, "okay" = "Species", "Sepal.Width")))

  iris2 <- dplyr::group_by(iris, Sepal.Length, Species)

  testthat::expect_equal(fselect(iris2),
                         dplyr::select(iris2))
  testthat::expect_equal(fselect(iris2, NULL),
                         dplyr::select(iris2, NULL))
  testthat::expect_equal(fselect(iris2, dplyr::any_of(c("okay", "Species"))),
                         dplyr::select(iris2, dplyr::any_of(c("okay", "Species"))))
  testthat::expect_error(fselect(iris2, dplyr::all_of(c("okay", "Species"))))
  testthat::expect_equal(fselect(iris2, dplyr::all_of(rev(names(iris2)))),
                         dplyr::select(iris2, dplyr::all_of(rev(names(iris2)))))

  testthat::expect_equal(fselect(iris2, c("okay" = 3, 3, 4)),
                         dplyr::select(iris2, c("okay" = 3, 3, 4)))
  testthat::expect_equal(fselect(iris2, c(3, "okay" = 3, 4)),
                         dplyr::select(iris2, c(3, "okay" = 3, 4)))

  testthat::expect_equal(fselect(iris2, c(1, NULL, "okay" = "Species", "Sepal.Width")),
                         dplyr::select(iris2, c(1, NULL, "okay" = "Species", "Sepal.Width")))

  testthat::expect_equal(fselect(iris2, okay = Species, ok2 = Sepal.Length),
                         dplyr::select(iris2, okay = Species, ok2 = Sepal.Length))
})
