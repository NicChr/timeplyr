test_that("selecting", {
  expect_equal(fselect(iris),
                         dplyr::select(iris))
  expect_equal(fselect(iris, NULL),
                         dplyr::select(iris, NULL))
  expect_equal(fselect(iris, .cols = character(0)),
                         dplyr::select(iris, all_of(character(0))))
  expect_equal(fselect(iris, .cols = integer(0)),
                         dplyr::select(iris, all_of(character(0))))
  expect_equal(fselect(iris, dplyr::any_of(c("ok", names(iris)))),
                         dplyr::select(iris, dplyr::any_of(c("ok", names(iris)))))
  expect_error(fselect(iris, dplyr::all_of(c("ok", names(iris)))))
  expect_equal(fselect(iris, dplyr::any_of(c("okay", "Species"))),
                         dplyr::select(iris, dplyr::any_of(c("okay", "Species"))))
  expect_error(fselect(iris, dplyr::all_of(c("okay", "Species"))))
  expect_equal(fselect(iris, dplyr::all_of(rev(names(iris)))),
                         dplyr::select(iris, dplyr::all_of(rev(names(iris)))))

  expect_equal(fselect(iris, c("okay" = 3, 3, 4)),
                         dplyr::select(iris, c("okay" = 3, 3, 4)))
  expect_equal(fselect(iris, c(3, "okay" = 3, 4)),
                         dplyr::select(iris, c(3, "okay" = 3, 4)))

  expect_equal(fselect(iris, c(1, NULL, "okay" = "Species", "Sepal.Width")),
                         dplyr::select(iris, c(1, NULL, "okay" = "Species", "Sepal.Width")))

  iris2 <- dplyr::group_by(iris, Sepal.Length, Species)

  expect_equal(fselect(iris2),
                         dplyr::select(iris2))
  expect_equal(fselect(iris2, NULL),
                         dplyr::select(iris2, NULL))
  expect_equal(fselect(iris2, dplyr::any_of(c("okay", "Species"))),
                         dplyr::select(iris2, dplyr::any_of(c("okay", "Species"))))
  expect_error(fselect(iris2, dplyr::all_of(c("okay", "Species"))))
  expect_equal(fselect(iris2, dplyr::all_of(rev(names(iris2)))),
                         dplyr::select(iris2, dplyr::all_of(rev(names(iris2)))))

  expect_equal(fselect(iris2, c("okay" = 3, 3, 4)),
                         dplyr::select(iris2, c("okay" = 3, 3, 4)))
  expect_equal(fselect(iris2, c(3, "okay" = 3, 4)),
                         dplyr::select(iris2, c(3, "okay" = 3, 4)))

  expect_equal(fselect(iris2, c(1, NULL, "okay" = "Species", "Sepal.Width")),
                         dplyr::select(iris2, c(1, NULL, "okay" = "Species", "Sepal.Width")))

  expect_equal(fselect(iris2, okay = Species, ok2 = Sepal.Length),
                         dplyr::select(iris2, okay = Species, ok2 = Sepal.Length))
})


test_that("renaming", {
  expect_equal(frename(iris), iris)
  expect_equal(frename(iris, .cols = 0), iris)

  expect_equal(frename(iris, .cols = c("okay" = "Species")),
               dplyr::rename(iris, okay = Species))

  expect_equal(frename(dplyr::group_by(iris, Species), .cols = c("okay" = "Species")),
               dplyr::rename(dplyr::group_by(iris, Species), okay = Species))
})
