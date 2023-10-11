# Set number of data.table threads to 1
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

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
