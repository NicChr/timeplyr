# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("groups", {
  target <- iris %>%
    dplyr::group_by(Species) %>%
    dplyr::group_by(max(Sepal.Length), .add = TRUE)
  result <- iris %>%
    fgroup_by(Species) %>%
    fgroup_by(max(Sepal.Length), .add = TRUE)
  attr(attr(result, "groups"), "sorted") <- NULL
  testthat::expect_equal(result, target)
})
