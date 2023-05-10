testthat::test_that("farrange", {
  testthat::expect_equal(iris, farrange(iris))
  flights2 <- add_row_id(fslice_sample(nycflights13::flights, seed = 9192919))
  iris2 <- add_row_id(fslice_sample(iris, seed = 098124))
  base1 <- iris2 %>%
    dplyr::arrange(dplyr::desc(Species), Sepal.Length)
  base2 <- flights2 %>%
    dplyr::group_by(origin, dest) %>%
    dplyr::arrange(dest, dplyr::desc(tailnum), origin)
  base3 <- flights2 %>%
    dplyr::group_by(origin, dest) %>%
    dplyr::arrange(dest, dplyr::desc(tailnum), origin,
                   .by_group = TRUE)
  res1 <- iris2 %>%
    farrange(desc(Species), Sepal.Length)
  res2 <- flights2 %>%
    fgroup_by(origin, dest) %>%
    farrange(dest, desc(tailnum), origin)
  res3 <- flights2 %>%
    fgroup_by(origin, dest) %>%
    farrange(dest, desc(tailnum), origin,
             .by_group = TRUE)

  testthat::expect_equal(base1$row_id, res1$row_id)
  testthat::expect_equal(base2$row_id, res2$row_id)
  testthat::expect_equal(base3$row_id, res3$row_id)
})
