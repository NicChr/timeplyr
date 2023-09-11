testthat::test_that("Test cross-join", {
  iris2 <- dplyr::select(iris, Sepal.Length,
                         Species,
                         Petal.Width)
  CJ <- data.table::CJ
  testthat::expect_equal(crossed_join(iris2, unique = TRUE,
                                      sort = FALSE, as_dt = TRUE),
                         do.call(CJ, args = c(iris2, list(unique = TRUE,
                                                          sorted = FALSE))))
  test <- crossed_join(iris2, unique = TRUE,
                       sort = TRUE, as_dt = TRUE)
  setkeyv2(test, names(test))
  target <- do.call(CJ, args = c(iris2, list(unique = TRUE, sorted = TRUE)))
  testthat::expect_equal(test, target)

  test <- crossed_join(list(iris$Sepal.Width, iris$Petal.Length),
                       unique = FALSE,
                       sort = TRUE,
                       as_dt = TRUE)
  target <- CJ(iris$Sepal.Width, iris$Petal.Length,
               unique = FALSE, sorted = TRUE)
  setkeyv2(target, NULL)
  testthat::expect_equal(test, target)

  test <- crossed_join(list(iris$Sepal.Width, iris$Petal.Length),
                       unique = FALSE,
                       sort = TRUE,
                       as_dt = TRUE)
  target <- CJ(iris$Sepal.Width, iris$Petal.Length,
               unique = FALSE, sorted = TRUE)
  data.table::setkeyv(target, NULL)
  testthat::expect_equal(test, target)

  test <- crossed_join(list(iris$Sepal.Width, iris$Petal.Length),
                       unique = FALSE,
                       sort = TRUE,
                       as_dt = FALSE)
  data.table::setDT(test)
  testthat::expect_equal(test, target)

  test <- crossed_join(list(iris$Species, letters),
               strings_as_factors = TRUE,
               sort = TRUE)
  setkeyv2(test, names(test))
  target <- CJ(V1 = iris$Species, V2 = as.factor(letters),
               unique = TRUE, sorted = TRUE)
  testthat::expect_equal(test, target)
})
