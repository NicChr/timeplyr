testthat::test_that("stat_summarise", {
  iris2 <- list_to_tibble(fslice_sample(iris, n = 10^3, replace = TRUE,
                                        seed = 918291))

  stat_summarise(fslice(iris2, 0))
  stat_summarise(fslice(iris2, 0), stat = "mean")
  stat_summarise(fslice(data.table::as.data.table(iris2), 0), stat = "mean")
  stat_summarise(fslice(data.table::as.data.table(iris2), 0), stat = "n")
  stat_summarise(fslice(data.table::as.data.table(iris2), 0), stat = c("n", "min"))

  stat_summarise(fslice(iris2, 0), stat = .stat_fns)

  stat_summarise(iris2)
  stat_summarise(iris2, stat = "mean",
                 .cols = "Sepal.Width")
  stat_summarise(iris2, stat = "mean",
                 .cols = "Sepal.Width",
                 .by = dplyr::all_of("Species"), sort = T)
  stat_summarise(iris2, stat = "mean",
                 .cols = "Sepal.Width",
                 .by = dplyr::all_of("Species"), sort = F)
  stat_summarise(iris2, stat = c("min", "max"),
                 .cols = 1)
  stat_summarise(iris2, stat = c("min", "max"),
                 .cols = 1, .names = "{.fn}")
  stat_summarise(iris2, stat = c("min", "mean", "max"),
                 .cols = 1:2)
  testthat::expect_error(stat_summarise(iris2, stat = c("min", "mean", "max"),
                 .cols = 1:2, .names = "{.fn}"))
  testthat::expect_error(stat_summarise(iris2, stat = c("min", "mean", "max"),
                 .cols = 1:2, .names = "{.col}"))
  testthat::expect_error(stat_summarise(iris2, stat = c("min", "max"),
                 .cols = 1, .names = "{.col}"))
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summarise(mean, stat = "mean")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summarise(mean, stat = "mean", .names = "ok")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summarise(.cols = c("ok" = "mean"), stat = "mean")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summarise(.cols = c("ok" = "mean"), stat = "mean", .names = "{.col}")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summarise(mean, stat = "max")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summarise(mean, stat = "mean", .by = mean)
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summarise(mean, stat = "max", .by = mean)
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summarise(mean, stat = "max", .by = mean, .names = "{.fn}")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summarise(.cols = c("ok" = "mean"), stat = "max", .by = mean, .names = "ok")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summarise(.cols = c("ok" = "mean"), stat = "max", .by = mean, .names = "ok")
})
