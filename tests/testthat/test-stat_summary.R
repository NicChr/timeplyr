testthat::test_that("stat_summary", {
  iris2 <- list_to_tibble(fslice_sample(iris, n = 10^3, replace = TRUE,
                                        seed = 918291))

  stat_summary(fslice(iris2, 0))
  stat_summary(fslice(iris2, 0), stat = "mean")
  stat_summary(fslice(data.table::as.data.table(iris2), 0), stat = "mean")
  stat_summary(fslice(data.table::as.data.table(iris2), 0), stat = "n")
  stat_summary(fslice(data.table::as.data.table(iris2), 0), stat = c("n", "min"))

  stat_summary(fslice(iris2, 0), stat = .stat_fns)

  stat_summary(iris2)
  stat_summary(iris2, stat = "mean",
                 .cols = "Sepal.Width")
  stat_summary(iris2, stat = "mean",
                 .cols = "Sepal.Width",
                 .by = dplyr::all_of("Species"), sort = T)
  stat_summary(iris2, stat = "mean",
                 .cols = "Sepal.Width",
                 .by = dplyr::all_of("Species"), sort = F)
  stat_summary(iris2, stat = c("min", "max"),
                 .cols = 1)
  stat_summary(iris2, stat = c("min", "max"),
                 .cols = 1, .names = "{.fn}")
  stat_summary(iris2, stat = c("min", "mean", "max"),
                 .cols = 1:2)
  testthat::expect_error(stat_summary(iris2, stat = c("min", "mean", "max"),
                 .cols = 1:2, .names = "{.fn}"))
  testthat::expect_error(stat_summary(iris2, stat = c("min", "mean", "max"),
                 .cols = 1:2, .names = "{.col}"))
  testthat::expect_error(stat_summary(iris2, stat = c("min", "max"),
                 .cols = 1, .names = "{.col}"))
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summary(mean, stat = "mean")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summary(mean, stat = "mean", .names = "ok")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summary(.cols = c("ok" = "mean"), stat = "mean")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summary(.cols = c("ok" = "mean"), stat = "mean", .names = "{.col}")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summary(mean, stat = "max")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summary(mean, stat = "mean", .by = mean)
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summary(mean, stat = "max", .by = mean)
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summary(mean, stat = "max", .by = mean, .names = "{.fn}")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summary(.cols = c("ok" = "mean"), stat = "max", .by = mean, .names = "ok")
  iris2 %>%
    transmute2(mean = mean(Sepal.Length)) %>%
    stat_summary(.cols = c("ok" = "mean"), stat = "max", .by = mean, .names = "ok")
})
