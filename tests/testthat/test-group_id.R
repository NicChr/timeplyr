testthat::test_that("Group IDs", {
  base1 <- iris %>%
    dplyr::group_by(Petal.Width) %>%
    dplyr::group_indices()
  base2 <- iris %>%
    dplyr::group_by(Species, Sepal.Length) %>%
    dplyr::group_indices()

  iris2 <- data.table::as.data.table(iris)
  base3 <- iris2[, ("id") := .GRP, by = c("Species", "Sepal.Length")][][["id"]]

  testthat::expect_identical(rep_len(1L, nrow(iris)),
                             group_id(iris))
  testthat::expect_error(iris %>%
                           dplyr::group_by(Species) %>%
                               group_id(.by = all_of("Sepal.Length"),
                                        order = TRUE))
  testthat::expect_identical(base1,
                             iris %>%
                               group_id(Petal.Width, order = TRUE))

  testthat::expect_identical(iris %>%
                               dplyr::select(Petal.Width) %>%
                               group_id.default(order = TRUE),
                             iris %>%
                               group_id(Petal.Width, order = TRUE))
  testthat::expect_identical(iris %>%
                               dplyr::select(Petal.Width) %>%
                               group_id.default(order = FALSE),
                             iris %>%
                               group_id(Petal.Width, order = FALSE))
  testthat::expect_identical(iris %>%
                               dplyr::pull(Petal.Width) %>%
                               group_id.default(order = TRUE),
                             iris %>%
                               group_id(Petal.Width, order = TRUE))
  testthat::expect_identical(iris %>%
                               dplyr::pull(Petal.Width) %>%
                               group_id.default(order = FALSE),
                             iris %>%
                               group_id(Petal.Width, order = FALSE))
  testthat::expect_identical(base2,
                             iris %>%
                               dplyr::group_by(Species) %>%
                               group_id(Sepal.Length, order = TRUE))
  testthat::expect_identical(base2,
                             iris %>%
                               dplyr::group_by(Species, Sepal.Length) %>%
                               group_id())
  testthat::expect_identical(base2,
                             iris %>%
                               group_id(Species, Sepal.Length, order = TRUE))
  testthat::expect_identical(base2,
                             iris %>%
                               group_id(.by = all_of(c("Species", "Sepal.Length")),
                                        order = TRUE))
  testthat::expect_identical(base2,
                             iris %>%
                               dplyr::group_by(Species) %>%
                               group_id(Sepal.Length, order = TRUE,
                                        as_qg = TRUE) %>%
                               as.integer())
  # Unsorted
  testthat::expect_identical(base3,
                             iris %>%
                               dplyr::group_by(Species) %>%
                               group_id(Sepal.Length, order = FALSE))
  testthat::expect_identical(base3,
                             iris %>%
                               group_id(Species, Sepal.Length, order = FALSE))
  testthat::expect_identical(base3,
                             iris %>%
                               group_id(.by = all_of(c("Species", "Sepal.Length")),
                                        order = FALSE))
  testthat::expect_identical(base3,
                             iris %>%
                               dplyr::group_by(Species) %>%
                               group_id(Sepal.Length, order = FALSE,
                                        as_qg = TRUE) %>%
                               as.integer())

  # Zero cols
  testthat::expect_identical(group_id(iris2 %>% dplyr::select()),
                             seq_ones(nrow2(iris2)))
  testthat::expect_identical(group_id(iris %>% dplyr::select()),
                             seq_ones(nrow2(iris2)))
  # With renaming

  testthat::expect_identical(iris %>% group_id(okay = Petal.Width),
                             base1)
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Petal.Width) %>%
                               group_id(okay = Petal.Width),
                             base1)

  # Intervals

  x <- lubridate::interval(time_seq(Sys.Date(), length.out = 10, by = "day"),
                           time_seq(Sys.Date() + lubridate::days(11), length.out = 10, by = "day"))
  testthat::expect_equal(group_id(x),
                         seq_len(length(x)))
  testthat::expect_equal(group_id(dplyr::tibble(x), x),
                         seq_len(length(x)))

  set.seed(98123355)
  x <- c(rep(x, 20), lubridate::interval(Sys.Date(),
                                         Sys.Date() + lubridate::hours(sample2(0:50))))
  x <- sample2(x)

  df <- dplyr::tibble(x) %>%
    dplyr::group_by(x)
  res <- dplyr::group_indices(df)
  testthat::expect_equal(group_id.Interval(x, order = TRUE),
                         res)
  testthat::expect_equal(group_id.default(x, order = TRUE),
                         res)
  testthat::expect_equal(group_id.data.frame(df, order = TRUE),
                         res)
  testthat::expect_equal(group_id.data.frame(safe_ungroup(df), x, order = TRUE),
                         res)
  testthat::expect_equal(GRP2(x, sort = TRUE)$group.id,
                         res)
  testthat::expect_equal(collapse::GRP(x, sort = TRUE)$group.id,
                         res)
  testthat::expect_equal(GRP2(df, sort = TRUE)$group.id,
                         res)
  testthat::expect_equal(GRP2(safe_ungroup(df), by = "x", sort = TRUE)$group.id,
                         res)

  testthat::expect_equal(nrow(group_loc(x)),
                         61)
  testthat::expect_equal(nrow(group_loc(df)),
                         61)
  testthat::expect_equal(nrow(group_loc(safe_ungroup(df), x)),
                         61)
})
testthat::test_that("Adding group IDs", {
  base1 <- iris %>%
    dplyr::group_by(Species, Sepal.Length) %>%
    dplyr::mutate(id = dplyr::cur_group_id()) %>%
    df_reconstruct(iris)

  iris2 <- data.table::as.data.table(iris)
  base2 <- iris2[, ("id") := .GRP, by = c("Species", "Sepal.Length")]
  base2 <- df_reconstruct(base2, iris)

  testthat::expect_identical(base1,
                             iris %>%
                               add_group_id(Species, Sepal.Length, order = TRUE,
                                            .name = "id"))
  testthat::expect_identical(base2,
                             iris %>%
                               add_group_id(Species, Sepal.Length, order = FALSE,
                                            .name = "id"))

  testthat::expect_identical(base1,
                             iris %>%
                               add_group_id(Species, Sepal.Length, order = TRUE) %>%
                               dplyr::rename(id = group_id))
})


testthat::test_that("Group locs", {
  flights <- nycflights13::flights
  base1 <- iris %>%
    dplyr::group_data() %>%
    dplyr::mutate(group_id = 1L)
  base2 <- flights %>%
    add_group_id(origin, dest, order = TRUE) %>%
    dplyr::pull(group_id) %>%
    vctrs::vec_group_loc() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(loc = vctrs::as_list_of(loc, .ptype = integer(0))) %>%
    dplyr::rename(group_id = key,
                  .rows = loc)
  base3 <- flights %>%
    dplyr::group_by(origin, dest) %>%
    dplyr::group_data() %>%
    dplyr::mutate(group_id = growid(., NULL))
  base4 <- base2 %>%
    dplyr::mutate(group_id = seq_len(nrow(base2)))
  base5 <- base4
  res1 <- iris %>%
    group_loc()
  res2 <- flights %>%
    group_loc(origin, dest, sort = FALSE, order = TRUE)
  res3 <- flights %>%
    group_loc(origin, dest, sort = TRUE, order = TRUE)
  res4 <- flights %>%
    group_loc(origin, dest, sort = TRUE, order = FALSE)
  res5 <- flights %>%
    group_loc(origin, dest, sort = FALSE, order = FALSE)

  testthat::expect_equal(base1$.rows, res1$.rows)
  testthat::expect_equal(base1$group_id, res1$group_id)
  testthat::expect_equal(base2$.rows, res2$.rows)
  testthat::expect_equal(base2$group_id, res2$group_id)
  testthat::expect_equal(base3$.rows, res3$.rows)
  testthat::expect_equal(base3$group_id, res3$group_id)
  testthat::expect_equal(base4$.rows, res4$.rows)
  testthat::expect_equal(base4$group_id, res4$group_id)
  testthat::expect_equal(base5$.rows, res5$.rows)
  testthat::expect_equal(base5$group_id, res5$group_id)

})
