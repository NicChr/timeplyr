# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Group IDs", {
  base1 <- iris %>%
    dplyr::group_by(Petal.Width) %>%
    dplyr::group_indices()
  base2 <- iris %>%
    dplyr::group_by(Species, Sepal.Length) %>%
    dplyr::group_indices()

  iris2 <- data.table::as.data.table(iris)
  base3 <- iris2[, ("id") := .GRP, by = c("Species", "Sepal.Length")][][["id"]]

  testthat::expect_equal(rep_len(1L, nrow(iris)),
                             group_id(iris))
  testthat::expect_error(iris %>%
                           dplyr::group_by(Species) %>%
                               group_id(.by = all_of("Sepal.Length"),
                                        order = TRUE))

  # testthat::expect_equal(iris %>%
  #                          dplyr::group_by(Species, Sepal.Length) %>%
  #                          dplyr::group_indices() %>%
  #                          collapse::qG(ordered = TRUE, na.exclude = FALSE),
  #                        iris %>%
  #                         fgroup_by(Species, Sepal.Length) %>%
  #                          group_id(as_qg = TRUE))
  testthat::expect_equal(base1,
                             iris %>%
                               group_id(Petal.Width, order = TRUE))

  testthat::expect_equal(iris %>%
                               dplyr::select(Petal.Width) %>%
                               group_id.default(order = TRUE),
                             iris %>%
                               group_id(Petal.Width, order = TRUE))
  testthat::expect_equal(iris %>%
                               dplyr::select(Petal.Width) %>%
                               group_id.default(order = FALSE),
                             iris %>%
                               group_id(Petal.Width, order = FALSE))
  testthat::expect_equal(iris %>%
                               dplyr::pull(Petal.Width) %>%
                               group_id.default(order = TRUE),
                             iris %>%
                               group_id(Petal.Width, order = TRUE))
  testthat::expect_equal(iris %>%
                               dplyr::pull(Petal.Width) %>%
                               group_id.default(order = FALSE),
                             iris %>%
                               group_id(Petal.Width, order = FALSE))
  testthat::expect_equal(base2,
                             iris %>%
                               dplyr::group_by(Species) %>%
                               group_id(Sepal.Length, order = TRUE))
  testthat::expect_equal(base2,
                             iris %>%
                               dplyr::group_by(Species, Sepal.Length) %>%
                               group_id())
  testthat::expect_equal(base2,
                             iris %>%
                               group_id(Species, Sepal.Length, order = TRUE))
  testthat::expect_equal(base2,
                             iris %>%
                               group_id(.by = all_of(c("Species", "Sepal.Length")),
                                        order = TRUE))
  testthat::expect_equal(base2,
                             iris %>%
                               dplyr::group_by(Species) %>%
                               group_id(Sepal.Length, order = TRUE,
                                        as_qg = TRUE) %>%
                               as.integer())
  # Unsorted
  testthat::expect_equal(base3,
                             iris %>%
                               dplyr::group_by(Species) %>%
                               group_id(Sepal.Length, order = FALSE))
  testthat::expect_equal(base3,
                             iris %>%
                               group_id(Species, Sepal.Length, order = FALSE))
  testthat::expect_equal(base3,
                             iris %>%
                               group_id(.by = all_of(c("Species", "Sepal.Length")),
                                        order = FALSE))
  testthat::expect_equal(base3,
                             iris %>%
                               dplyr::group_by(Species) %>%
                               group_id(Sepal.Length, order = FALSE,
                                        as_qg = TRUE) %>%
                               as.integer())

  # Zero cols
  testthat::expect_equal(group_id(iris2 %>% dplyr::select()),
                             seq_ones(df_nrow(iris2)))
  testthat::expect_equal(group_id(iris %>% dplyr::select()),
                             seq_ones(df_nrow(iris2)))
  # With renaming

  testthat::expect_equal(iris %>% group_id(okay = Petal.Width),
                             base1)
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Petal.Width) %>%
                               group_id(okay = Petal.Width),
                             base1)

  # Intervals

  x <- lubridate::interval(time_seq(Sys.Date(), length.out = 10, time_by = "day"),
                           time_seq(Sys.Date() + lubridate::days(11),
                                    length.out = 10,
                                    time_by = "day"))
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
  testthat::expect_equal(as.integer(group_id(x, as_qg = TRUE)),
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

  testthat::expect_equal(nrow(group_collapse(x)),
                         61)
  testthat::expect_equal(nrow(group_collapse(df)),
                         61)
  testthat::expect_equal(nrow(group_collapse(safe_ungroup(df), x)),
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

  testthat::expect_equal(base1,
                             iris %>%
                               add_group_id(Species, Sepal.Length, order = TRUE,
                                            .name = "id"))
  testthat::expect_equal(base2,
                             iris %>%
                               add_group_id(Species, Sepal.Length, order = FALSE,
                                            .name = "id"))

  testthat::expect_equal(base1,
                             iris %>%
                               add_group_id(Species, Sepal.Length, order = TRUE) %>%
                               dplyr::rename(id = group_id))
})


testthat::test_that("Group locs", {
  flights <- nycflights13::flights
  base1 <- iris %>%
    dplyr::group_data() %>%
    dplyr::mutate(.group = 1L)
  base2 <- flights %>%
    add_group_id(origin, dest, order = TRUE, .name = ".group") %>%
    dplyr::pull(.group) %>%
    vctrs::vec_group_loc() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(loc = vctrs::as_list_of(loc, .ptype = integer(0))) %>%
    dplyr::rename(.group = key,
                  .rows = loc)
  base3 <- flights %>%
    dplyr::group_by(origin, dest) %>%
    dplyr::group_data() %>%
    dplyr::mutate(.group = frowid(., NULL))
  base4 <- base2 %>%
    dplyr::mutate(.group = seq_len(nrow(base2)))
  base5 <- base4
  res1 <- iris %>%
    group_collapse()
  res2 <- flights %>%
    group_collapse(origin, dest, sort = FALSE, order = TRUE)
  res3 <- flights %>%
    group_collapse(origin, dest, sort = TRUE, order = TRUE)
  res4 <- flights %>%
    group_collapse(origin, dest, sort = TRUE, order = FALSE)
  res5 <- flights %>%
    group_collapse(origin, dest, sort = FALSE, order = FALSE)

  testthat::expect_equal(base1$.rows, res1$.loc)
  testthat::expect_equal(base1$.group, res1$.group)
  testthat::expect_equal(base2$.rows, res2$.loc)
  testthat::expect_equal(base2$.group, res2$.group)
  testthat::expect_equal(base3$.rows, res3$.loc)
  testthat::expect_equal(base3$.group, res3$.group)
  testthat::expect_equal(base4$.rows, res4$.loc)
  testthat::expect_equal(base4$.group, res4$.group)
  testthat::expect_equal(base5$.rows, res5$.loc)
  testthat::expect_equal(base5$.group, res5$.group)

})


testthat::test_that("Row IDs", {
  flights2 <- fslice_sample(nycflights13::flights,
                           seed = 9192919)
  iris2 <- fslice_sample(iris, seed = 098124)
  base1 <- iris2 %>%
    dplyr::mutate(id = dplyr::row_number())
  base2 <- flights2 %>%
    dplyr::group_by(origin, dest) %>%
    dplyr::mutate(id = dplyr::row_number())
  res1 <- iris2 %>%
    add_row_id()
  res2 <- flights2 %>%
    add_row_id(.by = c(origin, dest))
  res3 <- flights2 %>%
    add_row_id(origin, dest)
  res4 <- flights2 %>%
    dplyr::mutate(row_id = row_id(dplyr::pick(origin, dest),
                                  .by = dplyr::everything()))

  testthat::expect_equal(row_id.default(flights2),
                         row_id(flights2, .by = dplyr::everything()))
  testthat::expect_equal(row_id.default(flights2, ascending = FALSE),
                         row_id(flights2, .by = dplyr::everything(), ascending = FALSE))

  testthat::expect_equal(base1$id, res1$row_id)
  testthat::expect_equal(base2$id, res2$row_id)
  testthat::expect_equal(res2$row_id, res3$row_id)
  testthat::expect_equal(res2$row_id, res4$row_id)

  testthat::expect_equal(
    iris2 %>%
      fslice(0) %>%
      add_row_id(),
    iris2 %>%
      fslice(0) %>%
      dplyr::mutate(row_id = dplyr::row_number()))
  testthat::expect_equal(
    iris2 %>%
      data.table::as.data.table() %>%
      fslice(0) %>%
      add_row_id(),
    iris2 %>%
      data.table::as.data.table() %>%
      dplyr::slice(0) %>%
      dplyr::mutate(row_id = dplyr::row_number()))
  testthat::expect_equal(
    iris2 %>%
      dplyr::select() %>%
      add_row_id() %>%
      data.table::as.data.table(),
    iris2 %>%
      dplyr::select() %>%
      dplyr::mutate(row_id = dplyr::row_number()) %>%
      data.table::as.data.table())

  testthat::expect_equal(row_id(base2),
                         base2$id)
})

testthat::test_that("group order", {
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
    dplyr::mutate(Species = dplyr::desc(Species), Sepal.Length) %>%
    dplyr::slice(group_order(dplyr::pick(Species, Sepal.Length),
                             .by = dplyr::everything()))
  res2 <- flights2 %>%
    dplyr::mutate(tailnum2 = group_id(tailnum),
                  tailnum = dplyr::desc(dplyr::if_else(is.na(tailnum), NA_integer_, tailnum2))) %>%
    dplyr::select(-tailnum2) %>%
    dplyr::mutate(order = group_order(dplyr::pick(dest, tailnum, origin),
                                      .by = dplyr::everything())) %>%
    dplyr::slice(.data[["order"]])
  i <- flights2 %>%
    dplyr::group_by(origin, dest) %>%
    dplyr::mutate(tailnum2 = group_id(tailnum),
                  tailnum = dplyr::desc(dplyr::if_else(is.na(tailnum), NA_integer_, tailnum2))) %>%
    dplyr::select(-tailnum2) %>%
    group_order(tailnum)
  res3 <- flights2 %>%
    fslice(i)

  testthat::expect_equal(base1$row_id, res1$row_id)
  testthat::expect_equal(base2$row_id, res2$row_id)
  testthat::expect_equal(base3$row_id, res3$row_id)
})
