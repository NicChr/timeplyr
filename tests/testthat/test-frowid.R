testthat::test_that("Testing frowid", {
  flights <- dplyr::select(nycflights13::flights, origin, dest)
  g1 <- group_id(flights, origin, dest)
  g2 <- group_id(flights, origin, dest, order = FALSE)
  g3 <- group_id(flights, origin, dest, order = FALSE, as_qg = TRUE)
  g4 <- group_id(flights, origin, dest, order = TRUE, as_qg = TRUE)
  g5 <- collapse::GRP(g1, sort = TRUE)
  g6 <- collapse::GRP(g1, sort = FALSE)
  g7 <- collapse::GRP(g2, sort = TRUE)
  g8 <- collapse::GRP(g2, sort = FALSE)
  g9 <- collapse::GRP(g3, sort = TRUE)
  g10 <- collapse::GRP(g3, sort = FALSE)
  g11 <- collapse::GRP(g4, sort = TRUE)
  g12 <- collapse::GRP(g4, sort = FALSE)
  g13 <- collapse::GRP(g5, sort = TRUE)
  g14 <- collapse::GRP(g5, sort = FALSE)
  g15 <- collapse::GRP(g6, sort = TRUE)
  g16 <- collapse::GRP(g6, sort = FALSE)

  res <- flights %>%
    dplyr::mutate(id = dplyr::row_number(),
                  .keep = "none")
  gres1 <- flights %>%
    dplyr::group_by(origin, dest) %>%
    dplyr::mutate(id = dplyr::row_number(),
                  .keep = "none") %>%
    dplyr::ungroup()
  testthat::expect_equal(res,
                         flights %>%
                           dplyr::mutate(id = frowid(., g = NULL),
                                         .keep = "none"))
  testthat::expect_identical(frowid(as.list(iris), g = NULL),
                             seq_len(150))
  testthat::expect_identical(frowid(iris$Sepal.Length, g = NULL),
                             seq_len(150))
  # Grouped
  # testthat::expect_identical(frowid(flights), frowid2(flights, flights))
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = frowid(flights),
                  id3 = frowid(flights, g = g1)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  id3 = frowid(flights, g = g2)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = frowid(flights),
                  id3 = frowid(flights, g = g3)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  id3 = frowid(flights, g = g4)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  id3 = frowid(flights, g = g5)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  id3 = frowid(flights, g = g6)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = frowid(flights),
                  id3 = frowid(flights, g = g7)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = frowid(flights),
                  id3 = frowid(flights, g = g8)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = frowid(flights),
                  id3 = frowid(flights, g = g9)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = frowid(flights),
                  id3 = frowid(flights, g = g10)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = frowid(flights),
                  id3 = frowid(flights, g = g11)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = frowid(flights),
                  id3 = frowid(flights, g = g12)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = frowid(flights),
                  id3 = frowid(flights, g = g13)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = frowid(flights),
                  id3 = frowid(flights, g = g14)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = frowid(flights),
                  id3 = frowid(flights, g = g15)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = frowid(flights),
                  id3 = frowid(flights, g = g16)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3)),
                    max = do.call(pmax, dplyr::pick(id1, id3))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
})
