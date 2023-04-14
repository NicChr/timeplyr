testthat::test_that("Testing growid", {
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
                           dplyr::mutate(id = growid(., g = NULL),
                                         .keep = "none"))
  testthat::expect_identical(gseq_len(0, g = 1),
                             gseq_len(0, g = NULL))
  testthat::expect_identical(growid(as.list(iris), g = NULL),
                             seq_len(150))
  testthat::expect_identical(growid(iris$Sepal.Length, g = NULL),
                             seq_len(150))
  # Grouped
  # testthat::expect_identical(growid(flights), growid2(flights, flights))
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g1),
                  id4 = gseq_len(nrow2(flights), g = g1)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g2),
                  id4 = gseq_len(nrow2(flights), g = g2)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g3),
                  id4 = gseq_len(nrow2(flights), g = g3)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g4),
                  id4 = gseq_len(nrow2(flights), g = g4)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g5),
                  id4 = gseq_len(nrow2(flights), g = g5)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g6),
                  id4 = gseq_len(nrow2(flights), g = g6)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g7),
                  id4 = gseq_len(nrow2(flights), g = g7)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g8),
                  id4 = gseq_len(nrow2(flights), g = g8)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g9),
                  id4 = gseq_len(nrow2(flights), g = g9)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g10),
                  id4 = gseq_len(nrow2(flights), g = g10)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g11),
                  id4 = gseq_len(nrow2(flights), g = g11)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g12),
                  id4 = gseq_len(nrow2(flights), g = g12)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g13),
                  id4 = gseq_len(nrow2(flights), g = g13)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g14),
                  id4 = gseq_len(nrow2(flights), g = g14)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g15),
                  id4 = gseq_len(nrow2(flights), g = g15)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
  testthat::expect_equal(
    dplyr::tibble(id1 = gres1$id,
                  # id2 = growid(flights),
                  id3 = growid(flights, g = g16),
                  id4 = gseq_len(nrow2(flights), g = g16)) %>%
      dplyr::mutate(min = do.call(pmin, dplyr::pick(id1, id3, id4)),
                    max = do.call(pmax, dplyr::pick(id1, id3, id4))) %>%
      dplyr::filter(min != max) %>%
      nrow(),
    0)
})
