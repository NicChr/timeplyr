testthat::test_that("fslice", {
  flights <- nycflights13::flights
  flights[["id"]] <- seq_len(nrow(flights))
  set.seed(5199123)
  ids <- sample(1:10^3)
  testthat::expect_error(flights %>%
                           fslice(1, -1))
  testthat::expect_equal(flights %>%
                               fslice(),
                             flights %>%
                               dplyr::slice())
  testthat::expect_equal(flights %>%
                               dplyr::group_by(origin, dest) %>%
                               fslice(),
                             flights %>%
                               dplyr::group_by(origin, dest) %>%
                               dplyr::slice())
  testthat::expect_equal(flights %>%
                               fslice(ids, keep_order = TRUE),
                             flights %>%
                               dplyr::slice(ids) %>%
                               dplyr::arrange(id))
  testthat::expect_equal(flights %>%
                           fslice(ids, keep_order = FALSE),
                         flights %>%
                           dplyr::slice(ids))
  testthat::expect_equal(flights %>%
                               dplyr::group_by(origin, dest) %>%
                               fslice(ids),
                             flights %>%
                               dplyr::group_by(origin, dest) %>%
                               dplyr::slice(ids))
  testthat::expect_equal(flights %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice(ids, keep_order = TRUE),
                         flights %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice(ids) %>%
                           dplyr::arrange(id))
  testthat::expect_equal(flights %>%
                           fslice(ids, .by = c(origin, dest),
                                  sort_groups = FALSE),
                         flights %>%
                           dplyr::slice(ids, .by = c(origin, dest)))
  testthat::expect_equal(flights %>%
                           fslice(-ids, keep_order = FALSE),
                         flights %>%
                           dplyr::slice(-ids))
  testthat::expect_equal(flights %>%
                               fslice(-ids, keep_order = TRUE),
                             flights %>%
                               dplyr::slice(-ids) %>%
                               dplyr::arrange(id))
  testthat::expect_equal(flights %>%
                               dplyr::group_by(origin, dest) %>%
                               fslice(1:5, keep_order = TRUE),
                             flights %>%
                               dplyr::group_by(origin, dest) %>%
                               dplyr::slice(1:5) %>%
                               dplyr::arrange(id))
  testthat::expect_equal(flights %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice(1:5, keep_order = FALSE),
                         flights %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice(1:5))
  testthat::expect_equal(flights %>%
                               dplyr::group_by(origin, dest) %>%
                               fslice(-ids, keep_order = TRUE),
                             flights %>%
                               dplyr::group_by(origin, dest) %>%
                               dplyr::slice(-ids) %>%
                               dplyr::arrange(id))
  testthat::expect_equal(flights %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice(-ids, keep_order = FALSE),
                         flights %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice(-ids))
})
testthat::test_that("fslice_head", {
  flights2 <- nycflights13::flights
  set.seed(81243844)
  flights2 <- flights2[sample(seq_len(nrow(flights2)),
                              size = 10^4), ]
  flights2[["id"]] <- seq_len(nrow(flights2))

  testthat::expect_error(flights2 %>%
                           fslice_head(1, -1))
  testthat::expect_equal(flights2 %>%
                               fslice_head(),
                             flights2 %>%
                               dplyr::slice_head())
  testthat::expect_equal(flights2 %>%
                               dplyr::group_by(origin, dest) %>%
                               fslice_head(),
                             flights2 %>%
                               dplyr::group_by(origin, dest) %>%
                               dplyr::slice_head())
  testthat::expect_equal(flights2 %>%
                           fslice_head(.by = c(origin, dest),
                                       sort_groups = FALSE),
                         flights2 %>%
                           dplyr::slice_head(by = c(origin, dest)))
  testthat::expect_equal(flights2 %>%
                               fslice_head(n = 150),
                             flights2 %>%
                               dplyr::slice_head(n = 150))
  testthat::expect_equal(flights2 %>%
                               dplyr::group_by(origin, dest) %>%
                               fslice_head(n = 4, keep_order = FALSE),
                             flights2 %>%
                               dplyr::group_by(origin, dest) %>%
                               dplyr::slice_head(n = 4))
  testthat::expect_equal(flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice_head(n = 4, keep_order = TRUE),
                         flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice_head(n = 4) %>%
                           dplyr::arrange(id))

  testthat::expect_equal(flights2 %>%
                               fslice_head(n = -4, keep_order = TRUE),
                             flights2 %>%
                               dplyr::slice_head(n = -4) %>%
                               dplyr::arrange(id))
  testthat::expect_equal(flights2 %>%
                           fslice_head(n = -4, keep_order = FALSE),
                         flights2 %>%
                           dplyr::slice_head(n = -4))
  testthat::expect_equal(flights2 %>%
                               dplyr::group_by(origin, dest) %>%
                               fslice_head(n = -10, keep_order = FALSE),
                             flights2 %>%
                               dplyr::group_by(origin, dest) %>%
                               dplyr::slice_head(n = -10))
  testthat::expect_equal(flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice_head(n = -10, keep_order = TRUE),
                         flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice_head(n = -10) %>%
                           dplyr::arrange(id))
})

testthat::test_that("fslice_tail", {
  flights2 <- nycflights13::flights
  set.seed(81243844)
  flights2 <- flights2[sample(seq_len(nrow(flights2)),
                              size = 10^4), ]
  flights2[["id"]] <- seq_len(nrow(flights2))

  testthat::expect_error(flights2 %>%
                           fslice_tail(1, -1))
  testthat::expect_equal(flights2 %>%
                           fslice_tail(),
                         flights2 %>%
                           dplyr::slice_tail())
  testthat::expect_equal(flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice_tail(),
                         flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice_tail())
  testthat::expect_equal(flights2 %>%
                           fslice_tail(.by = c(origin, dest),
                                       sort_groups = FALSE),
                         flights2 %>%
                           dplyr::slice_tail(by = c(origin, dest)))
  testthat::expect_equal(flights2 %>%
                           fslice_tail(n = 150),
                         flights2 %>%
                           dplyr::slice_tail(n = 150))
  testthat::expect_equal(flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice_tail(n = 4, keep_order = FALSE),
                         flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice_tail(n = 4))
  testthat::expect_equal(flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice_tail(n = 4, keep_order = TRUE),
                         flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice_tail(n = 4) %>%
                           dplyr::arrange(id))

  testthat::expect_equal(flights2 %>%
                           fslice_tail(n = -4, keep_order = TRUE),
                         flights2 %>%
                           dplyr::slice_tail(n = -4) %>%
                           dplyr::arrange(id))
  testthat::expect_equal(flights2 %>%
                           fslice_tail(n = -4, keep_order = FALSE),
                         flights2 %>%
                           dplyr::slice_tail(n = -4))
  testthat::expect_equal(flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice_tail(n = -10, keep_order = FALSE),
                         flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice_tail(n = -10))
  testthat::expect_equal(flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice_tail(n = -10, keep_order = TRUE),
                         flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice_tail(n = -10) %>%
                           dplyr::arrange(id))
})
testthat::test_that("fslice_sample", {
  flights2 <- nycflights13::flights
  set.seed(81243844)
  flights2 <- flights2[sample(seq_len(nrow(flights2)),
                              size = 10^4), ]
  flights2[["id"]] <- seq_len(nrow(flights2))

  testthat::expect_error(flights2 %>%
                           fslice_sample(1, -1))
  set.seed(42)
  testthat::expect_equal(flights2 %>%
                           dplyr::slice_sample(n = 1),
                         flights2 %>%
                           fslice_sample(n = 1, seed = 42))
  set.seed(42)
  testthat::expect_equal(flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice_sample(n = Inf),
                         flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice_sample(seed = 42))
  set.seed(42)
  testthat::expect_equal(flights2 %>%
                           dplyr::slice_sample(n = Inf,
                                               by = c(origin, dest)),
                         flights2 %>%
                           fslice_sample(.by = c(origin, dest),
                                         sort_groups = FALSE,
                                         seed = 42))
  set.seed(42)
  testthat::expect_equal(flights2 %>%
                           dplyr::slice_sample(n = 150),
                         flights2 %>%
                           fslice_sample(n = 150, seed = 42))
  set.seed(42)
  testthat::expect_equal(flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice_sample(n = 4),
                         flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice_sample(n = 4, keep_order = FALSE, seed = 42))
  set.seed(42)
  testthat::expect_equal(flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice_sample(n = 4) %>%
                           dplyr::arrange(id),
                         flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice_sample(n = 4, keep_order = TRUE, seed = 42))
  set.seed(42)
  testthat::expect_equal(flights2 %>%
                           dplyr::slice_sample(n = -4) %>%
                           dplyr::arrange(id),
                         flights2 %>%
                           fslice_sample(n = -4, keep_order = TRUE, seed = 42))
  set.seed(42)
  testthat::expect_equal(flights2 %>%
                           dplyr::slice_sample(n = -4),
                         flights2 %>%
                           fslice_sample(n = -4, keep_order = FALSE, seed = 42))
  set.seed(42)
  testthat::expect_equal(flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice_sample(n = -10),
                         flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice_sample(n = -10, keep_order = FALSE, seed = 42))
  set.seed(42)
  testthat::expect_equal(flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           dplyr::slice_sample(n = -10) %>%
                           dplyr::arrange(id),
                         flights2 %>%
                           dplyr::group_by(origin, dest) %>%
                           fslice_sample(n = -10, keep_order = TRUE, seed = 42))
})
