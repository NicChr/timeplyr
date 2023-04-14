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
                               fslice(ids),
                             flights %>%
                               dplyr::slice(ids) %>%
                               dplyr::arrange(id))
  testthat::expect_equal(flights %>%
                               dplyr::group_by(origin, dest) %>%
                               fslice(ids),
                             flights %>%
                               dplyr::group_by(origin, dest) %>%
                               dplyr::slice(ids) %>%
                               dplyr::arrange(id))

  testthat::expect_equal(flights %>%
                               fslice(-ids),
                             flights %>%
                               dplyr::slice(-ids) %>%
                               dplyr::arrange(id))
  testthat::expect_equal(flights %>%
                               dplyr::group_by(origin, dest) %>%
                               fslice(1:5),
                             flights %>%
                               dplyr::group_by(origin, dest) %>%
                               dplyr::slice(1:5) %>%
                               dplyr::arrange(id))
  testthat::expect_equal(flights %>%
                               dplyr::group_by(origin, dest) %>%
                               fslice(-ids),
                             flights %>%
                               dplyr::group_by(origin, dest) %>%
                               dplyr::slice(-ids) %>%
                               dplyr::arrange(id))
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
                               dplyr::slice_head() %>%
                               dplyr::arrange(id))
  testthat::expect_equal(flights2 %>%
                               fslice_head(n = 150),
                             flights2 %>%
                               dplyr::slice_head(n = 150) %>%
                               dplyr::arrange(id))
  testthat::expect_equal(flights2 %>%
                               dplyr::group_by(origin, dest) %>%
                               fslice_head(n = 4),
                             flights2 %>%
                               dplyr::group_by(origin, dest) %>%
                               dplyr::slice_head(n = 4) %>%
                               dplyr::arrange(id))

  testthat::expect_equal(flights2 %>%
                               fslice_head(n = -4),
                             flights2 %>%
                               dplyr::slice_head(n = -4) %>%
                               dplyr::arrange(id))
  testthat::expect_equal(flights2 %>%
                               dplyr::group_by(dest) %>%
                               fslice_head(n = -2),
                             flights2 %>%
                               dplyr::group_by(dest) %>%
                               dplyr::slice_head(n = -2) %>%
                               dplyr::arrange(id))
})
