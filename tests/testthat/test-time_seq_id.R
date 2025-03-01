# Set number of data.table threads to 2
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("Time sequence IDs", {
  # Documentation examples
  expect_identical(
    time_seq_id(c(3, 3, 6, NA, NA, 6, NA, 8),
      1,
      na_skip = TRUE
    ),
    c(1L, 1L, 2L, NA, NA, 2L, NA, 3L)
  )
  expect_identical(
    time_seq_id(c(3, 3, 6, NA, NA, 6, NA, 8),
      1,
      na_skip = FALSE
    ),
    c(1L, 1L, 2L, NA, NA, NA, NA, NA)
  )

  set.seed(123876123)
  df <- dplyr::tibble(
    g = sample.int(100, 10^4, replace = TRUE),
    x = round(rnorm(10^4), 1)
  )
  df$x2 <- df$x
  df$x2[sample.int(10^4, 2 * (10^3), replace = FALSE)] <- NA

  # expect_error(time_seq_id(df$x, 1))
  # expect_error(time_seq_id(df$x, g = df$g, 1))
  # expect_error(time_seq_id(df$x, 0.1))
  # expect_error(time_seq_id(df$x, g = df$g, 0.1))

  df <- fastplyr::f_arrange(df, .cols = "x")
  tol <- sqrt(.Machine$double.eps)
  expect_equal(
    time_seq_id(df$x),
    cumsum(c(1, (diff(df$x) - 0.1) > tol))
  )

  x <- c(
    NA, 2, 4, 8, 10, 12, 14, 18,
    6, 12, NA, 12, NA, NA, 14, 14,
    NA, NA, NA, 10, NA, NA, NA, NA
  )
  g <- cheapr::seq_id(rep(8, 3))
  rowid <- seq_along(x)
  # t1 <- time_elapsed(x, g = g, 2, na_skip = TRUE)
  # expect_equal(time_seq_id(x, g = g, 2, na_skip = TRUE),
  #                        c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 1L, 2L, 2L, 2L, 2L, 2L, 2L,
  #                          2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L))
  expect_equal(
    time_seq_id(x, g = g, 2, na_skip = FALSE),
    c(
      1L, NA, NA, NA, NA, NA, NA, NA, 1L, 2L, NA, NA, NA, NA, NA,
      NA, 1L, NA, NA, NA, NA, NA, NA, NA
    )
  )
  set.seed(1239122)
  df <- dplyr::tibble(x, g, rowid)
  df <- dplyr::slice_sample(df, n = nrow(df))
  df <- dplyr::arrange(df, x)
  df$t1 <- time_elapsed(df$x, g = df$g, 1, na_skip = TRUE, fill = 0)
  df <- dplyr::arrange(df, g)
  expect_equal(
    df$t1,
    c(
      0, 2, 4, 2, 2, 2, 4, NA, 0, 6, 0, 2, 0, NA, NA, NA, 0, NA,
      NA, NA, NA, NA, NA, NA
    )
  )
  df <- dplyr::arrange(df, rowid)

  set.seed(1239122)
  g <- sample.int(3, length(x), TRUE)
  df <- dplyr::tibble(x, g, rowid)
  df$t1 <- time_elapsed(df$x, g = df$g, 1, na_skip = FALSE, fill = 0)
  df$t2 <- time_elapsed(df$x, g = df$g, 1, na_skip = TRUE)

  df$t3 <- time_elapsed(df$x,
    g = df$g, 1, na_skip = FALSE,
    rolling = FALSE
  )
  df$t4 <- time_elapsed(df$x,
    g = df$g, 1, na_skip = TRUE,
    rolling = FALSE
  )
  df <- dplyr::arrange(df, x)
  df$id1 <- time_seq_id(df$x, g = df$g, 1, na_skip = FALSE)
  df$id2 <- time_seq_id(df$x, g = df$g, 1, na_skip = TRUE)
  # expect_equal(df$t1, c(0, 2, -12, NA, 0, -4, 8, 4,
  #   -2, 2, NA, NA, 8, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  # expect_equal(df$t2, c(0, 2, -12, NA, 0, -4, 8, 4, -2, 2, 2, 8, 8, 0, NA,
  #   NA, NA, NA, NA, NA, NA, NA, NA, NA))
  # expect_equal(df$t3, c(0, 2, -4, NA,
  #   0, NA, 10, NA, 10, 12, NA, 4, 8, NA, NA, NA, NA, NA, NA,
  #   NA, NA, NA, NA, NA))
  # expect_equal(df$t4,
  #                        c(0, 2, -4, 0, 0, 2, 10, 4, 10,
  #                          12, 6, 4, 8, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  df <- dplyr::arrange(df, g)
  df$id3 <- time_seq_id(df$x, g = df$g, 1, na_skip = FALSE)
  df$id4 <- time_seq_id(df$x, g = df$g, 1, na_skip = TRUE)
  expect_equal(df$id1, df$id3)
  expect_equal(df$id2, df$id4)
})

test_that("Floating point errors", {
  expect_identical(suppressMessages(time_id(seq(0, 20, 0.01))), 1:2001)
  expect_identical(time_id(seq(0, 20, 0.01), 0.01), 1:2001)
})
