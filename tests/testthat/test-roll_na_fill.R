# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("NA fill", {
  set.seed(42)
  words <- do.call(paste0,
                   do.call(expand.grid, rep(list(letters), 3)))
  groups <- sample(words, size = 10^5, replace = TRUE)
  groups2 <- sample(words, size = 10^5, replace = TRUE)
  x <- sample.int(10^2, 10^5, TRUE)
  y <- na_fill(rnorm(10^5), prop = 3/5)
  z <- na_fill(groups2, prop = 3/5)
  x[sample.int(10^5, 10^4)] <- NA

  df <- new_tbl(x = x, y = y, z = z, groups = groups, groups2 = groups2)
  sorted_df <- dplyr::arrange(df, groups)

  testthat::expect_identical(
    roll_na_fill(x),
    vctrs::vec_fill_missing(x, direction = "down")
  )
  testthat::expect_equal(
    roll_na_fill(y),
    vctrs::vec_fill_missing(y, direction = "down")
  )
  testthat::expect_equal(
    roll_na_fill(z),
    vctrs::vec_fill_missing(z, direction = "down")
  )
  testthat::expect_identical(
    roll_na_fill(x, g = groups),
    dplyr::pull(
      dplyr::mutate(df, filled =
                      vctrs::vec_fill_missing(x, direction = "down"),
                    .by = groups),
      filled
    )
  )
  testthat::expect_equal(
    roll_na_fill(y, g = groups),
    dplyr::pull(
      dplyr::mutate(df, filled =
                      vctrs::vec_fill_missing(y, direction = "down"),
                    .by = groups),
      filled
    )
  )
  testthat::expect_equal(
    roll_na_fill(z, g = groups),
    dplyr::pull(
      dplyr::mutate(df, filled =
                      vctrs::vec_fill_missing(z, direction = "down"),
                    .by = groups),
      filled
    )
  )
  testthat::expect_identical(
    roll_na_fill(sorted_df$x, g = sorted_df$groups),
    dplyr::pull(
      dplyr::mutate(sorted_df, filled =
                      vctrs::vec_fill_missing(x, direction = "down"),
                    .by = groups),
      filled
    )
  )
  testthat::expect_equal(
    roll_na_fill(sorted_df$y, g = sorted_df$groups),
    dplyr::pull(
      dplyr::mutate(sorted_df, filled =
                      vctrs::vec_fill_missing(y, direction = "down"),
                    .by = groups),
      filled
    )
  )
  testthat::expect_identical(
    roll_na_fill(sorted_df$z, g = sorted_df$groups),
    dplyr::pull(
      dplyr::mutate(sorted_df, filled =
                      vctrs::vec_fill_missing(z, direction = "down"),
                    .by = groups),
      filled
    )
  )

  ### Fill limit
  testthat::expect_equal(
    roll_na_fill(sorted_df$y, g = sorted_df$groups, fill_limit = 1),
    dplyr::pull(
      dplyr::mutate(sorted_df, filled =
                      vctrs::vec_fill_missing(y, direction = "down", max_fill = 1),
                    .by = groups),
      filled
    )
  )
  testthat::expect_equal(
    roll_na_fill(sorted_df$y, g = sorted_df$groups, fill_limit = 2),
    dplyr::pull(
      dplyr::mutate(sorted_df, filled =
                      vctrs::vec_fill_missing(y, direction = "down", max_fill = 2),
                    .by = groups),
      filled
    )
  )
})
