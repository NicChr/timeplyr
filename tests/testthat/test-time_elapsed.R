# Set number of data.table threads to 2
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("Time elapsed", {
  # Documentation examples
  expect_equal(
    time_elapsed(c(3, 4, 6, NA, NA, 9), timespan(1),
      rolling = TRUE,
      fill = NA,
      na_skip = TRUE
    ),
    c(NA, 1, 2, NA, NA, 3)
  )
  expect_equal(
    time_elapsed(c(NA, NA, 3, 4, 6, NA, 8), timespan(1),
      rolling = FALSE,
      fill = NA,
      na_skip = TRUE
    ),
    c(NA, NA, 0, 1, 3, NA, 5)
  )
  set.seed(123876123)
  df <- dplyr::tibble(
    g = sample.int(100, 10^4, replace = TRUE),
    x = round(rnorm(10^4), 1)
  )
  df$x2 <- df$x
  df$x2[sample.int(10^4, 2 * (10^3), replace = FALSE)] <- NA

  expect_equal(
    time_elapsed(df$x, timespan(1), rolling = TRUE,
      fill = 0
    ),
    c(0, diff(df$x))
  )
  expect_equal(
    time_elapsed(df$x, timespan(1),
      rolling = TRUE,
      fill = NA
    ),
    c(NA, diff(df$x))
  )
  expect_equal(
    time_elapsed(df$x, timespan(1), rolling = FALSE),
    c(0, cumsum(diff(df$x)))
  )

  expect_equal(
    time_elapsed(df$x2, timespan(1), rolling = FALSE,
      na_skip = TRUE
    ),
    df$x2 - df$x2[1]
  )
  expect_equal(
    time_elapsed(df$x2, timespan(0.1),
      rolling = FALSE,
      na_skip = FALSE
    ),
    (df$x2 - df$x2[1]) / 0.1
  )

  # Grouped -----------------------------------------------------------------

  expect_equal(
    time_elapsed(df$x,
      timespan(1),
      g = df$g,
      rolling = TRUE, fill = 0
    ),
    df |>
      dplyr::mutate(
        t = c(0, diff(x)),
        .by = g
      ) |>
      dplyr::pull(t)
  )
  expect_equal(
    time_elapsed(df$x2,
      timespan(1),
      g = df$g,
      rolling = TRUE, fill = 0,
      na_skip = TRUE
    ),
    df |>
      dplyr::mutate(
        t = time_elapsed(x2,
          timespan(1), na_skip = TRUE,
          fill = 0
        ),
        .by = g
      ) |>
      dplyr::pull(t)
  )
  expect_equal(
    time_elapsed(df$x2,
      timespan(1),
      g = df$g,
      rolling = TRUE, fill = 0,
      na_skip = FALSE
    ),
    df |>
      dplyr::mutate(
        t = time_elapsed(x2,
          timespan(1), na_skip = FALSE,
          fill = 0
        ),
        .by = g
      ) |>
      dplyr::pull(t)
  )
  expect_equal(
    time_elapsed(df$x,
      timespan(1),
      g = df$g,
      rolling = TRUE, fill = 0,
      na_skip = FALSE
    ),
    df |>
      dplyr::mutate(
        t = time_elapsed(x,
          timespan(1), na_skip = FALSE,
          fill = 0
        ),
        .by = g
      ) |>
      dplyr::pull(t)
  )

  expect_equal(
    time_elapsed(df$x2,
      timespan(1),
      g = df$g,
      rolling = FALSE,
      na_skip = FALSE
    ),
    df |>
      dplyr::mutate(
        t = x2 - dplyr::first(x2),
        # t = cheapr::cheapr_if_else(dplyr::row_number() == 1L, 0, t),
        .by = g
      ) |>
      dplyr::pull(t)
  )

  expect_equal(
    time_elapsed(df$x2,
      timespan(1),
      g = df$g,
      rolling = FALSE,
      na_skip = TRUE
    ),
    df |>
      dplyr::mutate(
        t = x2 - dplyr::first(x2, na_rm = TRUE),
        # t = cheapr::cheapr_if_else(dplyr::row_number() == 1L, 0, t),
        .by = g
      ) |>
      dplyr::pull(t)
  )
})
