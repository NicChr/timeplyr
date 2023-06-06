testthat::test_that("grouped quantile tests", {
  flights <- nycflights13::flights
  x <- unname(stats::quantile(flights$arr_delay, na.rm = TRUE))

  testthat::expect_equal(flights %>%
    q_summary(arr_delay, pivot = "long") %>%
      dplyr::pull(arr_delay),
    x)
  testthat::expect_equal(flights %>%
    q_summary(arr_delay) %>%
    unlist() %>%
    unname(),
    x)
  testthat::expect_equal(
    flights %>%
      dplyr::group_by(origin) %>%
      dplyr::reframe(arr_delay =
                       unname(stats::quantile(arr_delay, na.rm = TRUE,
                                       probs = seq(0, 1, 0.2)))),
  flights %>%
    q_summary(arr_delay, .by = origin, sort = TRUE,
              probs = seq(0, 1, 0.2),
              pivot = "long") %>%
    dplyr::select(origin, arr_delay) %>%
    as.list() %>%
    dplyr::as_tibble()
  )

  flights %>%
    q_summary(arr_delay, pivot = "long", .by = origin)

  # flights %>%
  #   q_summary(arr_delay, .by = origin, sort = T)
  # flights %>%
  #   q_summary(arr_delay, pivot = "long", .by = origin, sort = F)
  # flights %>%
  #   fslice(0) %>%
  #   q_summary(arr_delay)
  # flights %>%
  #   fslice(0) %>%
  #   q_summary(arr_delay, .by = origin)
  #
  # flights %>%
  #   group_by(origin, dest, tailnum) %>%
  #   q_summary(arr_delay, probs = seq(0, 1, 0.05))
})


