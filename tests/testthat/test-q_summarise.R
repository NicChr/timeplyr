# Set number of data.table threads to 1
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("grouped quantile tests", {
  flights <- nycflights13::flights
  x <- unname(stats::quantile(flights$arr_delay, na.rm = TRUE))
  testthat::expect_error(flights %>%
                           q_summarise())
  testthat::expect_equal(flights %>%
    q_summarise(arr_delay, pivot = "long") %>%
      dplyr::pull(arr_delay),
    x)
  testthat::expect_equal(flights %>%
    q_summarise(arr_delay) %>%
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
    q_summarise(arr_delay, .by = origin, sort = TRUE,
              probs = seq(0, 1, 0.2),
              pivot = "long") %>%
    dplyr::select(origin, arr_delay) %>%
    as.list() %>%
    dplyr::as_tibble()
  )


  q_nms <- factor(c("p0", "p25", "p50", "p75", "p100"),
                  levels = c("p0", "p25", "p50", "p75", "p100"))
  testthat::expect_equal(flights %>%
                           dplyr::reframe(arr_delay =
                                            unname(stats::quantile(arr_delay, na.rm = TRUE,
                                                                   probs = seq(0, 1, 0.25))),
                                          .by = origin) %>%
                           transmute2(origin, .quantile = rep(q_nms, 3),
                                     arr_delay),
                         flights %>%
                           q_summarise(arr_delay, pivot = "long", .by = origin,
                                     sort = FALSE) %>%
                           as.list() %>%
                           list_to_tibble()
  )
  testthat::expect_equal(flights %>%
                           dplyr::reframe(arr_delay =
                                            unname(stats::quantile(arr_delay, na.rm = TRUE,
                                                                   probs = seq(0, 1, 0.25))),
                                          .by = origin) %>%
                           transmute2(origin, .quantile = rep(q_nms, 3),
                                      arr_delay) %>%
                           tidyr::pivot_wider(names_from = .quantile,
                                              values_from = arr_delay),
                         flights %>%
                           q_summarise(arr_delay, pivot = "wide", .by = origin,
                                     sort = FALSE) %>%
                           as.list() %>%
                           list_to_tibble()
  )
  testthat::expect_equal(
    flights %>%
      fslice(0) %>%
      q_summarise(arr_delay),
    data.table::data.table(p0 = numeric(0),
                           p25 = numeric(0),
                           p50 = numeric(0),
                           p75 = numeric(0),
                           p100 = numeric(0))
  )
  testthat::expect_equal(
    flights %>%
      fslice(0) %>%
      q_summarise(arr_delay, pivot = "long"),
    data.table::data.table(.quantile = q_nms,
                           arr_delay = NA_real_)
  )
})


