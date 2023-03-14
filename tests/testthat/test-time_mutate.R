testthat::test_that("General tests", {
  flights <- nycflights13::flights
  df1 <- flights %>% time_mutate(time = time_hour,
                                 include_interval = FALSE,
                                 by = "week",
                                 .by = dplyr::all_of(c("origin", "dest", "tailnum")),
                                 seq_type = "duration",
                                 .keep = "none") %>%
    fcount(origin, dest, tailnum, time_hour)

  df2 <- flights %>%
    time_count(time = time_hour, by = "week", .by =
                 dplyr::all_of(c("origin", "dest", "tailnum")),
               seq_type = "duration") %>%
    dplyr::filter(.data[["n"]] > 0)
  testthat::expect_equal(nrow2(dplyr::anti_join(df1, df2)), 0)
  testthat::expect_equal(nrow2(dplyr::anti_join(df2, df1)), 0)
})
