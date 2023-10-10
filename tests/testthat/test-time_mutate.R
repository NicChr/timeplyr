testthat::test_that("General tests", {
  flights <- nycflights13::flights
  start <- lubridate::ymd_hms("2013-03-16 11:43:48",
                              tz = "Europe/London")
  end <- start + lubridate::ddays(10)
  testthat::expect_equal(flights,
                         flights %>% time_mutate(time_hour))
  df1 <- flights %>% time_mutate(time = time_hour,
                                 include_interval = FALSE,
                                 time_by = "week",
                                 .by = dplyr::all_of(c("origin", "dest", "tailnum")),
                                 time_type = "duration",
                                 .keep = "none") %>%
    fcount(origin, dest, tailnum, time_hour)

  df2 <- flights %>%
    time_count(time = time_hour, time_by = "week", .by =
                 dplyr::all_of(c("origin", "dest", "tailnum")),
               time_type = "duration") %>%
    dplyr::filter(.data[["n"]] > 0)
  testthat::expect_equal(nrow2(dplyr::anti_join(df1, df2)), 0)
  testthat::expect_equal(nrow2(dplyr::anti_join(df2, df1)), 0)

  flights2 <- dplyr::select(flights, time_hour, origin, dest)
  testthat::expect_equal(
    flights2 %>%
      time_mutate(time = time_hour,
                time_by = "week",
                from = start) %>%
      dplyr::pull(time_hour),
    time_summarisev(flights2$time_hour,
                    time_by = "week",
                    from = start)
  )
  testthat::expect_equal(
    flights2 %>%
      time_mutate(time = time_hour,
                  time_by = "week",
                  time_type = "period",
                  .by = c(origin, dest)) %>%
      dplyr::pull(time_hour),
    flights2 %>%
      dplyr::mutate(time_hour = time_summarisev(time_hour,
                                                time_by = "week",
                                                time_type = "period"),
                    .by = c(origin, dest)) %>%
      dplyr::pull(time_hour)
  )
  testthat::expect_equal(
    flights2 %>%
      time_mutate(time = time_hour,
                  time_by = "week",
                  .by = c(origin, dest),
                  time_type = "duration") %>%
      dplyr::pull(time_hour),
    flights2 %>%
      dplyr::mutate(time_hour = time_summarisev(time_hour,
                                                time_by = "week",
                                                time_type = "duration"),
                    .by = c(origin, dest)) %>%
      dplyr::pull(time_hour)
  )
})
