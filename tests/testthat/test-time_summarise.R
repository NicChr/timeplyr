# testthat::test_that("General tests", {
#   flights <- nycflights13::flights
#   flights <- flights %>%
#     dplyr::mutate(date = lubridate::as_date(time_hour))
#   testthat::expect_identical(flights %>%
#                                dplyr::count(time_hour) %>%
#                                tidyr::complete(time_hour = hour_seq,
#                                                fill = list(n = 0)),
#                              flights %>% ts_count(time = time_hour, include_interval = FALSE))
# })
#
# as_tibble(time_summarisev(sample(flights$time_hour), by = "week", include_interval = TRUE)) %>% distinct() %>%
#   mutate(start = interval$start) %>% fcount(start == x)
testthat::test_that("General tests", {
  flights <- nycflights13::flights
  df1 <- time_summarisev(flights$time_hour,
                         by = "week",
                         unique = FALSE,
                         include_interval = TRUE,
                         from = lubridate::as_datetime(lubridate::dmy(02042013)) +
                           lubridate::minutes(35),
                         to = lubridate::dmy(08092013)) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct() %>%
    dplyr::arrange(x)
  NA_NY <- lubridate::with_tz(lubridate::NA_POSIXct_,
                              "America/New_York")
  df2 <- time_summarisev(flights$time_hour,
                         by = "week",
                         unique = TRUE,
                         include_interval = TRUE,
                         from = lubridate::as_datetime(lubridate::dmy(02042013)) +
                           lubridate::minutes(35),
                         to = lubridate::dmy(08092013)) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct() %>%
    dplyr::arrange(x)
  df3 <- time_summarisev(flights$time_hour,
                         by = "week",
                         unique = FALSE,
                         include_interval = TRUE,
                         from = lubridate::as_datetime(lubridate::dmy(02042013)) +
                           lubridate::minutes(35) - lubridate::years(1),
                         to = lubridate::dmy(08092013) +
                           lubridate::years(1)) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct() %>%
    dplyr::arrange(x)

  df4 <- time_summarisev(flights$time_hour,
                         by = "week",
                         unique = TRUE,
                         include_interval = TRUE,
                         from = lubridate::as_datetime(lubridate::dmy(02042013)) +
                           lubridate::minutes(35) - lubridate::years(1),
                         to = lubridate::dmy(08092013) + lubridate::years(1)) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct() %>%
    dplyr::arrange(x)
  NA_tbl <- dplyr::tibble(x = NA_NY,
                          interval = lubridate::interval(NA_NY,
                                                         NA_NY))
  empty_tbl <- dplyr::slice(NA_tbl, 0L)
  # testthat::expect_identical(dplyr::anti_join(df1, df2), NA_tbl)
  testthat::expect_identical(dplyr::anti_join(df1, df2), empty_tbl)
  testthat::expect_identical(dplyr::anti_join(df2, df1), empty_tbl)
  testthat::expect_identical(dplyr::anti_join(df3, df4), empty_tbl)
  testthat::expect_identical(dplyr::anti_join(df4, df3), empty_tbl)
})


# flights %>%
#   select(time_hour) %>%
#   mutate(time_agg = time_summarisev(time_hour, by = "week",
#                                     from = dmy(03062013),
#                                     to = dmy(07082013))) %>%
#   group_by(time_agg) %>%
#   reframe(min = min(time_hour),
#           max = max(time_hour))
#
# time_summarisev(flights$time_hour, by = "week",
#                 from = dmy(03062013),
#                 to = dmy(07082013)) %>%
#   table()
# time_countv(flights$time_hour, by = "week",
#             from = dmy(03062013),
#             to = dmy(07082013),
#             unique = TRUE)
