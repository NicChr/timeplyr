testthat::test_that("Compare to tidyr", {
  flights <- nycflights13::flights
  flights <- flights %>%
    dplyr::mutate(date = lubridate::as_date(time_hour))
  from1 <- min(flights$time_hour)
  to1 <- max(flights$time_hour)
  from2 <- min(flights$date)
  to2 <- max(flights$date)
  hour_seq <- seq(from1, to1, by = "hour")
  date_seq <- seq(from2, to2, by = "day")
  date_seq2 <- seq(from1, to1, by = "day")
  year_seq <- seq(from2, to2, by = "year")
  testthat::expect_identical(flights %>% fcount(origin, dest),
                             flights %>% time_count(origin, dest))
  testthat::expect_identical(flights %>%
                               fcount(origin, dest) %>%
                               data.table::as.data.table(),
                             flights %>%
                               time_count(origin, dest,
                                                    keep_class = FALSE))
  testthat::expect_identical(flights %>%
                               fcount(origin, dest) %>%
                               tidyr::pivot_wider(names_from = "origin",
                                                  values_from = "n"),
                             flights %>%
                               time_count(origin, dest,
                                          wide_cols = origin))
  testthat::expect_identical(flights %>%
                               dplyr::count(time_hour) %>%
                               tidyr::complete(time_hour = hour_seq,
                                               fill = list(n = 0)),
                             flights %>% time_count(time = time_hour, include_interval = FALSE))
  # testthat::expect_identical(flights %>%
  #                              dplyr::count(time_hour) %>%
  #                              tidyr::complete(time_hour = hour_seq,
  #                                              fill = list(n = 0)),
  #                            flights %>% create_ts(time_hour, include_interval = FALSE))
  # testthat::expect_identical(flights %>%
  #                              dplyr::count(date) %>%
  #                              tidyr::complete(date = date_seq,
  #                                              fill = list(n = 0)),
  #                            flights %>% create_ts(date, by = "day", include_interval = FALSE))
  testthat::expect_identical(flights %>%
                               dplyr::count(date) %>%
                               tidyr::complete(date = date_seq,
                                               fill = list(n = 0)),
                             flights %>% time_count(time = date, by = "day", include_interval = FALSE))
  # testthat::expect_identical(flights %>%
  #                              dplyr::count(date, origin, dest) %>%
  #                              tidyr::complete(date = date_seq,
  #                                              tidyr::nesting(origin, dest),
  #                                              fill = list(n = 0)),
  #                            flights %>% create_ts(date, groups = c(origin, dest),
  #                                                 by = "day", include_interval = FALSE))
  # testthat::expect_identical(flights %>%
  #                              dplyr::count(time_hour, origin, dest) %>%
  #                              tidyr::complete(time_hour = hour_seq,
  #                                              fill = list(n = 0),
  #                                              tidyr::nesting(origin, dest)) %>%
  #                              dplyr::select(origin, dest, dplyr::everything()) %>%
  #                              dplyr::group_by(origin, dest),
  #                            flights %>% dplyr::group_by(origin, dest) %>%
  #                              create_ts(time_hour, by = "hour", include_interval = FALSE))
  # testthat::expect_identical(flights %>%
  #                              dplyr::count(time_hour, origin, dest) %>%
  #                              tidyr::complete(time_hour = hour_seq,
  #                                              fill = list(n = 0),
  #                                              tidyr::nesting(origin, dest)) %>%
  #                              dplyr::select(origin, dest, dplyr::everything()) %>%
  #                              dplyr::group_by(origin, dest),
  #                            flights %>% dplyr::group_by(origin, dest) %>%
  #                              time_count(time = time_hour, by = "hour", include_interval = FALSE))
  # testthat::expect_identical(365L,
  #                            flights %>% create_ts(time_hour, by = "day", include_interval = FALSE) %>%
  #                              nrow())
  testthat::expect_identical(365L,
                             flights %>% time_count(time = time_hour, by = "day", include_interval = FALSE) %>%
                               nrow())
  # testthat::expect_identical(8755L,
  #                            flights %>% create_ts(time_hour, by = "hour", include_interval = FALSE) %>%
  #                              nrow())
  testthat::expect_identical(8755L,
                             flights %>% time_count(time = time_hour, by = "hour", include_interval = FALSE) %>%
                               nrow())
  testthat::expect_identical(flights %>% time_count(time = time_hour, by = "hour", include_interval = FALSE),
                             flights %>% time_count(time = time_hour, by = "hour", include_interval = FALSE) %>%
                               dplyr::arrange(time_hour))
  testthat::expect_identical(flights %>% time_count(time = time_hour, by = "day", include_interval = FALSE),
                             flights %>% time_count(time = time_hour, by = "day", include_interval = FALSE) %>%
                               dplyr::arrange(time_hour))
  # testthat::expect_identical(flights %>% dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
  #                              time_count(time = date, by = "hour", include_interval = FALSE),
  #                            flights %>% dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
  #                              time_count(time = date, by = "hour", include_interval = FALSE) %>%
  #                              dplyr::arrange(date))
  testthat::expect_identical(flights %>% dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
                               time_count(time = date, by = "day", include_interval = FALSE, complete = FALSE),
                             flights %>% dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
                               time_count(time = date, by = "day", include_interval = FALSE, complete = FALSE) %>%
                               dplyr::arrange(date))
  testthat::expect_identical(flights %>% time_count(time = time_hour, by = "week",
                                                  include_interval = FALSE,
                                                  complete = FALSE),
                             flights %>%
                               time_count(time = time_hour, by = "week",
                                        include_interval = FALSE,
                                        complete = FALSE) %>%
                               dplyr::arrange(time_hour))
  # testthat::expect_identical(flights %>% time_count(dest, origin, time = time_hour, by = "week",
  #                                                 include_interval = FALSE,
  #                                                 complete = TRUE),
  #                            flights %>%
  #                              tidyr::expand(time_hour = time_summarisev(time_hour, by = "week", unique = TRUE),
  #                                      tidyr::nesting(origin, dest))
  #                              dplyr::count(time_hour, dest, origin)
  #                              time_count(dest, origin, time = time_hour, by = "week",
  #                                       include_interval = FALSE,
  #                                       complete = TRUE) %>%
  #                              dplyr::arrange(dest, origin, time_hour))
  res1 <- flights %>% time_count(.by = dplyr::all_of(c("origin", "dest")), time = time_hour, by = "day",
                                 include_interval = FALSE,
                                 complete = TRUE)
  res2 <- flights %>% dplyr::group_by(origin, dest) %>%
    time_count(time = time_hour, by = "day",
                                 include_interval = FALSE,
                                 complete = TRUE)
  testthat::expect_identical(res1,
                             res1 %>%
                               dplyr::arrange(origin, dest, time_hour))
  testthat::expect_identical(res2,
                             res2 %>%
                               dplyr::arrange(origin, dest, time_hour))
  res3 <- flights %>% time_count(dest, origin, time = time_hour, by = "day",
                                 include_interval = FALSE,
                                 complete = TRUE, expand_type = "cross")
  testthat::expect_identical(res3,
                             res3 %>%
                               dplyr::arrange(time_hour, dest, origin))
  res4 <- flights %>% time_count(dest, origin, time = time_hour, by = "week",
                                 include_interval = FALSE,
                                 complete = FALSE)
  testthat::expect_identical(res4,
                             res4 %>%
                               dplyr::arrange(time_hour, dest, origin))

})

testthat::test_that("Tests for time_countv", {
  flights2 <- nycflights13::flights
  flights2 <- flights2 %>%
    dplyr::slice_sample(n = nrow(flights2)) %>%
    dplyr::mutate(date = lubridate::as_date(time_hour))
  from <- lubridate::as_datetime(lubridate::dmy(02042013)) +
    lubridate::minutes(35)
  to <- lubridate::dmy(08092013)
  from2 <- bound_from(from, flights2$time_hour)
  to2 <- bound_to(to, flights2$time_hour)
  nrow_flights2 <- flights2 %>%
    dplyr::filter(dplyr::between(time_hour, from2, to2)) %>%
    nrow()
  # Test for if the input order is retained, and whether the count is correct too
  res1 <- flights2 %>%
    dplyr::mutate(n1 = time_countv(time_hour, by = "hour",
                                  include_interval = FALSE, sort = FALSE, unique = FALSE, use.names = FALSE,
                                  complete = FALSE)) %>%
    fadd_count(time_hour, name = "n2") %>%
    dplyr::select(dplyr::all_of(c("time_hour", "n1", "n2"))) %>%
    dplyr::distinct() %>%
    dplyr::arrange(time_hour) %>%
    dplyr::mutate(diff = abs(n1 - n2)) %>%
    fcount(diff)
  testthat::expect_identical(res1, dplyr::tibble(diff = 0L,
                                                 n = 6936L))

  res2 <- time_countv(flights2$time_hour, use.names = FALSE)

  testthat::expect_identical(res2,
                             flights2 %>%
                               fcount(time_hour) %>%
                               time_complete(time = time_hour,
                                             fill = list(n = 0)) %>%
                               dplyr::pull(n))
  res3 <- time_countv(flights2$time_hour, use.names = FALSE,
                      from = from, to = to)
  testthat::expect_identical(res2,
                             flights2 %>%
                               fcount(time_hour) %>%
                               time_complete(time = time_hour,
                                             fill = list(n = 0)) %>%
                               dplyr::pull(n))
  res4 <- time_countv(flights2$time_hour, by = "month",
                      from = from, to = to)
  tbreaks <- time_seq(from, to, by = "month",
                      tz = lubridate::tz(flights2$time_hour))

  testthat::expect_equal(res4,
                         flights2 %>%
                           dplyr::filter(dplyr::between(time_hour,
                                                        from,
                                                        lubridate::with_tz(to, tz = "America/New_York"))) %>%
                           dplyr::mutate(time = cut_time2(time_hour,
                                                   c(tbreaks, max(tbreaks) + 1))) %>%
                           fcount(time) %>%
                           dplyr::pull(n) %>%
                           setnames(tbreaks))

  res5 <- time_countv(flights2$time_hour, by = "month",
                      from = from, to = to, sort = FALSE,
                      use.names = FALSE)

  testthat::expect_equal(res5,
                         flights2 %>%
                           dplyr::filter(dplyr::between(time_hour,
                                                        from,
                                                        lubridate::with_tz(to, tz = "America/New_York"))) %>%
                           dplyr::mutate(time = cut_time2(time_hour,
                                                          c(tbreaks, max(tbreaks) + 1))) %>%
                           dplyr::summarise(n = dplyr::n(), .by = dplyr::all_of("time")) %>%
                           dplyr::pull(n))

  # Unfinished
  res6 <- time_countv(flights2$time_hour, by = "month",
                      from = from, to = to, sort = FALSE,
                      use.names = TRUE,
                      include_interval = TRUE)
  res7 <- time_countv(flights2$time_hour, by = "month",
                      from = from, to = to, sort = FALSE, unique = FALSE,
                      use.names = TRUE,
                      include_interval = TRUE)
  res8 <- time_countv(flights2$time_hour, by = "month",
                      from = from, to = to, sort = TRUE, unique = FALSE,
                      use.names = TRUE,
                      include_interval = TRUE)
  res9 <- time_countv(flights2$time_hour, by = "month",
                      from = from, to = to, sort = TRUE, unique = TRUE,
                      use.names = TRUE,
                      include_interval = TRUE)
  # Unfinished
  res10 <- time_countv(flights2$time_hour, by = "month",
                      from = from, to = to, sort = FALSE,
                      use.names = TRUE,
                      include_interval = FALSE)
  res11 <- time_countv(flights2$time_hour, by = "month",
                      from = from, to = to, sort = FALSE, unique = FALSE,
                      use.names = TRUE,
                      include_interval = FALSE)
  res12 <- time_countv(flights2$time_hour, by = "month",
                      from = from, to = to, sort = TRUE, unique = FALSE,
                      use.names = TRUE,
                      include_interval = FALSE)
  res13 <- time_countv(flights2$time_hour, by = "month",
                      from = from, to = to, sort = TRUE, unique = TRUE,
                      use.names = TRUE,
                      include_interval = FALSE)

})
