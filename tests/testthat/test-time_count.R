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
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
                               tz = "GB")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)

  res1 <- flights %>%
    time_count(time = date, by = "day")

  # With weights
  set.seed(812123123)
  wt1 <- rep_len(3L, nrow(res1))
  wt2 <- sample2(1:10, size = nrow(res1), replace = TRUE)

  res1 <- res1 %>%
    dplyr::mutate(wt2)
  testthat::expect_equal(res1 %>%
                               dplyr::count(date, wt = wt2),
                             res1 %>%
                               time_count(date, wt = wt2))
  testthat::expect_equal(res1 %>%
                               dplyr::mutate(wt1) %>%
                               dplyr::count(date, wt = wt1),
                             res1 %>%
                               time_count(date, wt = wt1))
  testthat::expect_equal(res1 %>%
                               dplyr::count(date, n, wt = wt2),
                             res1 %>%
                               time_count(date, n, wt = wt2))
  testthat::expect_equal(res1 %>%
                               dplyr::count(date, wt = n),
                             res1 %>%
                               time_count(date, wt = n))
  testthat::expect_equal(res1 %>%
                               dplyr::count(date, wt = n),
                             res1 %>%
                               time_count(time = date, wt = n, by = "day"))

  testthat::expect_equal(
    flights %>%
      time_count(from = start1,
                 to = end2,
                 time = time_hour,
                 by = "hour",
                 complete = FALSE),
    flights %>%
      dplyr::filter(time_hour >= time_cast(start1, flights$time_hour) &
                      time_hour <= time_cast(end2, flights$time_hour)) %>%
      fcount(time_hour)
    )
  testthat::expect_equal(
    flights %>%
      time_count(time = time_hour, by = "hour", sort = TRUE),
    flights %>%
      fcount(time_hour) %>%
      time_complete(time = time_hour, fill = list(n = 0L),
                    by = "hour", sort = FALSE) %>%
      dplyr::arrange(dplyr::desc(n))
  )
  testthat::expect_equal(
    flights %>%
      time_count(time = time_hour, tailnum, by = "year") %>%
      dplyr::select(tailnum, time_hour, n),
    flights %>%
      dplyr::group_by(tailnum) %>%
      dplyr::summarise(time_hour = min(flights$time_hour),
                       n = dplyr::n()) %>%
      safe_ungroup() %>%
      dplyr::select(tailnum, time_hour, n)
  )

  # Intervals
  testthat::expect_equal(
    flights %>%
    time_count(time = time_hour, by = "2 weeks",
               include_interval = TRUE, seq_type = "period") %>%
    dplyr::mutate(n_days = interval / lubridate::days(1)) %>%
    fcount(n_days),
    dplyr::tibble(n_days = c(0.75, 14),
                  n = c(1, 26))
  )
  testthat::expect_equal(
    flights %>%
      time_count(time = time_hour, by = "hour",
                 include_interval = TRUE) %>%
      dplyr::mutate(n_hrs = interval / lubridate::dhours(1)) %>%
      fcount(n_hrs),
    dplyr::tibble(n_hrs = c(0, 1),
                  n = c(1, 8754))
  )
  testthat::expect_equal(
    flights %>%
      time_count(time = time_hour, by = "3.5 hours", include_interval = TRUE) %>%
      dplyr::filter(interval / duration_unit("hours")(1) > 3.5) %>%
      nrow(),
    0L
  )
  testthat::expect_equal(
    flights %>%
      time_count(time = time_hour,
                 include_interval = TRUE, by = "3.5 hours") %>%
    fcount(n_hrs = interval/ duration_unit("hours")(1)),
    dplyr::tibble(n_hrs = c(0.5, 3.5),
                  n = c(1L, 2501L))
  )
  testthat::expect_equal(
    flights %>%
      time_count(time = time_hour,
                 include_interval = TRUE, by = "3.5 weeks",
                 seq_type = "duration") %>%
      fcount(n_hrs = round(interval/ duration_unit("weeks")(1),
                           2)),
    dplyr::tibble(n_hrs = c(3.11, 3.5),
                  n = c(1L, 14L))
  )
  res42 <- flights %>%
    dplyr::mutate(start = start1, end = lubridate::dmy(18112013)) %>%
    time_count(time = time_hour, by = "13.5 hours", from = start, to = end, .by = c(origin, dest),
               include_interval = TRUE)
  testthat::expect_equal(res42$time_hour, lubridate::int_start(res42$interval))
  res42 <- flights %>%
    dplyr::mutate(start = start1, end = lubridate::dmy(18112013)) %>%
    time_count(time = time_hour, by = "13.5 hours", from = start, to = end, .by = c(origin, dest),
               include_interval = TRUE, floor_date = TRUE)
  testthat::expect_equal(res42$time_hour, lubridate::int_start(res42$interval))
  res42 <- flights %>%
    dplyr::mutate(start = start1, end = lubridate::dmy(18112013)) %>%
    time_count(origin, dest, time = time_hour, by = "17 hours", from = start, to = end,
               include_interval = TRUE, floor_date = FALSE)
  testthat::expect_equal(res42$time_hour, lubridate::int_start(res42$interval))
  testthat::expect_equal(
    flights %>%
      time_count(time = time_hour, .by = origin,
                 by = "month"),
    flights %>%
      dplyr::mutate(month = time_summarisev(time_hour, by = "month"),
                    .by = origin) %>%
      fcount(origin, time_hour = month)
  )
  # Warning
  # testthat::expect_warning(flights %>%
  #                            time_count(time = time_hour, lubridate::today()))
  testthat::expect_equal(flights %>% fcount(origin, dest),
                             flights %>% time_count(origin, dest))
  # testthat::expect_equal(flights %>%
  #                              fcount(origin, dest) %>%
  #                              tidyr::pivot_wider(names_from = "origin",
  #                                                 values_from = "n"),
  #                            flights %>%
  #                              time_count(origin, dest,
  #                                         wide_cols = origin))
  testthat::expect_equal(flights %>%
                               dplyr::count(time_hour) %>%
                               tidyr::complete(time_hour = hour_seq,
                                               fill = list(n = 0)),
                             flights %>% time_count(time = time_hour, include_interval = FALSE))
  testthat::expect_equal(flights %>%
                               dplyr::count(date) %>%
                               tidyr::complete(date = date_seq,
                                               fill = list(n = 0)),
                             flights %>% time_count(time = date, by = "day", include_interval = FALSE))
  # testthat::expect_equal(flights %>%
  #                              dplyr::count(date, origin, dest) %>%
  #                              tidyr::complete(date = date_seq,
  #                                              tidyr::nesting(origin, dest),
  #                                              fill = list(n = 0)),
  #                            flights %>% create_ts(date, groups = c(origin, dest),
  #                                                 by = "day", include_interval = FALSE))
  # testthat::expect_equal(flights %>%
  #                              dplyr::count(time_hour, origin, dest) %>%
  #                              tidyr::complete(time_hour = hour_seq,
  #                                              fill = list(n = 0),
  #                                              tidyr::nesting(origin, dest)) %>%
  #                              dplyr::select(origin, dest, dplyr::everything()) %>%
  #                              dplyr::group_by(origin, dest),
  #                            flights %>% dplyr::group_by(origin, dest) %>%
  #                              create_ts(time_hour, by = "hour", include_interval = FALSE))
  # testthat::expect_equal(flights %>%
  #                              dplyr::count(time_hour, origin, dest) %>%
  #                              tidyr::complete(time_hour = hour_seq,
  #                                              fill = list(n = 0),
  #                                              tidyr::nesting(origin, dest)) %>%
  #                              dplyr::select(origin, dest, dplyr::everything()) %>%
  #                              dplyr::group_by(origin, dest),
  #                            flights %>% dplyr::group_by(origin, dest) %>%
  #                              time_count(time = time_hour, by = "hour", include_interval = FALSE))
  # testthat::expect_equal(365L,
  #                            flights %>% create_ts(time_hour, by = "day", include_interval = FALSE) %>%
  #                              nrow())
  testthat::expect_equal(365L,
                             flights %>% time_count(time = time_hour, by = "day", include_interval = FALSE) %>%
                               nrow())
  # testthat::expect_equal(8755L,
  #                            flights %>% create_ts(time_hour, by = "hour", include_interval = FALSE) %>%
  #                              nrow())
  testthat::expect_equal(8755L,
                             flights %>% time_count(time = time_hour, by = "hour", include_interval = FALSE) %>%
                               nrow())
  testthat::expect_equal(flights %>% time_count(time = time_hour, by = "hour", include_interval = FALSE),
                             flights %>% time_count(time = time_hour, by = "hour", include_interval = FALSE) %>%
                               dplyr::arrange(time_hour))
  testthat::expect_equal(flights %>% time_count(time = time_hour, by = "day", include_interval = FALSE),
                             flights %>% time_count(time = time_hour, by = "day", include_interval = FALSE) %>%
                               dplyr::arrange(time_hour))
  # testthat::expect_equal(flights %>% dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
  #                              time_count(time = date, by = "hour", include_interval = FALSE),
  #                            flights %>% dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
  #                              time_count(time = date, by = "hour", include_interval = FALSE) %>%
  #                              dplyr::arrange(date))
  testthat::expect_equal(flights %>% dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
                               time_count(time = date, by = "day", include_interval = FALSE, complete = FALSE),
                             flights %>% dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
                               time_count(time = date, by = "day", include_interval = FALSE, complete = FALSE) %>%
                               dplyr::arrange(date))
  testthat::expect_equal(flights %>% time_count(time = time_hour, by = "week",
                                                  include_interval = FALSE,
                                                  complete = FALSE),
                             flights %>%
                               time_count(time = time_hour, by = "week",
                                        include_interval = FALSE,
                                        complete = FALSE) %>%
                               dplyr::arrange(time_hour))
  # testthat::expect_equal(flights %>% time_count(dest, origin, time = time_hour, by = "week",
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
  testthat::expect_equal(res1,
                             res1 %>%
                               dplyr::arrange(origin, dest, time_hour))
  testthat::expect_equal(res2,
                             res2 %>%
                               dplyr::arrange(origin, dest, time_hour))
  res3 <- flights %>% time_count(dest, origin, time = time_hour, by = "day",
                                 include_interval = FALSE,
                                 complete = TRUE, expand_type = "cross")
  testthat::expect_equal(res3,
                             res3 %>%
                               dplyr::arrange(time_hour, dest, origin))
  res4 <- flights %>% time_count(dest, origin, time = time_hour, by = "week",
                                 include_interval = FALSE,
                                 complete = FALSE)
  testthat::expect_equal(res4,
                             res4 %>%
                               dplyr::arrange(time_hour, dest, origin))
  testthat::expect_equal(flights %>%
                           time_count(time = time_hour, complete = FALSE, by = "quarter",
                                      .by = dest, include_interval = TRUE),
                         flights %>%
                           time_summarise(n = dplyr::n(),
                                          time = time_hour, by = "quarter",
                                          .by = dest, include_interval = TRUE))
  testthat::expect_equal(flights %>%
                           time_count(time = time_hour, complete = TRUE, by = "quarter",
                                      .by = dest, include_interval = TRUE),
                         flights %>%
                           time_summarise(n = dplyr::n(),
                                          time = time_hour, by = "quarter",
                                          .by = dest) %>%
                           time_complete(time = time_hour, by = "quarter",
                                         .by = dest,
                                         fill = list(n = 0)) %>%
                           dplyr::mutate(interval = tseq_interval(flights$time_hour,
                                                                  seq = .$time_hour,
                                                                  gx = group_id(flights$dest),
                                                                  gseq = group_id(.$dest))) %>%
                           dplyr::select(dest, time_hour, interval, n))
})
