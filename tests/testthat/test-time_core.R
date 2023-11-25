# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

time_countv2 <- function(..., include_interval = FALSE, use.names = TRUE){
  out <- time_countv(..., include_interval = include_interval)
  if (!include_interval){
   out <- fdeframe(out)
   if (!use.names){
     out <- unname(out)
   }
  }
  out
}

testthat::test_that("Tests for time_countv2", {
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
    dplyr::mutate(n1 = time_countv2(time_hour, time_by = "hour",
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

  res2 <- time_countv2(flights2$time_hour, use.names = FALSE)

  testthat::expect_equal(res2,
                             flights2 %>%
                               fcount(time_hour) %>%
                               time_complete(time = time_hour,
                                             fill = list(n = 0)) %>%
                               dplyr::pull(n))
  res3 <- time_countv2(flights2$time_hour, use.names = FALSE,
                      from = from, to = to)
  testthat::expect_equal(res2,
                             flights2 %>%
                               fcount(time_hour) %>%
                               time_complete(time = time_hour,
                                             fill = list(n = 0)) %>%
                               dplyr::pull(n))
  res4 <- time_countv2(flights2$time_hour,
                      time_by = "month",
                      from = from, to = to)
  testthat::expect_equal(table(time_aggregate_left(flights2$time_hour,
                                                   time_by = "month",
                                                   start = rep_len(from, nrow(flights2)),
                                                   end = rep_len(to, nrow(flights2)))),
                           table(
                             time_summarisev(flights2$time_hour,
                                             time_by = "month",
                                             from = from, to = to)
                           )
  )
  tbreaks <- lubridate::with_tz(
    time_seq(from, to, time_by = "month"),
    lubridate::tz(flights2$time_hour)
  )

  testthat::expect_equal(res4,
                         flights2 %>%
                           dplyr::filter(dplyr::between(time_hour,
                                                        from,
                                                        lubridate::with_tz(to, tz = "America/New_York"))) %>%
                           dplyr::mutate(time = cut_time2(time_hour,
                                                          c(tbreaks, time_cast(to, time_hour) + 1))) %>%
                           fcount(time) %>%
                           dplyr::pull(n) %>%
                           add_names(tbreaks))

  res5 <- time_countv2(flights2$time_hour, time_by = "month",
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
  res6 <- time_countv2(flights2$time_hour, time_by = "month",
                      from = from, to = to, sort = FALSE,
                      use.names = TRUE,
                      include_interval = TRUE)
  res7 <- time_countv2(flights2$time_hour, time_by = "month",
                      from = from, to = to, sort = FALSE, unique = FALSE,
                      use.names = TRUE,
                      include_interval = TRUE)
  res8 <- time_countv2(flights2$time_hour, time_by = "month",
                      from = from, to = to, sort = TRUE, unique = FALSE,
                      use.names = TRUE,
                      include_interval = TRUE)
  res9 <- time_countv2(flights2$time_hour, time_by = "month",
                      from = from, to = to, sort = TRUE, unique = TRUE,
                      use.names = FALSE,
                      include_interval = TRUE)
  testthat::expect_equal(res9,
                         flights2 %>%
                           dplyr::filter(time_hour >= from &
                                           time_hour <= time_cast(to, flights2$time_hour)) %>%
                           dplyr::mutate(x = cut_time2(time_hour,
                                                       time_span(time_hour, time_by = "month",
                                                                 from = from, to = to))) %>%
                           fcount(x) %>%
                           dplyr::mutate(interval = tseq_interval(to, x)) %>%
                           dplyr::select(x, interval, n))
  res9a <- time_countv2(flights2$time_hour,
                       time_by = "hour", include_interval = TRUE)
  res9b <- flights2 %>%
    fcount(x = time_hour) %>%
    time_complete(time = x, time_by = "hour",
                  fill = list(n = 0L),
                  sort = TRUE) %>%
    dplyr::mutate(interval = tseq_interval(max(flights2$time_hour), x)) %>%
    dplyr::select(x, interval, n)
  testthat::expect_true(nrow(dplyr::anti_join(res9a, res9b)) == 0L)
  testthat::expect_true(nrow(dplyr::anti_join(res9b, res9a)) == 0L)
  testthat::expect_identical(
    time_countv2(flights2$time_hour, time_by = "3.5 hours", include_interval = TRUE) %>%
      dplyr::filter(interval / duration_unit("hours")(1) > 3.5) %>%
      nrow(),
    0L
  )
  # Unfinished
  res10 <- time_countv2(flights2$time_hour, time_by = "month",
                       from = from, to = to, sort = FALSE,
                       use.names = TRUE,
                       include_interval = FALSE)
  res11 <- time_countv2(flights2$time_hour, time_by = "month",
                       from = from, to = to, sort = FALSE, unique = FALSE,
                       use.names = TRUE,
                       include_interval = FALSE)
  res12 <- time_countv2(flights2$time_hour, time_by = "month",
                       from = from, to = to, sort = TRUE, unique = FALSE,
                       use.names = TRUE,
                       include_interval = FALSE)
  res13 <- time_countv2(flights2$time_hour, time_by = "month",
                       from = from, to = to, sort = TRUE, unique = TRUE,
                       use.names = TRUE,
                       include_interval = FALSE)
  x <- flights2$time_hour
  res <- time_countv2(x, sort = FALSE, unique = TRUE, time_by = "2 weeks", include_interval = TRUE)
  testthat::expect_equal(res$x,
                         lubridate::int_start(res$interval))
  res <- time_countv2(x[c(1:100, 30000:35000, 100000:100001)],
                         sort = TRUE, unique = FALSE, time_by = "week", include_interval = TRUE,
                         time_floor = TRUE)
  testthat::expect_equal(res$x,
                         lubridate::int_start(res$interval))

})

testthat::test_that("Tests for time_span", {
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  x <- nycflights13::flights$time_hour
  y <- lubridate::as_date(x)

  testthat::expect_equal(time_span(x),
                             time_seq(min(x), max(x), time_by = "hour"))
  testthat::expect_equal(time_span(y),
                             time_seq(min(y), max(y), time_by = "day"))
  testthat::expect_equal(time_span(x, from = start1, to = end1),
                         lubridate::with_tz(
                           time_seq(start1, end1, time_by = "hour"),
                           "America/New_York")
  )
  testthat::expect_equal(time_span(x, from = start2, to = end2),
                         lubridate::with_tz(
                           time_seq(start2, end2, time_by = "hour"),
                           "America/New_York"
                         )
  )
  testthat::expect_equal(time_span(x, from = start2, time_by = "week", time_floor = TRUE,
                                   time_type = "period"),
                         lubridate::with_tz(
                           time_seq(start2, max(x),
                                    time_by = "week",
                                    time_floor = TRUE),
                           "America/New_York")
  )
  testthat::expect_equal(time_span(x, to = end2, time_by = "week", time_floor = TRUE,
                                   time_type = "period"),
                         lubridate::with_tz(
                           time_seq(min(x), end2,
                                    time_by = "week",
                                    time_floor = TRUE),
                           "America/New_York")
  )
})

testthat::test_that("Tests for time_completev", {
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  x <- nycflights13::flights$time_hour
  y <- lubridate::as_date(x)

  tseq <- time_span(x, time_by = "hour")
  x_missed <- time_cast(setdiff(tseq, x), tseq)
  testthat::expect_identical(time_completev(x, sort = FALSE, time_by = "hour"),
                             c(x, x_missed))
  testthat::expect_identical(time_completev(x, sort = TRUE, time_by = "hour"),
                             radix_sort(c(x, x_missed)))
})
testthat::test_that("Tests for time_summarisev", {
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  x <- nycflights13::flights$time_hour
  y <- lubridate::as_date(x)
  x_max <- max(x)
  tseq <- time_span(x, time_by = "hour")
  x_missed <- time_cast(setdiff(tseq, x), tseq)
  # Without interval
  testthat::expect_identical(time_summarisev(x, sort = TRUE, unique = TRUE,
                                             time_by = "2 weeks"),
                             time_span(x, time_by = "2 weeks"))
  testthat::expect_identical(time_summarisev(x, sort = FALSE, unique = FALSE,
                                             time_by = "2 weeks"),
                             cut_time2(x, time_span(x, time_by = "2 weeks")))
  testthat::expect_identical(time_summarisev(x, sort = TRUE, unique = FALSE,
                                             time_by = "2 weeks"),
                             radix_sort(cut_time2(x, time_span(x, time_by = "2 weeks"))))
  testthat::expect_identical(time_summarisev(x, sort = FALSE, unique = TRUE,
                                             time_by = "2 weeks"),
                             unique(cut_time2(x, time_span(x, time_by = "2 weeks"))))
  # With interval
  testthat::expect_identical(
    time_summarisev(x, sort = TRUE, unique = TRUE,
                    time_by = "2 weeks", time_floor = TRUE,
                    include_interval = TRUE),
    dplyr::tibble(x = time_span(x, time_by = "2 weeks", time_floor = TRUE)) %>%
      dplyr::mutate(interval = tseq_interval(x_max, x))
  )

  res <- time_summarisev(x, sort = FALSE, unique = TRUE, time_by = "2 weeks", include_interval = TRUE)
  testthat::expect_equal(res$x,
                         lubridate::int_start(res$interval))
  res <- time_summarisev(x[c(1:100, 30000:35000, 100000:100001)],
                         sort = TRUE, unique = FALSE, time_by = "week", include_interval = TRUE,
                         time_floor = TRUE)
  testthat::expect_equal(res$x,
                         lubridate::int_start(res$interval))

  # Test for out-of-bounds from argument
  testthat::expect_equal(time_summarisev(lubridate::dmy(21022020),
                                         time_by = "week",
                                         from = lubridate::dmy(01012020)),
                         lubridate::dmy(19022020))
  testthat::expect_equal(time_summarisev(lubridate::dmy(21022020),
                                         time_by = "week"),
                         lubridate::dmy(21022020))

})

