# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

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
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)

  res1 <- flights %>%
    time_count(time = date, time_by = "day", .name = "date")

  # With weights
  set.seed(812123123)
  wt1 <- rep_len(3L, nrow(res1))
  wt2 <- sample2(1:10, size = nrow(res1), replace = TRUE)

  res1 <- res1 %>%
    dplyr::mutate(wt2)
  testthat::expect_equal(res1 %>%
                               dplyr::count(date, wt = wt2),
                             res1 %>%
                               time_count(date, wt = wt2, .name = "date"))
  testthat::expect_equal(res1 %>%
                               dplyr::mutate(wt1) %>%
                               dplyr::count(date, wt = wt1),
                             res1 %>%
                               time_count(date, wt = wt1, .name = "date"))
  testthat::expect_equal(res1 %>%
                               dplyr::count(date, n, wt = wt2),
                             res1 %>%
                               time_count(time = NULL,
                                          date, n, wt = wt2, .name = "date"))
  testthat::expect_equal(res1 %>%
                               dplyr::count(date, wt = n),
                             res1 %>%
                               time_count(date, wt = n, .name = "date"))
  testthat::expect_equal(res1 %>%
                               dplyr::count(date, wt = n),
                             res1 %>%
                               time_count(time = date, wt = n, time_by = "day", .name = "date"))

  testthat::expect_equal(
    flights %>%
      time_count(from = start1,
                 to = end2,
                 time = time_hour,
                 time_by = "hour",
                 time_floor = TRUE, .name = "time_hour") %>%
      dplyr::filter(!is.na(time_hour)),
    flights %>%
      dplyr::filter(time_hour >= time_cast(start1, flights$time_hour) &
                      time_hour <= time_cast(end2, flights$time_hour)) %>%
      fcount(time_hour = time_summarisev(time_hour, from = start1, to = end1,
                                         time_by = "hours", time_floor = TRUE))
    )
  testthat::expect_equal(
    flights %>%
      time_count(from = start1,
                 to = end2,
                 time = time_hour,
                 time_by = "hour",
                 .name = "time_hour"),
    flights %>%
      fcount(time_hour = time_summarisev(time_hour, from = start1, to = end2,
                                         time_by = "hours"))
  )
  testthat::expect_equal(
    flights %>%
      time_count(time = time_hour, time_by = "hour", sort = TRUE, .name = "time_hour"),
    flights %>%
      fcount(time_hour) %>%
      dplyr::arrange(dplyr::desc(n))
  )
})


testthat::test_that("Test flooring", {
  out <- new_tbl(x = time_seq(lubridate::dmy(04102023),
                       length.out = 200,
                       time_by = "days")) %>%
    time_count(x, time_by = "weeks", week_start = 2, time_floor = TRUE,
               .name = "x")

  res <- new_tbl(
    x = lubridate::ymd(c("2023-10-03","2023-10-10","2023-10-17",
          "2023-10-24","2023-10-31","2023-11-07","2023-11-14",
          "2023-11-21","2023-11-28","2023-12-05","2023-12-12","2023-12-19",
          "2023-12-26","2024-01-02","2024-01-09","2024-01-16",
          "2024-01-23","2024-01-30","2024-02-06","2024-02-13","2024-02-20",
          "2024-02-27","2024-03-05","2024-03-12","2024-03-19",
          "2024-03-26","2024-04-02","2024-04-09","2024-04-16")),
    n = c(6L,7L,7L,7L,7L,7L,7L,7L,7L,7L,7L,
          7L,7L,7L,7L,7L,7L,7L,7L,7L,7L,7L,7L,7L,7L,7L,7L,
          7L,5L)
  )

  testthat::expect_equal(out, res)
})

# testthat::test_that("Test intervals", {
#   testthat::expect_equal(
#     new_tbl(x = 1:10) %>%
#       time_count(x, time_by = 3, include_interval = TRUE),
#     new_tbl(x = c(1, 4, 7, 10),
#             interval = add_attr(c(3, 3, 3, 0),
#                                 "start", c(1, 4, 7, 10)),
#             n = c(3, 3, 3, 1))
#   )
#   set.seed(42)
#   df <- new_tbl(x = sample(1:100, replace = T, 10^3)) %>%
#     time_count(x, time_by = 13, include_interval = TRUE, sort = TRUE)
#
#   df$start <- attr(df$interval, "start")
#
#   testthat::expect_equal(df$x, df$start)
# })
