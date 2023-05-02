testthat::test_that("time expand", {
  flights <- nycflights13::flights %>%
    fcount(dest, origin, time_hour, tailnum, carrier, flight)
  flights2 <- flights %>%
    fcount(dest, origin, time_hour)

# Time expand -------------------------------------------------------------

  testthat::expect_error(flights %>%
                           time_expand(across(dplyr::everything()),
                                       expand_type = "crossing"))
  testthat::expect_error(flights %>%
                           time_expand(across(dplyr::everything()),
                                       expand_type = "nesting",
                                       1:10^6,
                                       log_limit = 7))
  testthat::expect_equal(
    flights %>%
      time_expand(across(all_of(c("dest", "tailnum", "origin"))),
                  expand_type = "crossing",
                  keep_class = FALSE),
    flights %>%
      fexpand(across(all_of(c("dest", "tailnum", "origin"))),
              sort = TRUE, keep_class = FALSE)
  )
  testthat::expect_equal(
    flights %>%
      time_expand(across(all_of(c("dest", "tailnum", "origin"))),
                  expand_type = "nesting",
                  sort = FALSE),
    flights %>%
      fdistinct(across(all_of(c("dest", "tailnum", "origin"))))
  )
  testthat::expect_equal(
    flights2 %>%
      time_expand(time = time_hour),
    flights2 %>%
      fdistinct(time_hour) %>%
      fcomplete(time_hour = time_span(time_hour, by = "hour"),
                sort = TRUE)
  )
  testthat::expect_equal(
    flights2 %>%
      time_expand(time = time_hour, by = "week"),
    flights2 %>%
      fdistinct(time_hour) %>%
      fcomplete(time_hour = time_span(time_hour, by = "hour"),
                sort = TRUE) %>%
      dplyr::reframe(time_hour = cut_time2(time_hour,
                                           time_span(time_hour, by = "week"))) %>%
      fdistinct()
  )
  testthat::expect_equal(
    flights2 %>%
      time_expand(time = time_hour, by = "week",
                  floor_date = TRUE),
    flights2 %>%
      fdistinct(time_hour) %>%
      fcomplete(time_hour = time_span(time_hour, by = "hour"),
                sort = TRUE) %>%
      dplyr::reframe(time_hour = cut_time2(time_hour,
                                           time_span(time_hour, by = "week",
                                                     floor_date = TRUE))) %>%
      fdistinct()
  )

  # With groups..
  testthat::expect_equal(
    flights2 %>%
      time_expand(time = time_hour, origin, dest, by = "week",
                  floor_date = TRUE),
    flights2 %>%
      tidyr::expand(time_hour = time_span(time_hour, by = "week",
                                          floor_date = TRUE),
                    tidyr::nesting(origin, dest))
  )
  testthat::expect_equal(
    flights2 %>%
      time_expand(time = time_hour, origin, dest, by = "week",
                  expand_type = "crossing",
                  floor_date = TRUE),
    flights2 %>%
      tidyr::expand(time_hour = time_span(time_hour, by = "week",
                                          floor_date = TRUE),
                    tidyr::crossing(origin, dest))
  )
  testthat::expect_equal(
    flights2 %>%
      dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
      time_expand(time = across(date, .names = "time_hour"), .by = c(origin, dest),
                  by = "2 week",
                  floor_date = TRUE),
    flights2 %>%
      dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
      dplyr::group_by(origin, dest) %>%
      tidyr::expand(time_hour = time_span(date,
                                          by = "2 week",
                                          floor_date = TRUE)) %>%
      safe_ungroup()
  )
  testthat::expect_equal(
    flights2 %>%
      dplyr::group_by(origin, dest) %>%
      time_expand(time = time_hour, by = "2 week",
                  floor_date = TRUE, seq_type = "duration"),
    flights2 %>%
      dplyr::group_by(origin, dest) %>%
      tidyr::expand(time_hour = time_span(time_hour, by = "2 week",
                                          floor_date = TRUE,
                                          seq_type = "duration"))
  )
  testthat::expect_equal(
    flights2 %>%
      dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
      dplyr::group_by(dest) %>%
      time_expand(time = date, by = "2 week",
                  floor_date = TRUE, seq_type = "period"),
    flights2 %>%
      dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
      dplyr::group_by(dest) %>%
      tidyr::expand(date = time_span(date, by = "2 week",
                                          floor_date = TRUE,
                                          seq_type = "period"))
  )
  testthat::expect_equal(
    flights2 %>%
      time_expand(time = time_hour, origin, dest, by = "week",
                  expand_type = "crossing",
                  from = lubridate::dmy(17022013),
                  to = lubridate::dmy_hms(19092013063123),
                  floor_date = FALSE),
    flights2 %>%
      tidyr::expand(time_hour = time_span(time_hour, by = "week",
                                          floor_date = FALSE,
                                          from = lubridate::dmy(17022013),
                                          to = lubridate::dmy_hms(19092013063123)),
                    origin, dest)
  )
  testthat::expect_equal(
    flights2 %>%
      time_expand(time = time_hour, origin, dest, by = "week",
                  expand_type = "nesting",
                  from = lubridate::dmy(17022013),
                  to = lubridate::dmy_hms(19092013063123),
                  floor_date = FALSE),
    flights2 %>%
      tidyr::expand(time_hour = time_span(time_hour, by = "week",
                                          floor_date = FALSE,
                                          from = lubridate::dmy(17022013),
                                          to = lubridate::dmy_hms(19092013063123)),
                    tidyr::nesting(origin, dest))
  )
  grouped_res <- flights %>%
    dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
    dplyr::group_by(origin, dest, tailnum) %>%
    time_expand(time = date, by = "week",
                seq_type = "auto") %>%
    add_group_id() %>%
    safe_ungroup() %>%
    dplyr::mutate(min = gmin(date, g = group_id),
                  max = gmax(date, g = group_id)) %>%
    fdistinct(origin, dest, tailnum, min, max)
  base_res <- flights %>%
    dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
    add_group_id(origin, dest, tailnum) %>%
    dplyr::arrange(group_id) %>%
    dplyr::mutate(min = gmin(date, g = group_id),
                  max = gmax(date, g = group_id)) %>%
    fdistinct(origin, dest, tailnum, min, max)

  testthat::expect_equal(grouped_res %>%
                               dplyr::select(-max),
                             base_res %>%
                               dplyr::select(-max))
  testthat::expect_true(all(time_diff(grouped_res$max, base_res$max,
                                      by = "days", type = "duration") <= 7))


# Time complete -----------------------------------------------------------


  testthat::expect_equal(
    flights %>%
      time_complete(across(all_of(c("dest", "origin"))),
                  expand_type = "crossing", sort = FALSE),
    flights %>%
      fcomplete(across(all_of(c("dest", "origin"))),
                sort = FALSE)
  )
  testthat::expect_equal(
    flights %>%
      time_complete(across(all_of(c("dest",  "origin"))),
                    expand_type = "crossing", sort = TRUE),
    flights %>%
      fcomplete(across(all_of(c("dest", "origin"))),
                sort = TRUE)
  )
  testthat::expect_equal(
    flights %>%
      time_complete(across(all_of(c("dest", "origin"))),
                  expand_type = "nesting",
                  sort = FALSE),
    flights %>%
      fdistinct()
  )
  testthat::expect_equal(
    flights2 %>%
      time_complete(time = time_hour, sort = TRUE),
    flights2 %>%
      fcomplete(time_hour = time_span(time_hour, by = "hour"),
                sort = TRUE)
  )
  testthat::expect_equal(
    flights2 %>%
      time_complete(time = time_hour, by = "week"),
    flights2 %>%
      fcomplete(time_hour = time_span(time_hour, by = "week"),
                sort = TRUE)
  )
  # With groups..
  testthat::expect_equal(
    flights2 %>%
      time_complete(time = time_hour, origin, dest, by = "week",
                  floor_date = TRUE),
    flights2 %>%
      tidyr::complete(time_hour = time_span(time_hour, by = "week",
                                          floor_date = TRUE),
                    tidyr::nesting(origin, dest)) %>%
      dplyr::select(dest, origin, time_hour, n) %>%
      dplyr::arrange(time_hour, origin, dest)
  )

  testthat::expect_equal(dplyr::tibble(a = lubridate::today(),
                                           b = 1) %>%
                               time_complete(b = 1:10,
                                             fill = list(a = lubridate::today() + lubridate::days(1))),
                             dplyr::tibble(a = c(lubridate::today(),
                                                 rep(lubridate::today() + lubridate::days(1),
                                                     9)),
                                           b = 1:10))
})
