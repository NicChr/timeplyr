# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("time expand", {
  df <- nycflights13::flights %>%
    fcount(dest, origin, time_hour, tailnum, carrier, flight)
  df2 <- df %>%
    fcount(dest, origin, time_hour)

# Time expand -------------------------------------------------------------

  # expect_error(df %>%
  #                          time_expand(across(dplyr::everything()),
  #                                      expand_type = "crossing"))
  # expect_error(df %>%
  #                          time_expand(across(dplyr::everything()),
  #                                      expand_type = "nesting",
  #                                      1:10^6,
  #                                      log_limit = 7))
  # expect_equal(
  #   df %>%
  #     time_expand(time = NULL, across(all_of(c("dest", "tailnum", "origin"))),
  #                 expand_type = "crossing",
  #                 keep_class = FALSE),
  #   df %>%
  #     fexpand(across(all_of(c("dest", "tailnum", "origin"))),
  #             sort = TRUE, keep_class = FALSE)
  # )
  expect_equal(
    df %>%
      time_expand(time = NULL, across(all_of(c("dest", "tailnum", "origin"))),
                  expand_type = "nesting",
                  sort = FALSE),
    df %>%
      fdistinct(across(all_of(c("dest", "tailnum", "origin"))))
  )
  expect_equal(
    df2 %>%
      time_expand(time = time_hour),
    df2 %>%
      fdistinct(time_hour) %>%
      fcomplete(time_hour = time_span(time_hour, time_by = "hour"),
                sort = TRUE)
  )
  expect_equal(
    df2 %>%
      time_expand(time = time_hour, time_by = "week"),
    df2 %>%
      fdistinct(time_hour) %>%
      fcomplete(time_hour = time_span(time_hour, time_by = "hour"),
                sort = TRUE) %>%
      dplyr::reframe(time_hour = cut_time2(time_hour,
                                           time_span(time_hour, time_by = "week"))) %>%
      fdistinct()
  )
  expect_equal(
    df2 %>%
      time_expand(time = time_hour, time_by = "week",
                  time_floor = TRUE),
    df2 %>%
      fdistinct(time_hour) %>%
      fcomplete(time_hour = time_span(time_hour, time_by = "hour"),
                sort = TRUE) %>%
      dplyr::reframe(time_hour = cut_time2(time_hour,
                                           time_span(time_hour, time_by = "week",
                                                     time_floor = TRUE))) %>%
      fdistinct()
  )

  # With groups..
  expect_equal(
    df2 %>%
      time_expand(time = time_hour, origin, dest, time_by = "week",
                  time_floor = TRUE),
    df2 %>%
      tidyr::expand(time_hour = time_span(time_hour, time_by = "week",
                                          time_floor = TRUE),
                    tidyr::nesting(origin, dest))
  )
  expect_equal(
    df2 %>%
      time_expand(time = time_hour, origin, dest, time_by = "week",
                  expand_type = "crossing",
                  time_floor = TRUE),
    df2 %>%
      tidyr::expand(time_hour = time_span(time_hour, time_by = "week",
                                          time_floor = TRUE),
                    tidyr::crossing(origin, dest))
  )
  expect_equal(
    df2 %>%
      dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
      time_expand(time = across(date, .names = "time_hour"), .by = c(origin, dest),
                  time_by = "2 week",
                  time_floor = TRUE),
    df2 %>%
      dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
      dplyr::group_by(origin, dest) %>%
      tidyr::expand(time_hour = time_span(date,
                                          time_by = "2 week",
                                          time_floor = TRUE)) %>%
      safe_ungroup()
  )
  expect_equal(
    df2 %>%
      dplyr::group_by(origin, dest) %>%
      time_expand(time = time_hour, time_by = "2 week",
                  time_floor = TRUE, time_type = "duration"),
    df2 %>%
      dplyr::group_by(origin, dest) %>%
      tidyr::expand(time_hour = time_span(time_hour, time_by = "2 week",
                                          time_floor = TRUE,
                                          time_type = "duration"))
  )
  expect_equal(
    df2 %>%
      dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
      dplyr::group_by(dest) %>%
      time_expand(time = date, time_by = "2 week",
                  time_floor = TRUE, time_type = "period"),
    df2 %>%
      dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
      dplyr::group_by(dest) %>%
      tidyr::expand(date = time_span(date, time_by = "2 week",
                                          time_floor = TRUE,
                                          time_type = "period"))
  )
  expect_equal(
    df2 %>%
      time_expand(time = time_hour, origin, dest, time_by = "week",
                  expand_type = "crossing",
                  from = lubridate::dmy(17022013),
                  to = lubridate::dmy_hms(19092013063123),
                  time_floor = FALSE),
    df2 %>%
      tidyr::expand(time_hour = time_span(time_hour, time_by = "week",
                                          time_floor = FALSE,
                                          from = lubridate::dmy(17022013),
                                          to = lubridate::dmy_hms(19092013063123)),
                    origin, dest)
  )
  expect_equal(
    df2 %>%
      time_expand(time = time_hour, origin, dest, time_by = "week",
                  expand_type = "nesting",
                  from = lubridate::dmy(17022013),
                  to = lubridate::dmy_hms(19092013063123),
                  time_floor = FALSE),
    df2 %>%
      tidyr::expand(time_hour = time_span(time_hour, time_by = "week",
                                          time_floor = FALSE,
                                          from = lubridate::dmy(17022013),
                                          to = lubridate::dmy_hms(19092013063123)),
                    tidyr::nesting(origin, dest))
  )
  grouped_res <- df %>%
    dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
    dplyr::group_by(origin, dest, tailnum) %>%
    time_expand(time = date, time_by = "week",
                time_type = "auto") %>%
    add_group_id() %>%
    safe_ungroup() %>%
    dplyr::mutate(min = gmin(date, g = group_id),
                  max = gmax(date, g = group_id)) %>%
    fdistinct(origin, dest, tailnum, min, max)
  base_res <- df %>%
    dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
    add_group_id(origin, dest, tailnum) %>%
    dplyr::arrange(group_id) %>%
    dplyr::mutate(min = gmin(date, g = group_id),
                  max = gmax(date, g = group_id)) %>%
    fdistinct(origin, dest, tailnum, min, max)

  expect_equal(grouped_res %>%
                               dplyr::select(-max),
                             base_res %>%
                               dplyr::select(-max))
  expect_true(all(time_diff(grouped_res$max, base_res$max,
                                      time_by = "days", time_type = "duration") <= 7))


# Time complete -----------------------------------------------------------


  expect_equal(
    df %>%
      time_complete(time = NULL, across(all_of(c("dest", "origin"))),
                  expand_type = "crossing", sort = FALSE),
    df %>%
      fcomplete(across(all_of(c("dest", "origin"))),
                sort = FALSE)
  )
  expect_equal(
    df %>%
      time_complete(time = NULL, across(all_of(c("dest",  "origin"))),
                    expand_type = "crossing", sort = TRUE),
    df %>%
      fcomplete(across(all_of(c("dest", "origin"))),
                sort = TRUE)
  )
  expect_equal(
    df %>%
      time_complete(time = NULL,
                    across(all_of(c("dest", "origin"))),
                  expand_type = "nesting",
                  sort = FALSE),
    df %>%
      fdistinct()
  )
  expect_equal(
    df2 %>%
      time_complete(time = time_hour, sort = TRUE),
    df2 %>%
      fcomplete(time_hour = time_span(time_hour, time_by = "hour"),
                sort = TRUE)
  )
  expect_equal(
    df2 %>%
      time_complete(time = time_hour, time_by = "week"),
    df2 %>%
      fcomplete(time_hour = time_span(time_hour, time_by = "week"),
                sort = TRUE)
  )
  # With groups..
  expect_equal(
    df2 %>%
      time_complete(time = time_hour, origin, dest, time_by = "week",
                  time_floor = TRUE),
    df2 %>%
      tidyr::complete(time_hour = time_span(time_hour, time_by = "week",
                                          time_floor = TRUE),
                    tidyr::nesting(origin, dest)) %>%
      dplyr::select(dest, origin, time_hour, n) %>%
      dplyr::arrange(time_hour, origin, dest)
  )

  expect_equal(dplyr::tibble(a = lubridate::today(),
                                           b = 1) %>%
                               time_complete(b = 1:10,
                                             fill = list(a = lubridate::today() + lubridate::days(1))),
                             dplyr::tibble(a = c(lubridate::today(),
                                                 rep(lubridate::today() + lubridate::days(1),
                                                     9)),
                                           b = 1:10))
})
