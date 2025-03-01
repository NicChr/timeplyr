# Set number of data.table threads to 2
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("time expand", {
  df <- nycflights13::flights %>%
    fastplyr::f_count(dest, origin, time_hour, tailnum, carrier, flight)
  df2 <- df %>%
    fastplyr::f_count(dest, origin, time_hour)

  # Time expand -------------------------------------------------------------

  expect_equal(
    df %>%
      time_expand(time = NULL, fastplyr::nesting(dest, tailnum, origin),
                  sort = FALSE),
    df %>%
      fastplyr::f_distinct(across(all_of(c("dest", "tailnum", "origin"))))
  )
  expect_equal(
    df2 %>%
      time_expand(time = time_hour),
    df2 %>%
      fastplyr::f_distinct(time_hour) %>%
      fastplyr::f_complete(
        time_hour = time_grid(time_hour, "hour"),
        .sort = TRUE
      )
  )
  # expect_equal(
  #   df2 %>%
  #     time_expand(time = time_hour, "week"),
  #   df2 %>%
  #     fastplyr::f_distinct(time_hour) %>%
  #     fastplyr::f_complete(
  #       time_hour = time_grid(time_hour, "hour"),
  #       .sort = TRUE
  #     ) %>%
  #     dplyr::reframe(time_hour = cut_time2(
  #       time_hour,
  #       time_grid(time_hour, "week")
  #     )) %>%
  #     fastplyr::f_distinct()
  # )
  # expect_equal(
  #   df2 %>%
  #     time_expand(
  #       time = time_hour, "week",
  #       time_floor = TRUE
  #     ),
  #   df2 %>%
  #     fastplyr::f_distinct(time_hour) %>%
  #     fastplyr::f_complete(
  #       time_hour = time_grid(time_hour, "hour"),
  #       .sort = TRUE
  #     ) %>%
  #     dplyr::reframe(time_hour = cut_time2(
  #       time_hour,
  #       time_grid(time_hour,
  #         "week",
  #         time_floor = TRUE
  #       )
  #     )) %>%
  #     fastplyr::f_distinct()
  # )

  # With groups..
  # expect_equal(
  #   df2 %>%
  #     time_expand(
  #       time = time_hour, fastplyr::nesting(origin, dest),
  #       "week", time_floor = TRUE
  #     ),
  #   df2 %>%
  #     tidyr::expand(
  #       time_hour = time_grid(time_hour,
  #         "week",
  #         time_floor = TRUE
  #       ),
  #       tidyr::nesting(origin, dest)
  #     )
  # )
  # expect_equal(
  #   df2 %>%
  #     time_expand(
  #       time = time_hour, fastplyr::crossing(origin, dest),
  #       "week", time_floor = TRUE
  #     ),
  #   df2 %>%
  #     tidyr::expand(
  #       time_hour = time_grid(time_hour,
  #         "week",
  #         time_floor = TRUE
  #       ),
  #       tidyr::crossing(origin, dest)
  #     )
  # )
  # expect_equal(
  #   df2 %>%
  #     dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
  #     time_expand(
  #       time = across(date, .names = "time_hour"), .by = c(origin, dest),
  #       "2 week",
  #       time_floor = TRUE
  #     ),
  #   df2 %>%
  #     dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
  #     dplyr::group_by(origin, dest) %>%
  #     tidyr::expand(time_hour = time_grid(date,
  #       "2 week",
  #       time_floor = TRUE
  #     )) %>%
  #     df_ungroup()
  # )
  # expect_equal(
  #   df2 %>%
  #     dplyr::group_by(origin, dest) %>%
  #     time_expand(
  #       time = time_hour, "2 week",
  #       time_floor = TRUE, time_type = "duration"
  #     ),
  #   df2 %>%
  #     dplyr::group_by(origin, dest) %>%
  #     tidyr::expand(time_hour = time_grid(time_hour,
  #       "2 week",
  #       time_floor = TRUE,
  #       time_type = "duration"
  #     ))
  # )
  # expect_equal(
  #   df2 %>%
  #     dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
  #     dplyr::group_by(dest) %>%
  #     time_expand(
  #       time = date, "2 week",
  #       time_floor = TRUE, time_type = "period"
  #     ),
  #   df2 %>%
  #     dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
  #     dplyr::group_by(dest) %>%
  #     tidyr::expand(date = time_grid(date,
  #       "2 week",
  #       time_floor = TRUE,
  #       time_type = "period"
  #     ))
  # )
  expect_equal(
    df2 %>%
      time_expand(time_hour, fastplyr::crossing(origin, dest),
                  time_by = "week",
        from = lubridate::dmy(17022013),
        to = lubridate::dmy_hms(19092013063123)
      ),
    df2 %>%
      tidyr::expand(
        time_hour = time_grid(time_hour,
          "week",
          from = lubridate::dmy(17022013),
          to = lubridate::dmy_hms(19092013063123)
        ),
        origin, dest
      )
  )
  expect_equal(
    df2 %>%
      time_expand(
        time = time_hour, fastplyr::nesting(origin, dest), time_by = "week",
        from = lubridate::dmy(17022013),
        to = lubridate::dmy_hms(19092013063123)
      ),
    df2 %>%
      tidyr::expand(
        time_hour = time_grid(time_hour,
          "week",
          from = lubridate::dmy(17022013),
          to = lubridate::dmy_hms(19092013063123)
        ),
        tidyr::nesting(origin, dest)
      )
  )
  grouped_res <- df %>%
    dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
    dplyr::group_by(origin, dest, tailnum) %>%
    time_expand(
      time = date, "week",
      time_type = "auto"
    ) %>%
    fastplyr::add_group_id() %>%
    df_ungroup() %>%
    dplyr::mutate(
      min = gmin(date, g = group_id),
      max = gmax(date, g = group_id)
    ) %>%
    fastplyr::f_distinct(origin, dest, tailnum, min, max)
  base_res <- df %>%
    dplyr::mutate(date = lubridate::as_date(time_hour)) %>%
    fastplyr::add_group_id(origin, dest, tailnum) %>%
    dplyr::arrange(group_id) %>%
    dplyr::mutate(
      min = gmin(date, g = group_id),
      max = gmax(date, g = group_id)
    ) %>%
    fastplyr::f_distinct(origin, dest, tailnum, min, max)

  expect_equal(
    grouped_res %>%
      dplyr::select(-max),
    base_res %>%
      dplyr::select(-max)
  )
  expect_true(all(time_diff(grouped_res$max, base_res$max,
    "86400 seconds"
  ) <= 7))


  # Time complete -----------------------------------------------------------


  expect_equal(
    df %>%
      time_complete(time = NULL, fastplyr::crossing(dest, origin), sort = FALSE),
    df %>%
      fastplyr::f_complete(dest, origin, .sort = FALSE)
  )
  expect_equal(
    df %>%
      time_complete(time = NULL, dest, origin, sort = TRUE),
    df %>%
      fastplyr::f_complete(dest, origin, .sort = TRUE)
  )
  expect_equal(
    df %>%
      time_complete(time = NULL, fastplyr::nesting(dest, origin), sort = FALSE),
    df %>%
      fastplyr::f_distinct()
  )
  expect_equal(
    df2 %>%
      time_complete(time = time_hour, sort = TRUE),
    df2 %>%
      fastplyr::f_complete(
        time_hour = time_grid(time_hour, "hour"),
        .sort = TRUE
      )
  )
  expect_equal(
    df2 %>%
      time_complete(time = time_hour, time_by = "week"),
    df2 %>%
      fastplyr::f_complete(
        time_hour = time_grid(time_hour, "week"),
        .sort = TRUE
      )
  )
  # With groups..
  # expect_equal(
  #   df2 %>%
  #     time_complete(
  #       time = time_hour, fastplyr::nesting(origin, dest),
  #       time_by = "week", time_floor = TRUE
  #     ),
  #   df2 %>%
  #     tidyr::complete(
  #       time_hour = time_grid(time_hour,
  #         "week",
  #         time_floor = TRUE
  #       ),
  #       tidyr::nesting(origin, dest)
  #     ) %>%
  #     dplyr::select(dest, origin, time_hour, n) %>%
  #     dplyr::arrange(time_hour, origin, dest)
  # )

  expect_equal(
    dplyr::tibble(
      a = lubridate::today(),
      b = 1
    ) %>%
      time_complete(
        b = 1:10,
        fill = list(a = lubridate::today() + lubridate::days(1))
      ),
    dplyr::tibble(
      a = c(
        lubridate::today(),
        rep(
          lubridate::today() + lubridate::days(1),
          9
        )
      ),
      b = 1:10
    )
  )
})
