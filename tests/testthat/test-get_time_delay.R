# Set number of data.table threads to 2
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("time delay", {
  ebola <- dplyr::as_tibble(outbreaks::ebola_sim$linelist)

  df1 <- ebola |>
    dplyr::mutate(delay = time_diff(
      date_of_infection,
      date_of_onset,
      "2 days"
    )) |>
    dplyr::filter(!is.na(delay)) |>
    dplyr::select(
      date_of_infection,
      date_of_onset, delay
    )

  df2 <- ebola |>
    dplyr::group_by(hospital) |>
    dplyr::mutate(delay = time_diff(
      date_of_infection,
      date_of_onset,
      "2 days"
    )) |>
    dplyr::filter(!is.na(delay)) |>
    dplyr::select(
      hospital,
      date_of_infection,
      date_of_onset, delay
    )
  df3 <- ebola |>
    dplyr::mutate(delay = time_diff(
      date_of_infection,
      date_of_onset,
      "2 days"
    )) |>
    dplyr::slice(0) |>
    dplyr::select(
      date_of_infection,
      date_of_onset, delay
    )
  df4 <- ebola |>
    dplyr::mutate(across(c(date_of_infection, date_of_onset), as.numeric)) |>
    dplyr::mutate(delay = time_diff(
      date_of_infection,
      date_of_onset,
      2
    )) |>
    dplyr::filter(!is.na(delay)) |>
    dplyr::select(
      date_of_infection,
      date_of_onset, delay
    )
  res1 <- suppressWarnings(get_time_delay(
    ebola,
    date_of_infection,
    date_of_onset,
    "2 days"
  ))

  res2 <- suppressWarnings(get_time_delay(
    ebola |>
      dplyr::group_by(hospital),
    date_of_infection,
    date_of_onset,
    "2 days"
  ))

  # res3 <- df3 |>
  #   get_time_delay(
  #     date_of_infection,
  #     date_of_onset,
  #     "2 days"
  #   ) |>
  #   suppressWarnings()

  res4 <- suppressWarnings(get_time_delay(
    ebola,
    across(date_of_infection, as.numeric),
    across(date_of_onset, as.numeric),
    2
  ))
  expect_equal(
    res1$data,
    df1
  )
  expect_equal(
    res2$data,
    df2
  )
  # expect_equal(
  #   res3$data,
  #   df3
  # )
  expect_equal(
    res4$data,
    df4
  )
  expect_equal(res1$units, "days")
  expect_equal(res2$units, "days")
  # expect_equal(res3$units, "days")
  expect_equal(res4$units, NA_character_)
  expect_equal(res1$num, 2)
  expect_equal(res2$num, 2)
  # expect_equal(res3$num, 2)
  expect_equal(res4$num, 2)

  expect_equal(res1$summary, df1 |>
    dplyr::summarise(
      n = dplyr::n(),
      min = min(delay),
      max = max(delay),
      mean = mean(delay),
      sd = stats::sd(delay),
      p5 = unname(stats::quantile(delay, 0.05)),
      p25 = unname(stats::quantile(delay, 0.25)),
      p50 = unname(stats::quantile(delay, 0.5)),
      p75 = unname(stats::quantile(delay, 0.75)),
      p95 = unname(stats::quantile(delay, 0.95)),
      iqr = p75 - p25,
      se = sd / sqrt(n)
    ))
  # expect_equal(res2$summary, df2 |>
  #                          dplyr::summarise(n = dplyr::n(),
  #                                           min = min(delay, na.rm = TRUE),
  #                                           max = max(delay, na.rm = TRUE),
  #                                           mean = mean(delay, na.rm = TRUE),
  #                                           sd = stats::sd(delay),
  #                                           p5 = unname(stats::quantile(delay, 0.05)),
  #                                           p25 = unname(stats::quantile(delay, 0.25)),
  #                                           p50 = unname(stats::quantile(delay, 0.5)),
  #                                           p75 = unname(stats::quantile(delay, 0.75)),
  #                                           p95 = unname(stats::quantile(delay, 0.95)),
  #                                           iqr = p75 - p25,
  #                                           se = sd/sqrt(n)))
  # expect_equal(
  #   res3$summary, df3 |>
  #     dplyr::summarise(
  #       n = dplyr::n(),
  #       min = NA_real_,
  #       max = NA_real_,
  #       mean = NA_real_,
  #       sd = stats::sd(delay),
  #       p5 = unname(stats::quantile(delay, 0.05)),
  #       p25 = unname(stats::quantile(delay, 0.25)),
  #       p50 = unname(stats::quantile(delay, 0.5)),
  #       p75 = unname(stats::quantile(delay, 0.75)),
  #       p95 = unname(stats::quantile(delay, 0.95)),
  #       iqr = p75 - p25,
  #       # mad = stats::mad(delay),
  #       se = sd / sqrt(n)
  #     ) |>
  #     dplyr::slice(0)
  # )
  expect_equal(res4$summary, df4 |>
    dplyr::summarise(
      n = dplyr::n(),
      min = min(delay),
      max = max(delay),
      mean = mean(delay),
      sd = stats::sd(delay),
      p5 = unname(stats::quantile(delay, 0.05)),
      p25 = unname(stats::quantile(delay, 0.25)),
      p50 = unname(stats::quantile(delay, 0.5)),
      p75 = unname(stats::quantile(delay, 0.75)),
      p95 = unname(stats::quantile(delay, 0.95)),
      iqr = p75 - p25,
      # mad = stats::mad(delay),
      se = sd / sqrt(n),
      .groups = "keep"
    ))
  expect_equal(res1$delay, df1 |>
    dplyr::mutate(
      delay = ceiling(delay),
      edf = dplyr::cume_dist(delay)
    ) |>
    fastplyr::f_count(delay, edf) |>
    dplyr::mutate(cumulative = cumsum(n)) |>
    dplyr::select(delay, n, cumulative, edf))
  expect_equal(res2$delay, df2 |>
    dplyr::mutate(
      delay = ceiling(delay),
      edf = dplyr::cume_dist(delay)
    ) |>
    fastplyr::f_count(delay, edf) |>
    dplyr::mutate(cumulative = cumsum(n)) |>
    dplyr::select(hospital, delay, n, cumulative, edf))
  # expect_equal(res3$delay, df3 |>
  #   dplyr::mutate(
  #     delay = ceiling(delay),
  #     edf = dplyr::cume_dist(delay)
  #   ) |>
  #   fastplyr::f_count(delay, edf) |>
  #   dplyr::mutate(cumulative = cumsum(n)) |>
  #   dplyr::select(delay, n, cumulative, edf))
  expect_equal(res4$delay, df4 |>
    dplyr::mutate(
      delay = ceiling(delay),
      edf = dplyr::cume_dist(delay)
    ) |>
    fastplyr::f_count(delay, edf) |>
    dplyr::mutate(cumulative = cumsum(n)) |>
    dplyr::select(delay, n, cumulative, edf))

  expect_equal(
    res1$plot$data,
    df1
  )
  expect_equal(
    res2$plot$data,
    fastplyr::add_group_id(df2, .name = ".group")
  )
  # expect_equal(
  #   res3$plot$data,
  #   df3
  # )
  expect_equal(
    res4$plot$data,
    df4
  )
})
