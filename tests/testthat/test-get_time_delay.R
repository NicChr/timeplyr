# Set number of data.table threads to 1
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("time delay", {
  ebola <- dplyr::as_tibble(outbreaks::ebola_sim$linelist)

  df1 <- ebola %>%
    dplyr::mutate(delay = time_diff(date_of_infection,
                                    date_of_onset,
                                    time_by = "2 days")) %>%
    dplyr::filter(!is.na(delay)) %>%
    dplyr::select(date_of_infection,
                  date_of_onset, delay)

  df2 <- ebola %>%
    dplyr::group_by(hospital) %>%
    dplyr::mutate(delay = time_diff(date_of_infection,
                                    date_of_onset,
                                    time_by = "2 days")) %>%
    dplyr::filter(!is.na(delay)) %>%
    dplyr::select(hospital,
                  date_of_infection,
                  date_of_onset, delay)
  df3 <- ebola %>%
    dplyr::mutate(delay = time_diff(date_of_infection,
                                    date_of_onset,
                                    time_by = "2 days")) %>%
    dplyr::slice(0) %>%
    dplyr::select(date_of_infection,
                  date_of_onset, delay)
  df4 <- ebola %>%
    dplyr::mutate(across(c(date_of_infection, date_of_onset), as.numeric)) %>%
    dplyr::mutate(delay = time_diff(date_of_infection,
                                    date_of_onset,
                                    time_by = 2)) %>%
    dplyr::filter(!is.na(delay)) %>%
    dplyr::select(date_of_infection,
                  date_of_onset, delay)
  res1 <- suppressWarnings(get_time_delay(ebola,
                         date_of_infection,
                         date_of_onset,
                         time_by = "2 days"))

  res2 <- suppressWarnings(get_time_delay(ebola %>%
                           dplyr::group_by(hospital),
                         date_of_infection,
                         date_of_onset,
                         time_by = "2 days"))

  res3 <- df3 %>%
    get_time_delay(date_of_infection,
                   date_of_onset,
                   time_by = "2 days") %>%
    suppressWarnings()

  res4 <- suppressWarnings(get_time_delay(ebola,
                                          across(date_of_infection, as.numeric),
                                          across(date_of_onset, as.numeric),
                                          time_by = 2))
  testthat::expect_equal(res1$data,
                             df1)
  testthat::expect_equal(res2$data,
                             df2)
  testthat::expect_equal(res3$data,
                         df3)
  testthat::expect_equal(res4$data,
                         df4)
  testthat::expect_equal(res1$units, "days")
  testthat::expect_equal(res2$units, "days")
  testthat::expect_equal(res3$units, "days")
  testthat::expect_equal(res4$units, "numeric")
  testthat::expect_equal(res1$num, 2)
  testthat::expect_equal(res2$num, 2)
  testthat::expect_equal(res3$num, 2)
  testthat::expect_equal(res4$num, 2)

  testthat::expect_equal(res1$summary, df1 %>%
                           dplyr::summarise(n = dplyr::n(),
                                            min = min(delay),
                                            max = max(delay),
                                            mean = mean(delay),
                                            sd = stats::sd(delay),
                                            p25 = unname(stats::quantile(delay, 0.25)),
                                            p50 = unname(stats::quantile(delay, 0.5)),
                                            p75 = unname(stats::quantile(delay, 0.75)),
                                            p95 = unname(stats::quantile(delay, 0.95)),
                                            iqr = p75 - p25,
                                            # mad = stats::mad(delay),
                                            se = sd/sqrt(n)))
  testthat::expect_equal(res2$summary, df2 %>%
                           dplyr::summarise(n = dplyr::n(),
                                            min = min(delay),
                                            max = max(delay),
                                            mean = mean(delay),
                                            sd = stats::sd(delay),
                                            p25 = unname(stats::quantile(delay, 0.25)),
                                            p50 = unname(stats::quantile(delay, 0.5)),
                                            p75 = unname(stats::quantile(delay, 0.75)),
                                            p95 = unname(stats::quantile(delay, 0.95)),
                                            iqr = p75 - p25,
                                            # mad = stats::mad(delay),
                                            se = sd/sqrt(n),
                                            .groups = "keep"))
  # testthat::expect_equal(res3$summary, df3 %>%
  #                          dplyr::summarise(n = dplyr::n(),
  #                                           min = NA_real_,
  #                                           max = NA_real_,
  #                                           mean = NA_real_,
  #                                           sd = stats::sd(delay),
  #                                           p25 = unname(stats::quantile(delay, 0.25)),
  #                                           p50 = unname(stats::quantile(delay, 0.5)),
  #                                           p75 = unname(stats::quantile(delay, 0.75)),
  #                                           p95 = unname(stats::quantile(delay, 0.95)),
  #                                           iqr = p75 - p25,
  #                                           # mad = stats::mad(delay),
  #                                           se = sd/sqrt(n)))
  testthat::expect_equal(res4$summary, df4 %>%
                           dplyr::summarise(n = dplyr::n(),
                                            min = min(delay),
                                            max = max(delay),
                                            mean = mean(delay),
                                            sd = stats::sd(delay),
                                            p25 = unname(stats::quantile(delay, 0.25)),
                                            p50 = unname(stats::quantile(delay, 0.5)),
                                            p75 = unname(stats::quantile(delay, 0.75)),
                                            p95 = unname(stats::quantile(delay, 0.95)),
                                            iqr = p75 - p25,
                                            # mad = stats::mad(delay),
                                            se = sd/sqrt(n),
                                            .groups = "keep"))
  testthat::expect_equal(res1$delay, df1 %>%
                           dplyr::mutate(delay = ceiling(delay),
                                         edf = dplyr::cume_dist(delay)) %>%
                           fcount(delay, edf) %>%
                           dplyr::mutate(cumulative = cumsum(n)) %>%
                           dplyr::select(delay, n, cumulative, edf))
  testthat::expect_equal(res2$delay, df2 %>%
                           dplyr::mutate(delay = ceiling(delay),
                                         edf = dplyr::cume_dist(delay)) %>%
                           fcount(delay, edf) %>%
                           dplyr::mutate(cumulative = cumsum(n)) %>%
                           dplyr::select(hospital, delay, n, cumulative, edf))
  testthat::expect_equal(res3$delay, df3 %>%
                           dplyr::mutate(delay = ceiling(delay),
                                         edf = dplyr::cume_dist(delay)) %>%
                           fcount(delay, edf) %>%
                           dplyr::mutate(cumulative = cumsum(n)) %>%
                           dplyr::select(delay, n, cumulative, edf))
  testthat::expect_equal(res4$delay, df4 %>%
                           dplyr::mutate(delay = ceiling(delay),
                                         edf = dplyr::cume_dist(delay)) %>%
                           fcount(delay, edf) %>%
                           dplyr::mutate(cumulative = cumsum(n)) %>%
                           dplyr::select(delay, n, cumulative, edf))

  testthat::expect_equal(res1$plot$data,
                         df1)
  testthat::expect_equal(res2$plot$data,
                         add_group_id(df2, .name = ".group"))
  testthat::expect_equal(res3$plot$data,
                         df3)
  testthat::expect_equal(res4$plot$data,
                         df4)


})
