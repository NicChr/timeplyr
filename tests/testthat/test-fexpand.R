# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("Compared to tidyr", {
  flights <- nycflights13::flights
  testdf <- flights %>%
    fcount(dest, origin, time_hour, tailnum, carrier, flight)
  expect_equal(testdf %>%
                 fexpand(),
               empty_tbl()) # tidyr result makes less sense
  expect_equal(testdf %>%
                 fexpand(origin, dest, time_hour, sort = TRUE),
               testdf %>%
                 tidyr::expand(origin, dest, time_hour))
  expect_equal(testdf %>%
                 fexpand(origin, dest, time_hour,
                         expand_type = "nesting", sort = TRUE),
               testdf %>%
                 dplyr::distinct(origin, dest, time_hour) %>%
                 dplyr::select(origin, dest, time_hour) %>%
                 dplyr::arrange(dplyr::pick(dplyr::everything())))
  expect_equal(testdf %>%
                 fcomplete(origin, dest, time_hour,
                           expand_type = "nesting", sort = FALSE),
               testdf)
  expect_equal(testdf %>%
                 fcomplete(sort = TRUE),
               testdf)
  # Grouped calculations
  expect_equal(testdf %>%
                 dplyr::group_by(origin, dest) %>%
                 fexpand(sort = TRUE),
               testdf %>%
                 dplyr::group_by(origin, dest) %>%
                 tidyr::expand())
  expect_equal(testdf %>%
                 dplyr::group_by(origin, dest) %>%
                 fexpand(carrier, sort = TRUE),
               testdf %>%
                 dplyr::group_by(origin, dest) %>%
                 tidyr::expand(carrier))
  expect_equal(testdf %>%
                 dplyr::group_by(origin) %>%
                 fexpand(carrier, tailnum, sort = TRUE),
               testdf %>%
                 dplyr::group_by(origin) %>%
                 tidyr::expand(carrier, tailnum))
  expect_equal(testdf %>%
                 dplyr::group_by(origin) %>%
                 fexpand(carrier, -5:5, sort = TRUE),
               testdf %>%
                 dplyr::group_by(origin) %>%
                 tidyr::expand(carrier, -5:5))
  expect_equal(testdf %>%
                 fexpand(carrier, -5:5, .by = origin,
                         sort = TRUE),
               testdf %>%
                 dplyr::group_by(origin) %>%
                 tidyr::expand(carrier, -5:5) %>%
                 safe_ungroup())
  expect_equal(testdf %>%
                 dplyr::group_by(tailnum) %>%
                 fexpand(carrier, flight, keep_class = FALSE) %>%
                 df_nrow(),
               185292L)
  # expect_equal(testdf %>%
  #                              fexpand(carrier, flight,
  #                                      .by = c(origin, dest, tailnum),
  #                                      keep_class = FALSE) %>%
  #                              df_nrow(),
  #                            187205L)
  # expect_equal(testdf %>%
  #                              dplyr::group_by(origin, dest, tailnum) %>%
  #                              fexpand(carrier, flight,
  #                                      expand_type = "nesting",
  #                                      sort = FALSE),
  #                            testdf %>%
  #                              dplyr::distinct(origin, dest, tailnum, carrier, flight) %>%
  #                              dplyr::group_by(origin, dest, tailnum))
  # testdf %>%
  #   dplyr::group_by(origin, dest, tailnum) %>%
  #   tidyr::expand(carrier, flight))# tidyr cannot handle this :)
  # expect_error(testdf %>%
  #                          fexpand(dplyr::across(dplyr::everything()),
  #                                  expand_type = "cross",
  #                                  log_limit = 6))
  # expect_error(testdf %>%
  #                          fexpand(origin, 1:11, 1:12, 1:13, 1:10^5,
  #                                  expand_type = "nesting",
  #                                  log_limit = 5))
  expect_equal(testdf %>%
                 fexpand(1:10, yes = 1:10,
                         expand_type = "nesting",
                         sort = TRUE),
               tidyr::expand_grid(1:10, 1:10) %>%
                 add_names(c("1:10", "yes")))
  res1 <- flights %>%
    fcomplete(origin, dest, carrier, sort = FALSE)
  res2 <- flights %>%
    tidyr::complete(origin, dest, carrier)
  expect_equal(nrow(dplyr::anti_join(res1, res2, by = names(res1))), 0L)
  expect_equal(nrow(dplyr::anti_join(res2, res1, by = names(res1))), 0L)

  res3 <- flights %>%
    fcomplete(origin, dest, carrier, sort = TRUE)
  expect_equal(res3, res3 %>% dplyr::arrange(origin, dest, carrier))
  res4 <- flights %>%
    fcomplete(origin, dest, carrier, sort = FALSE, fill = list(arr_time = 0,
                                                               dep_time = 9999))
  res5 <- flights %>%
    dplyr::mutate(dplyr::across(c(arr_time, dep_time), as.double)) %>%
    tidyr::complete(origin, dest, carrier, fill = list(arr_time = 0, dep_time = 9999))
  expect_equal(nrow(dplyr::anti_join(res4, res5, by = names(res4))), 0L)
  expect_equal(nrow(dplyr::anti_join(res5, res4, by = names(res4))), 0L)

  expect_error(flights %>%
                 fexpand(across(dplyr::everything())))
})
