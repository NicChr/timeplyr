# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Compared to tidyr", {
  flights <- nycflights13::flights
  testdf <- flights %>%
    fcount(dest, origin, time_hour, tailnum, carrier, flight)
  # testthat::expect_equal(testdf %>%
  #                              fexpand(),
  #                            testdf %>%
  #                              tidyr::expand()) # tidyr result makes less sense
  testthat::expect_equal(testdf %>%
                               fexpand(origin, dest, time_hour, sort = TRUE),
                             testdf %>%
                               tidyr::expand(origin, dest, time_hour))
  testthat::expect_equal(testdf %>%
                               fexpand(origin, dest, time_hour,
                                       expand_type = "nesting", sort = TRUE),
                             testdf %>%
                               dplyr::distinct(origin, dest, time_hour) %>%
                               dplyr::select(origin, dest, time_hour) %>%
                               dplyr::arrange(dplyr::pick(dplyr::everything())))
  testthat::expect_equal(testdf %>%
                               fcomplete(origin, dest, time_hour,
                                       expand_type = "nesting", sort = FALSE),
                             testdf)
  testthat::expect_equal(testdf %>%
                               fcomplete(sort = TRUE),
                             testdf)
  # Grouped calculations
  testthat::expect_equal(testdf %>%
                               dplyr::group_by(origin, dest) %>%
                               fexpand(sort = TRUE),
                             testdf %>%
                               dplyr::group_by(origin, dest) %>%
                               tidyr::expand())
  testthat::expect_equal(testdf %>%
                           dplyr::group_by(origin, dest) %>%
                               fexpand(carrier, sort = TRUE),
                             testdf %>%
                               dplyr::group_by(origin, dest) %>%
                               tidyr::expand(carrier))
  testthat::expect_equal(testdf %>%
                           dplyr::group_by(origin) %>%
                               fexpand(carrier, tailnum, sort = TRUE),
                             testdf %>%
                               dplyr::group_by(origin) %>%
                               tidyr::expand(carrier, tailnum))
  testthat::expect_equal(testdf %>%
                           dplyr::group_by(origin) %>%
                               fexpand(carrier, -5:5, sort = TRUE),
                             testdf %>%
                               dplyr::group_by(origin) %>%
                               tidyr::expand(carrier, -5:5))
  testthat::expect_equal(testdf %>%
                               fexpand(carrier, -5:5, .by = origin,
                                       sort = TRUE),
                             testdf %>%
                               dplyr::group_by(origin) %>%
                               tidyr::expand(carrier, -5:5) %>%
                               safe_ungroup())
  testthat::expect_equal(testdf %>%
                           dplyr::group_by(tailnum) %>%
                               fexpand(carrier, flight, keep_class = FALSE) %>%
                               df_nrow(),
                             185292L)
  # testthat::expect_equal(testdf %>%
  #                              fexpand(carrier, flight,
  #                                      .by = c(origin, dest, tailnum),
  #                                      keep_class = FALSE) %>%
  #                              df_nrow(),
  #                            187205L)
  testthat::expect_equal(testdf %>%
                               dplyr::group_by(origin, dest, tailnum) %>%
                               fexpand(carrier, flight,
                                       expand_type = "nesting",
                                       sort = FALSE),
                             testdf %>%
                               dplyr::distinct(origin, dest, tailnum, carrier, flight) %>%
                               dplyr::group_by(origin, dest, tailnum))
                             # testdf %>%
                             #   dplyr::group_by(origin, dest, tailnum) %>%
                             #   tidyr::expand(carrier, flight))# tidyr cannot handle this :)
  testthat::expect_error(testdf %>%
                           fexpand(dplyr::across(dplyr::everything()),
                                   expand_type = "cross",
                                   log_limit = 6))
  testthat::expect_error(testdf %>%
                           fexpand(origin, 1:11, 1:12, 1:13, 1:10^5,
                                   expand_type = "nesting",
                                   log_limit = 5))
  testthat::expect_equal(testdf %>%
                                 fexpand(1:10, yes = 1:10,
                                         expand_type = "nesting",
                                         sort = TRUE),
                             tidyr::expand_grid(1:10, 1:10) %>%
                               add_names(c("1:10", "yes")))
  res1 <- flights %>%
    fcomplete(origin, dest, carrier, sort = FALSE)
  res2 <- flights %>%
    tidyr::complete(origin, dest, carrier)
  testthat::expect_equal(nrow(dplyr::anti_join(res1, res2, by = names(res1))), 0L)
  testthat::expect_equal(nrow(dplyr::anti_join(res2, res1, by = names(res1))), 0L)

  res3 <- flights %>%
    fcomplete(origin, dest, carrier, sort = TRUE)
  testthat::expect_equal(res3, res3 %>% dplyr::arrange(origin, dest, carrier))
  res4 <- flights %>%
    fcomplete(origin, dest, carrier, sort = FALSE, fill = list(arr_time = 0,
                                                              dep_time = 9999))
  res5 <- flights %>%
    dplyr::mutate(dplyr::across(c(arr_time, dep_time), as.double)) %>%
    tidyr::complete(origin, dest, carrier, fill = list(arr_time = 0, dep_time = 9999))
  testthat::expect_equal(nrow(dplyr::anti_join(res4, res5, by = names(res4))), 0L)
  testthat::expect_equal(nrow(dplyr::anti_join(res5, res4, by = names(res4))), 0L)

  testthat::expect_error(flights %>%
                           fexpand(across(dplyr::everything())))
})
