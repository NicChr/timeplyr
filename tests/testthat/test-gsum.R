# Set number of data.table threads to 1
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Test grouped stat functions", {
flights <- nycflights13::flights
g1 <- group_id(flights, origin, dest)
g2 <- group_id(flights, origin, dest, order = FALSE)
g3 <- group_id(flights, origin, dest, order = FALSE, as_qg = TRUE)
g4 <- group_id(flights, origin, dest, order = TRUE, as_qg = TRUE)
g5 <- collapse::GRP(g1, sort = TRUE)
g6 <- collapse::GRP(g1, sort = FALSE)
g7 <- collapse::GRP(g2, sort = TRUE)
g8 <- collapse::GRP(g2, sort = FALSE)
g9 <- collapse::GRP(g3, sort = TRUE)
g10 <- collapse::GRP(g3, sort = FALSE)
g11 <- collapse::GRP(g4, sort = TRUE)
g12 <- collapse::GRP(g4, sort = FALSE)
g13 <- collapse::GRP(g5, sort = TRUE)
g14 <- collapse::GRP(g5, sort = FALSE)
g15 <- collapse::GRP(g6, sort = TRUE)
g16 <- collapse::GRP(g6, sort = FALSE)

res <- flights %>%
  dplyr::mutate(min = min(time_hour),
                max = max(time_hour),
                mean = mean(time_hour),
                sum = sum(sched_arr_time, na.rm = TRUE),
                sd = stats::sd(sched_arr_time, na.rm = TRUE),
                var = stats::var(sched_arr_time, na.rm = TRUE),
                mode = collapse::fmode(sched_arr_time, na.rm = TRUE),
                median = stats::median(sched_arr_time, na.rm = TRUE),
                first = dplyr::first(sched_arr_time, na_rm = TRUE),
                last = dplyr::last(sched_arr_time, na_rm = TRUE),
                nobs = sum(!is.na(arr_time)))
gres1 <- flights %>%
  dplyr::mutate(min = min(time_hour),
                max = max(time_hour),
                mean = mean(time_hour),
                sum = sum(sched_arr_time, na.rm = TRUE),
                sd = stats::sd(sched_arr_time, na.rm = TRUE),
                var = stats::var(sched_arr_time, na.rm = TRUE),
                mode = collapse::fmode(sched_arr_time, na.rm = TRUE),
                median = stats::median(sched_arr_time, na.rm = TRUE),
                first = dplyr::first(sched_arr_time, na_rm = TRUE),
                last = dplyr::last(sched_arr_time, na_rm = TRUE),
                nobs = sum(!is.na(arr_time)),
                .by = c(origin, dest))
testthat::expect_equal(res,
                           flights %>%
                             dplyr::mutate(min = gmin(time_hour),
                                           max = gmax(time_hour),
                                           mean = gmean(time_hour),
                                           sum = gsum(sched_arr_time, na.rm = TRUE),
                                           sd = gsd(sched_arr_time, na.rm = TRUE),
                                           var = gvar(sched_arr_time, na.rm = TRUE),
                                           mode = gmode(sched_arr_time, na.rm = TRUE),
                                           median = gmedian(sched_arr_time, na.rm = TRUE),
                                           first = gfirst(sched_arr_time, na.rm = TRUE),
                                           last = glast(sched_arr_time, na.rm = TRUE),
                                           nobs = gnobs(arr_time)))
# Grouped
testthat::expect_equal(gres1,
                           flights %>%
                             dplyr::mutate(min = gmin(time_hour, g = g1),
                                           max = gmax(time_hour, g = g1),
                                           mean = gmean(time_hour, g = g1),
                                           sum = gsum(sched_arr_time, g = g1),
                                           sd = gsd(sched_arr_time, g = g1, na.rm = TRUE),
                                           var = gvar(sched_arr_time, g = g1, na.rm = TRUE),
                                           mode = gmode(sched_arr_time, g = g1, na.rm = TRUE),
                                           median = gmedian(sched_arr_time, g = g1, na.rm = TRUE),
                                           first = gfirst(sched_arr_time, g = g1, na.rm = TRUE),
                                           last = glast(sched_arr_time, g = g1, na.rm = TRUE),
                                           nobs = gnobs(arr_time, g = g1)))
testthat::expect_equal(gres1,
                           flights %>%
                             dplyr::mutate(min = gmin(time_hour, g = g2),
                                           max = gmax(time_hour, g = g2),
                                           mean = gmean(time_hour, g = g2),
                                           sum = gsum(sched_arr_time, g = g2),
                                           sd = gsd(sched_arr_time, g = g2, na.rm = TRUE),
                                           var = gvar(sched_arr_time, g = g2, na.rm = TRUE),
                                           mode = gmode(sched_arr_time, g = g2, na.rm = TRUE),
                                           median = gmedian(sched_arr_time, g = g2, na.rm = TRUE),
                                           first = gfirst(sched_arr_time, g = g2, na.rm = TRUE),
                                           last = glast(sched_arr_time, g = g2, na.rm = TRUE),
                                           nobs = gnobs(arr_time, g = g2)))
testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g3),
                                       max = gmax(time_hour, g = g3),
                                       mean = gmean(time_hour, g = g3),
                                       sum = gsum(sched_arr_time, g = g3),
                                       sd = gsd(sched_arr_time, g = g3, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g3, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g3, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g3, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g3, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g3, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g3)))
testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g4),
                                       max = gmax(time_hour, g = g4),
                                       mean = gmean(time_hour, g = g4),
                                       sum = gsum(sched_arr_time, g = g4),
                                       sd = gsd(sched_arr_time, g = g4, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g4, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g4, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g4, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g4, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g4, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g4)))
testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g5),
                                       max = gmax(time_hour, g = g5),
                                       mean = gmean(time_hour, g = g5),
                                       sum = gsum(sched_arr_time, g = g5),
                                       sd = gsd(sched_arr_time, g = g5, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g5, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g5, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g5, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g5, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g5, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g5)))
testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g6),
                                       max = gmax(time_hour, g = g6),
                                       mean = gmean(time_hour, g = g6),
                                       sum = gsum(sched_arr_time, g = g6),
                                       sd = gsd(sched_arr_time, g = g6, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g6, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g6, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g6, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g6, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g6, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g6)))
testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g7),
                                       max = gmax(time_hour, g = g7),
                                       mean = gmean(time_hour, g = g7),
                                       sum = gsum(sched_arr_time, g = g7),
                                       sd = gsd(sched_arr_time, g = g7, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g7, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g7, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g7, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g7, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g7, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g7)))
testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g8),
                                       max = gmax(time_hour, g = g8),
                                       mean = gmean(time_hour, g = g8),
                                       sum = gsum(sched_arr_time, g = g8),
                                       sd = gsd(sched_arr_time, g = g8, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g8, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g8, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g8, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g8, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g8, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g8)))

testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g9),
                                       max = gmax(time_hour, g = g9),
                                       mean = gmean(time_hour, g = g9),
                                       sum = gsum(sched_arr_time, g = g9),
                                       sd = gsd(sched_arr_time, g = g9, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g9, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g9, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g9, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g9, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g9, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g9)))

testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g10),
                                       max = gmax(time_hour, g = g10),
                                       mean = gmean(time_hour, g = g10),
                                       sum = gsum(sched_arr_time, g = g10),
                                       sd = gsd(sched_arr_time, g = g10, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g10, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g10, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g10, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g10, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g10, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g10)))
testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g11),
                                       max = gmax(time_hour, g = g11),
                                       mean = gmean(time_hour, g = g11),
                                       sum = gsum(sched_arr_time, g = g11),
                                       sd = gsd(sched_arr_time, g = g11, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g11, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g11, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g11, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g11, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g11, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g11)))
testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g12),
                                       max = gmax(time_hour, g = g12),
                                       mean = gmean(time_hour, g = g12),
                                       sum = gsum(sched_arr_time, g = g12),
                                       sd = gsd(sched_arr_time, g = g12, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g12, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g12, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g12, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g12, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g12, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g12)))
testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g13),
                                       max = gmax(time_hour, g = g13),
                                       mean = gmean(time_hour, g = g13),
                                       sum = gsum(sched_arr_time, g = g13),
                                       sd = gsd(sched_arr_time, g = g13, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g13, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g13, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g13, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g13, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g13, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g13)))
testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g14),
                                       max = gmax(time_hour, g = g14),
                                       mean = gmean(time_hour, g = g14),
                                       sum = gsum(sched_arr_time, g = g14),
                                       sd = gsd(sched_arr_time, g = g14, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g14, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g14, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g14, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g14, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g14, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g14)))
testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g15),
                                       max = gmax(time_hour, g = g15),
                                       mean = gmean(time_hour, g = g15),
                                       sum = gsum(sched_arr_time, g = g15),
                                       sd = gsd(sched_arr_time, g = g15, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g15, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g15, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g15, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g15, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g15, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g15)))
testthat::expect_equal(gres1,
                       flights %>%
                         dplyr::mutate(min = gmin(time_hour, g = g16),
                                       max = gmax(time_hour, g = g16),
                                       mean = gmean(time_hour, g = g16),
                                       sum = gsum(sched_arr_time, g = g16),
                                       sd = gsd(sched_arr_time, g = g16, na.rm = TRUE),
                                       var = gvar(sched_arr_time, g = g16, na.rm = TRUE),
                                       mode = gmode(sched_arr_time, g = g16, na.rm = TRUE),
                                       median = gmedian(sched_arr_time, g = g16, na.rm = TRUE),
                                       first = gfirst(sched_arr_time, g = g16, na.rm = TRUE),
                                       last = glast(sched_arr_time, g = g16, na.rm = TRUE),
                                       nobs = gnobs(arr_time, g = g16)))
})
