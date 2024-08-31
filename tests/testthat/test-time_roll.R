# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("time_roll", {
  max_print <- getOption("max.print", 1000L)
  options(max.print = 5)
  smr <- function(x){
    x <- x[!is.na(x)]
    c(var(x), mean(x), sd(x), stats::quantile(x, names = FALSE,
                                              probs = seq(0, 1, 0.1)))
  }
    flights <- nycflights13::flights
    x <- flights$arr_delay
    g <- flights$dest
    t <- seq_along(x)
    mu1 <- time_roll_mean(x, g = g, window = 5,
                   time = t,
                   close_left_boundary = TRUE,
                   partial = FALSE,
                   na.rm = TRUE)
    expect_snapshot(smr(mu1))
    mu2 <- time_roll_mean(x, g = g, window = 5,
                   time = frowid(g),
                   close_left_boundary = TRUE,
                   partial = FALSE,
                   na.rm = TRUE)
    expect_snapshot(smr(mu2))
    set.seed(420)
    t <- time_seq_v2(100, Sys.Date(), time_by = lubridate::days(1))

    t <- sample(t, size = 10^3, replace = TRUE)

    x <- rnorm(length(t))

    t <- sort(t)


    # With dups ---------------------------------------------------------------


    z1 <- time_roll_mean(x, time = t, days(11), close_left_boundary = TRUE)
    z3 <- time_roll_mean(x, time = t, days(11), close_left_boundary = FALSE)
    expect_snapshot(smr(z1 - z3))

    # With NA -----------------------------------------------------------------


    x <- na_fill(x, prop = 0.3)
    z1 <- time_roll_mean(x, time = t, days(11), close_left_boundary = TRUE)
    expect_snapshot(smr(z1))
    # With gaps ---------------------------------------------------------------

    t <- sort(sample(t, size = 20, FALSE))
    x <- rnorm(length(t))
    z1 <- time_roll_mean(x, time = t, days(11), close_left_boundary = TRUE)
    expect_snapshot(smr(z1))

    # With gaps and dups ------------------------------------------------------

    t <- time_seq_v2(100, Sys.Date(), time_by = lubridate::days(1))
    t <- sort(sample(t, size = 30, TRUE))
    x <- rnorm(length(t))
    z1 <- time_roll_mean(x, time = t, days(11), close_left_boundary = TRUE)
    expect_snapshot(smr(z1))
    # By group

    flights2 <- nycflights13::flights %>%
      fastplyr::f_arrange(time_hour)

    t <- flights2$time_hour
    x <- rnorm(length(t))
    z2 <- time_roll_mean(x, lubridate::dhours(2), time = t, partial = FALSE, close_left_boundary = TRUE)
    expect_snapshot(smr(z2))

    z2 <- flights2 %>%
      fastplyr::add_group_id(origin, dest) %>%
      dplyr::mutate(mean = time_roll_mean(arr_delay, time = time_hour,
                                          lubridate::dhours(2.5),
                                          close_left_boundary = TRUE,
                                          g = group_id)) %>%
      dplyr::pull(mean)
    expect_snapshot(smr(z2))
    options(max.print = max_print)
})
