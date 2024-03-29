# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Compare to dplyr", {
  set.seed(42)
  weights <- sample(1:150)
  iris[["weight"]] <- weights
  flights <- nycflights13::flights
  testthat::expect_equal(iris %>%
                               dplyr::mutate(interval = lubridate::interval(
                                 lubridate::today(), lubridate::today())) %>%
                               fcount(),
                             iris %>%
                               dplyr::mutate(interval = lubridate::interval(
                                 lubridate::today(), lubridate::today())) %>%
                               dplyr::count())
  testthat::expect_equal(iris %>% dplyr::count(),
                             iris %>% fcount())
  testthat::expect_identical(
    iris %>%
      data.table::as.data.table() %>%
      fcount(),
    data.table::data.table(n = 150L)
  )
  testthat::expect_identical(
    iris %>%
      data.table::as.data.table() %>%
      dplyr::slice(0L) %>%
      fcount(),
    data.table::data.table(n = 0L)
  )
  # Unused factor levels

  testthat::expect_equal(iris %>%
                           dplyr::slice(2, 128, 125) %>%
                           dplyr::group_by(Species, .drop = FALSE) %>%
                           fcount(),
                         iris %>%
                           dplyr::slice(2, 128, 125) %>%
                           dplyr::group_by(Species, .drop = FALSE) %>%
                           dplyr::count())
  testthat::expect_equal(iris %>%
                           dplyr::slice(2, 128, 125) %>%
                           dplyr::group_by(Species, .drop = TRUE) %>%
                           fcount(),
                         iris %>%
                           dplyr::slice(2, 128, 125) %>%
                           dplyr::group_by(Species, .drop = TRUE) %>%
                           dplyr::count())
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(across(all_of("Species"))),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fcount())
                               # fcount(across(all_of("Species"))))
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(across(all_of(c("Species", "Sepal.Length")))),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fcount(across(all_of(c("Sepal.Length")))))
                               # fcount(across(all_of(c("Species", "Sepal.Length")))))
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(Species),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fcount(Species))
  testthat::expect_equal(iris %>% dplyr::count(NULL),
                             iris %>% fcount(NULL))
  testthat::expect_equal(iris %>% dplyr::slice(0) %>% dplyr::count(),
                             iris %>% dplyr::slice(0) %>% fcount())
  testthat::expect_equal(iris %>% dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>% dplyr::count(),
                             iris %>% dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>% fcount())
  testthat::expect_equal(iris %>%
                               dplyr::slice(0) %>% dplyr::count(Species),
                             iris %>%
                               dplyr::slice(0) %>% fcount(Species))
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>%
                               dplyr::count(Species),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>%
                               fcount(Species))
  testthat::expect_equal(iris %>% dplyr::count(NA),
                             iris %>% fcount(NA))
  testthat::expect_equal(iris %>% dplyr::count(across(dplyr::everything())),
                             iris %>% fcount(across(dplyr::everything())))
  testthat::expect_equal(iris %>% dplyr::count(across(dplyr::everything()),
                                                   wt = weight),
                             iris %>% fcount(across(dplyr::everything()),
                                             wt = weight) %>%
                               dplyr::mutate(n = as.integer(n)))
  testthat::expect_equal(iris %>% dplyr::count(),
                             iris %>% fcount())
  testthat::expect_equal(iris %>% dplyr::count(name = ".count"),
                             iris %>% fcount(name = ".count"))
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(Sepal.Length) %>%
                               dplyr::count(n, name = "n"),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fcount(Sepal.Length) %>%
                               fcount(n, name = "n"))
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(),
                             iris %>% dplyr::group_by(Species) %>% fcount())
  testthat::expect_equal(iris %>% dplyr::group_by(across(everything())) %>% dplyr::count(),
                             iris %>% dplyr::group_by(across(everything())) %>% fcount())
  testthat::expect_equal(iris %>% dplyr::group_by(across(everything())) %>% dplyr::count(Species),
                             iris %>% dplyr::group_by(across(everything())) %>% fcount(Species))
  testthat::expect_equal(iris %>% dplyr::group_by(Species) %>% dplyr::count(Species),
                             iris %>% dplyr::group_by(Species) %>% fcount(Species))
  testthat::expect_equal(iris %>% dplyr::group_by(Species) %>%
                               dplyr::count(across(all_of(c("Sepal.Length")))),
                             iris %>% dplyr::group_by(Species) %>%
                               fcount(across(all_of(c("Sepal.Length")))))
  testthat::expect_equal(iris %>% dplyr::group_by(Species) %>%
                               dplyr::count(across(dplyr::any_of(c("Species", "Sepal.Length")))),
                             iris %>% dplyr::group_by(Species) %>%
                               fcount(across(dplyr::any_of(c("Species", "Sepal.Length")))))
  testthat::expect_equal(iris %>%
                               dplyr::count(as.character(Species)),
                             iris %>%
                               fcount(as.character(Species)))
  testthat::expect_equal(flights %>% dplyr::count(tailnum, origin, dest),
                             flights %>% fcount(tailnum, origin, dest))
  testthat::expect_equal(flights %>% dplyr::count(tailnum, origin, dest, sort = TRUE),
                             flights %>% fcount(tailnum, origin, dest, sort = TRUE))

  # With weights
  res1 <- flights %>%
    fcount(origin, dest)
  set.seed(812123123)
  wt1 <- rep_len(3L, nrow(res1))
  wt2 <- sample2(1:10, size = nrow(res1), replace = TRUE)

  res1 <- res1 %>%
    dplyr::mutate(wt2)
  testthat::expect_equal(res1 %>%
                               dplyr::count(origin, dest, wt = wt2),
                             res1 %>%
                               fcount(origin, dest, wt = wt2))
  testthat::expect_equal(res1 %>%
                               dplyr::mutate(wt1) %>%
                               dplyr::count(origin, dest, wt = wt1),
                             res1 %>%
                               fcount(origin, dest, wt = wt1))
  testthat::expect_equal(res1 %>%
                               dplyr::count(origin, dest, n, wt = wt2),
                             res1 %>%
                               fcount(origin, dest, n, wt = wt2))
  testthat::expect_equal(res1 %>%
                               dplyr::count(origin, dest, wt = n),
                             res1 %>%
                               fcount(origin, dest, wt = n))

  # Overwriting existing groups
  testthat::expect_equal(iris %>%
                           dplyr::group_by(Species) %>%
                           dplyr::count(Species = Sepal.Length),
                         iris %>%
                           dplyr::group_by(Species) %>%
                           fcount(Species = Sepal.Length))
})

testthat::test_that("Compare to dplyr, add_count", {
  set.seed(42)
  weights <- sample(1:150)
  iris[["weight"]] <- weights
  flights <- nycflights13::flights
  testthat::expect_equal(iris %>%
                               dplyr::mutate(interval = lubridate::interval(
                                 lubridate::today(), lubridate::today())) %>%
                               fadd_count(),
                             iris %>%
                               dplyr::mutate(interval = lubridate::interval(
                                 lubridate::today(), lubridate::today())) %>%
                               dplyr::add_count())
  testthat::expect_equal(iris %>% fadd_count(),
                             iris %>%
                               # data.table::as.data.table() %>%
                               dplyr::mutate(n = dplyr::n()))
  testthat::expect_equal(iris %>% dplyr::add_count(),
                             iris %>% fadd_count())
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(across(all_of("Species"))),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fadd_count())
  # fadd_count(across(all_of("Species"))))
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(across(all_of(c("Species", "Sepal.Length")))),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fadd_count(across(all_of(c("Sepal.Length")))))
  # fadd_count(across(all_of(c("Species", "Sepal.Length")))))
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(Species),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fadd_count(Species))
  testthat::expect_equal(iris %>% dplyr::add_count(NULL),
                             iris %>% fadd_count(NULL))
  testthat::expect_equal(iris %>% dplyr::slice(0) %>% dplyr::add_count(),
                             iris %>% dplyr::slice(0) %>% fadd_count())
  testthat::expect_equal(iris %>% dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>% dplyr::add_count(),
                             iris %>% dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>% fadd_count())
  testthat::expect_equal(iris %>%
                               dplyr::slice(0) %>% dplyr::add_count(Species),
                             iris %>%
                               dplyr::slice(0) %>% fadd_count(Species))
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>%
                               dplyr::add_count(Species),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>%
                               fadd_count(Species))
  testthat::expect_equal(iris %>% dplyr::add_count(NA),
                             iris %>% fadd_count(NA))
  testthat::expect_equal(iris %>% dplyr::add_count(across(dplyr::everything())),
                             iris %>% fadd_count(across(dplyr::everything())))
  testthat::expect_equal(iris %>% dplyr::add_count(across(dplyr::everything()),
                                                   wt = weight),
                             iris %>% fadd_count(across(dplyr::everything()),
                                             wt = weight) %>%
                               dplyr::mutate(n = as.integer(n)))
  testthat::expect_equal(iris %>% dplyr::add_count(),
                             iris %>% fadd_count())
  testthat::expect_equal(iris %>% dplyr::add_count(name = ".count"),
                             iris %>% fadd_count(name = ".count"))
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(Sepal.Length) %>%
                               dplyr::add_count(n, name = "n"),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fadd_count(Sepal.Length) %>%
                               fadd_count(n, name = "n"))
  testthat::expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(),
                             iris %>% dplyr::group_by(Species) %>% fadd_count())
  testthat::expect_equal(iris %>% dplyr::group_by(across(everything())) %>% dplyr::add_count(),
                             iris %>% dplyr::group_by(across(everything())) %>% fadd_count())
  testthat::expect_equal(iris %>% dplyr::group_by(across(everything())) %>% dplyr::add_count(Species),
                             iris %>% dplyr::group_by(across(everything())) %>% fadd_count(Species))
  testthat::expect_equal(iris %>% dplyr::group_by(Species) %>% dplyr::add_count(Species),
                             iris %>% dplyr::group_by(Species) %>% fadd_count(Species))
  testthat::expect_equal(iris %>% dplyr::group_by(Species) %>%
                               dplyr::add_count(across(all_of(c("Sepal.Length")))),
                             iris %>% dplyr::group_by(Species) %>%
                               fadd_count(across(all_of(c("Sepal.Length")))))
  testthat::expect_equal(iris %>% dplyr::group_by(Species) %>%
                               dplyr::add_count(across(dplyr::any_of(c("Species", "Sepal.Length")))),
                             iris %>% dplyr::group_by(Species) %>%
                               fadd_count(across(dplyr::any_of(c("Species", "Sepal.Length")))))
  testthat::expect_equal(iris %>%
                               dplyr::add_count(as.character(Species)),
                             iris %>%
                               fadd_count(as.character(Species)))
  testthat::expect_equal(flights %>% dplyr::add_count(tailnum, origin, dest),
                             flights %>% fadd_count(tailnum, origin, dest))
  testthat::expect_equal(flights %>% dplyr::add_count(tailnum, origin, dest, sort = TRUE),
                             flights %>% fadd_count(tailnum, origin, dest, sort = TRUE))

  # With weights
  res1 <- flights %>%
    fcount(origin, dest)
  set.seed(812123123)
  wt1 <- rep_len(3L, nrow(res1))
  wt2 <- sample2(1:10, size = nrow(res1), replace = TRUE)

  res1 <- res1 %>%
    dplyr::mutate(wt2)
  testthat::expect_equal(res1 %>%
                               dplyr::add_count(origin, dest, wt = wt2),
                             res1 %>%
                               fadd_count(origin, dest, wt = wt2))
  testthat::expect_equal(res1 %>%
                               dplyr::mutate(wt1) %>%
                               dplyr::add_count(origin, dest, wt = wt1),
                             res1 %>%
                               dplyr::mutate(wt1) %>%
                               fadd_count(origin, dest, wt = wt1))
  testthat::expect_equal(res1 %>%
                               dplyr::add_count(origin, dest, n, wt = wt2),
                             res1 %>%
                               fadd_count(origin, dest, n, wt = wt2))
  testthat::expect_equal(res1 %>%
                               dplyr::add_count(origin, dest, wt = n),
                             res1 %>%
                               fadd_count(origin, dest, wt = n))
  # Overwriting existing groups
  testthat::expect_equal(iris %>%
                           dplyr::group_by(Species) %>%
                           dplyr::add_count(Species = Sepal.Length),
                         iris %>%
                           dplyr::group_by(Species) %>%
                           fadd_count(Species = Sepal.Length))
})

