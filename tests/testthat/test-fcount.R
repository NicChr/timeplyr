
testthat::test_that("Compare to dplyr", {
  set.seed(42)
  weights <- sample(1:150)
  iris[["weight"]] <- weights
  flights <- nycflights13::flights
  testthat::expect_identical(iris %>%
                               dplyr::mutate(interval = lubridate::interval(
                                 lubridate::today(), lubridate::today())) %>%
                               fcount(),
                             iris %>%
                               dplyr::mutate(interval = lubridate::interval(
                                 lubridate::today(), lubridate::today())) %>%
                               dplyr::count())
  testthat::expect_identical(iris %>% dplyr::count(),
                             iris %>% fcount())
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(across(all_of("Species"))),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fcount())
                               # fcount(across(all_of("Species"))))
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(across(all_of(c("Species", "Sepal.Length")))),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fcount(across(all_of(c("Sepal.Length")))))
                               # fcount(across(all_of(c("Species", "Sepal.Length")))))
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(Species),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fcount(Species))
  testthat::expect_identical(iris %>% dplyr::count(NULL),
                             iris %>% fcount(NULL))
  testthat::expect_identical(iris %>% dplyr::slice(0) %>% dplyr::count(),
                             iris %>% dplyr::slice(0) %>% fcount())
  testthat::expect_identical(iris %>% dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>% dplyr::count(),
                             iris %>% dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>% fcount())
  testthat::expect_identical(iris %>%
                               dplyr::slice(0) %>% dplyr::count(Species),
                             iris %>%
                               dplyr::slice(0) %>% fcount(Species))
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>%
                               dplyr::count(Species),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>%
                               fcount(Species))
  testthat::expect_identical(iris %>% dplyr::count(NA),
                             iris %>% fcount(NA))
  testthat::expect_identical(iris %>% dplyr::count(across(dplyr::everything())),
                             iris %>% fcount(across(dplyr::everything())))
  testthat::expect_identical(iris %>% dplyr::count(across(dplyr::everything()),
                                                   wt = weight),
                             iris %>% fcount(across(dplyr::everything()),
                                             wt = weight) %>%
                               dplyr::mutate(n = as.integer(n)))
  testthat::expect_identical(iris %>% dplyr::count(),
                             iris %>% fcount())
  testthat::expect_identical(iris %>% dplyr::count(name = ".count"),
                             iris %>% fcount(name = ".count"))
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(Sepal.Length) %>%
                               dplyr::count(n, name = "n"),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fcount(Sepal.Length) %>%
                               fcount(n, name = "n"))
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(),
                             iris %>% dplyr::group_by(Species) %>% fcount())
  testthat::expect_identical(iris %>% dplyr::group_by(across(everything())) %>% dplyr::count(),
                             iris %>% dplyr::group_by(across(everything())) %>% fcount())
  testthat::expect_identical(iris %>% dplyr::group_by(across(everything())) %>% dplyr::count(Species),
                             iris %>% dplyr::group_by(across(everything())) %>% fcount(Species))
  testthat::expect_identical(iris %>% dplyr::group_by(Species) %>% dplyr::count(Species),
                             iris %>% dplyr::group_by(Species) %>% fcount(Species))
  testthat::expect_identical(iris %>% dplyr::group_by(Species) %>%
                               dplyr::count(across(all_of(c("Sepal.Length")))),
                             iris %>% dplyr::group_by(Species) %>%
                               fcount(across(all_of(c("Sepal.Length")))))
  testthat::expect_identical(iris %>% dplyr::group_by(Species) %>%
                               dplyr::count(across(dplyr::any_of(c("Species", "Sepal.Length")))),
                             iris %>% dplyr::group_by(Species) %>%
                               fcount(across(dplyr::any_of(c("Species", "Sepal.Length")))))
  testthat::expect_identical(iris %>%
                               dplyr::count(as.character(Species)),
                             iris %>%
                               fcount(as.character(Species)))
  testthat::expect_identical(flights %>% dplyr::count(tailnum, origin, dest),
                             flights %>% fcount(tailnum, origin, dest))
  testthat::expect_identical(flights %>% dplyr::count(tailnum, origin, dest, sort = TRUE),
                             flights %>% fcount(tailnum, origin, dest, sort = TRUE))

})

testthat::test_that("Compare to dplyr, add_count", {
  set.seed(42)
  weights <- sample(1:150)
  iris[["weight"]] <- weights
  flights <- nycflights13::flights
  testthat::expect_identical(iris %>%
                               dplyr::mutate(interval = lubridate::interval(
                                 lubridate::today(), lubridate::today())) %>%
                               fadd_count(keep_class = TRUE),
                             iris %>%
                               dplyr::mutate(interval = lubridate::interval(
                                 lubridate::today(), lubridate::today())) %>%
                               dplyr::add_count())
  testthat::expect_identical(iris %>% fadd_count(keep_class = FALSE),
                             iris %>%
                               data.table::as.data.table() %>%
                               dplyr::mutate(n = dplyr::n()))
  testthat::expect_identical(iris %>% dplyr::add_count(),
                             iris %>% fadd_count())
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(across(all_of("Species"))),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fadd_count())
  # fadd_count(across(all_of("Species"))))
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(across(all_of(c("Species", "Sepal.Length")))),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fadd_count(across(all_of(c("Sepal.Length")))))
  # fadd_count(across(all_of(c("Species", "Sepal.Length")))))
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(Species),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fadd_count(Species))
  testthat::expect_identical(iris %>% dplyr::add_count(NULL),
                             iris %>% fadd_count(NULL))
  testthat::expect_identical(iris %>% dplyr::slice(0) %>% dplyr::add_count(),
                             iris %>% dplyr::slice(0) %>% fadd_count())
  testthat::expect_identical(iris %>% dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>% dplyr::add_count(),
                             iris %>% dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>% fadd_count())
  testthat::expect_identical(iris %>%
                               dplyr::slice(0) %>% dplyr::add_count(Species),
                             iris %>%
                               dplyr::slice(0) %>% fadd_count(Species))
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>%
                               dplyr::add_count(Species),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>%
                               fadd_count(Species))
  testthat::expect_identical(iris %>% dplyr::add_count(NA),
                             iris %>% fadd_count(NA))
  testthat::expect_identical(iris %>% dplyr::add_count(across(dplyr::everything())),
                             iris %>% fadd_count(across(dplyr::everything())))
  testthat::expect_identical(iris %>% dplyr::add_count(across(dplyr::everything()),
                                                   wt = weight),
                             iris %>% fadd_count(across(dplyr::everything()),
                                             wt = weight) %>%
                               dplyr::mutate(n = as.integer(n)))
  testthat::expect_identical(iris %>% dplyr::add_count(),
                             iris %>% fadd_count())
  testthat::expect_identical(iris %>% dplyr::add_count(name = ".count"),
                             iris %>% fadd_count(name = ".count"))
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(Sepal.Length) %>%
                               dplyr::add_count(n, name = "n"),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               fadd_count(Sepal.Length) %>%
                               fadd_count(n, name = "n"))
  testthat::expect_identical(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(),
                             iris %>% dplyr::group_by(Species) %>% fadd_count())
  testthat::expect_identical(iris %>% dplyr::group_by(across(everything())) %>% dplyr::add_count(),
                             iris %>% dplyr::group_by(across(everything())) %>% fadd_count())
  testthat::expect_identical(iris %>% dplyr::group_by(across(everything())) %>% dplyr::add_count(Species),
                             iris %>% dplyr::group_by(across(everything())) %>% fadd_count(Species))
  testthat::expect_identical(iris %>% dplyr::group_by(Species) %>% dplyr::add_count(Species),
                             iris %>% dplyr::group_by(Species) %>% fadd_count(Species))
  testthat::expect_identical(iris %>% dplyr::group_by(Species) %>%
                               dplyr::add_count(across(all_of(c("Sepal.Length")))),
                             iris %>% dplyr::group_by(Species) %>%
                               fadd_count(across(all_of(c("Sepal.Length")))))
  testthat::expect_identical(iris %>% dplyr::group_by(Species) %>%
                               dplyr::add_count(across(dplyr::any_of(c("Species", "Sepal.Length")))),
                             iris %>% dplyr::group_by(Species) %>%
                               fadd_count(across(dplyr::any_of(c("Species", "Sepal.Length")))))
  testthat::expect_identical(iris %>%
                               dplyr::add_count(as.character(Species)),
                             iris %>%
                               fadd_count(as.character(Species)))
  testthat::expect_identical(flights %>% dplyr::add_count(tailnum, origin, dest),
                             flights %>% fadd_count(tailnum, origin, dest))
  testthat::expect_identical(flights %>% dplyr::add_count(tailnum, origin, dest, sort = TRUE),
                             flights %>% fadd_count(tailnum, origin, dest, sort = TRUE))

})

