# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("fdistinct", {
  flights <- nycflights13::flights
  expect_equal(
    fdistinct(fselect(iris, .cols = 0)),
    structure(list(), names = character(0),
              class = "data.frame",
              row.names = c(NA, -1L))
  )
  expect_equal(
    fdistinct(fselect(iris, .cols = 0), sort = TRUE),
    structure(list(), names = character(0),
              class = "data.frame",
              row.names = c(NA, -1L))
  )
  expect_equal(
    fdistinct(fselect(iris, .cols = 0), sort = FALSE, order = TRUE),
    structure(list(), names = character(0),
              class = "data.frame",
              row.names = c(NA, -1L))
  )

  testthat::expect_equal(fdistinct(flights, .cols = 0),
                         dplyr::distinct(flights,
                                         dplyr::across(dplyr::all_of(character(0)))))
  testthat::expect_equal(fdistinct(flights, dplyr::across(dplyr::all_of(character(0)))),
                         dplyr::distinct(flights,
                                         dplyr::across(dplyr::all_of(character(0)))))
  testthat::expect_identical(flights %>%
                               dplyr::distinct(),
                             flights %>%
                               fdistinct())
  testthat::expect_identical(flights %>%
                               dplyr::distinct(carrier, origin, tailnum, dest),
                             flights %>%
                               fdistinct(carrier, origin, tailnum, dest))
  testthat::expect_identical(flights %>%
                               dplyr::group_by(flight) %>%
                               dplyr::distinct(carrier, origin, tailnum, dest),
                             flights %>%
                               dplyr::group_by(flight) %>%
                               fdistinct(carrier, origin, tailnum, dest))
  testthat::expect_identical(flights %>%
                               dplyr::distinct(carrier, origin, tailnum, dest,
                                               .keep_all = TRUE),
                             flights %>%
                               fdistinct(carrier, origin, tailnum, dest,
                                         .keep_all = TRUE))
  testthat::expect_identical(flights %>%
                               dplyr::group_by(flight) %>%
                               dplyr::distinct(carrier, origin, tailnum, dest,
                                               .keep_all = TRUE),
                             flights %>%
                               dplyr::group_by(flight) %>%
                               fdistinct(carrier, origin, tailnum, dest,
                                         .keep_all = TRUE))

  testthat::expect_identical(flights %>%
                               dplyr::slice(0) %>%
                               dplyr::distinct(),
                             flights %>%
                               dplyr::slice(0) %>%
                               fdistinct())
  testthat::expect_identical(flights %>%
                               dplyr::group_by(dest, origin, tailnum) %>%
                               fslice(0) %>%
                               dplyr::distinct(),
                             flights %>%
                               dplyr::group_by(dest, origin, tailnum) %>%
                               fslice(0) %>%
                               fdistinct())
  testthat::expect_identical(flights %>%
                               dplyr::slice(0) %>%
                               dplyr::distinct(),
                             flights %>%
                               dplyr::slice(0) %>%
                               fdistinct())
  testthat::expect_identical(flights %>%
                               dplyr::group_by(dest, origin, tailnum) %>%
                               fslice(0) %>%
                               dplyr::distinct(.keep_all = TRUE),
                             flights %>%
                               dplyr::group_by(dest, origin, tailnum) %>%
                               fslice(0) %>%
                               fdistinct(.keep_all = TRUE))
})
