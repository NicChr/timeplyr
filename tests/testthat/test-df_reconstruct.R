# Set number of data.table threads to 1
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("Compare to dplyr", {
  # All credit goes to dplyr and its Authors/maintainers for this amazing function
  # Source: https://github.com/tidyverse/dplyr
  # This function is only used as a means of comparison with df_reconstruct for unit testing purposes
  # It is almost the same, except that when the template is a data.table,
  # attributes are mostly destroyed
  dplyr_reconstruct <- utils::getFromNamespace("dplyr_reconstruct", "dplyr")
  flights <- nycflights13::flights
  testthat::expect_identical(df_reconstruct(iris, iris),
                             dplyr_reconstruct(iris, iris))
  testthat::expect_identical(df_reconstruct(iris, dplyr::as_tibble(iris)),
                             dplyr_reconstruct(iris, dplyr::as_tibble(iris)))
  testthat::expect_identical(df_reconstruct(iris, data.table::as.data.table(iris)),
                             dplyr_reconstruct(iris, data.table::as.data.table(iris)))
  testthat::expect_identical(df_reconstruct(iris, iris %>% dplyr::group_by(Species)),
                             dplyr_reconstruct(iris, iris %>% dplyr::group_by(Species)))
  testthat::expect_identical(df_reconstruct(iris %>% dplyr::group_by(Species), iris %>% dplyr::group_by(Species)),
                             dplyr_reconstruct(iris %>% dplyr::group_by(Species), iris %>% dplyr::group_by(Species)))

  attributes(attributes(dplyr_reconstruct(iris, iris %>% dplyr::group_by(Species)))$groups)
  attributes(attributes(df_reconstruct(iris, iris %>% dplyr::group_by(Species)))$groups)

  testthat::expect_identical(df_reconstruct(iris, flights),
                             dplyr_reconstruct(iris, flights))
  testthat::expect_identical(df_reconstruct(dplyr::bind_rows(flights, flights), flights),
                             dplyr_reconstruct(dplyr::bind_rows(flights, flights), flights))
  testthat::expect_identical(df_reconstruct(dplyr::bind_rows(flights, flights), dplyr::group_by(flights, origin, dest)),
                             dplyr_reconstruct(dplyr::bind_rows(flights, flights), dplyr::group_by(flights, origin, dest)))
  testthat::expect_identical(df_reconstruct(iris, flights %>% dplyr::group_by(origin, dest)),
                             dplyr_reconstruct(iris, flights %>% dplyr::group_by(origin, dest)))
  testthat::expect_identical(df_reconstruct(iris %>% dplyr::group_by(Species), flights %>% dplyr::group_by(origin, dest)),
                             dplyr_reconstruct(iris %>% dplyr::group_by(Species), flights %>% dplyr::group_by(origin, dest)))
  testthat::expect_identical(df_reconstruct(flights %>% dplyr::group_by(origin), flights %>% dplyr::group_by(origin, dest)),
                             dplyr_reconstruct(flights %>% dplyr::group_by(origin), flights %>% dplyr::group_by(origin, dest)))
  testthat::expect_identical(df_reconstruct(flights %>% dplyr::group_by(origin, dest), flights %>% dplyr::group_by(origin)),
                             dplyr_reconstruct(flights %>% dplyr::group_by(origin, dest), flights %>% dplyr::group_by(origin)))
  testthat::expect_identical(df_reconstruct(flights %>% dplyr::group_by(origin, dest), flights %>% dplyr::group_by(tailnum)),
                             dplyr_reconstruct(flights %>% dplyr::group_by(origin, dest), flights %>% dplyr::group_by(tailnum)))
  testthat::expect_identical(df_reconstruct(flights %>% dplyr::group_by(origin, dest), data.table::as.data.table(flights)),
                             dplyr_reconstruct(flights %>% dplyr::group_by(origin, dest), data.table::as.data.table(flights)))
  # testthat::expect_identical(df_reconstruct(flights %>% dplyr::group_by(origin, dest),
  #                                           data.table::as.data.table(dplyr::group_by(flights, origin, tailnum))),
  #                            dplyr_reconstruct(flights %>% dplyr::group_by(origin, dest),
  #                                              data.table::as.data.table(dplyr::group_by(flights, origin, tailnum))))
  testthat::expect_identical(df_reconstruct(data.table::as.data.table(flights), flights),
                             dplyr_reconstruct(data.table::as.data.table(flights), flights))
  testthat::expect_identical(df_reconstruct(data.table::as.data.table(dplyr::group_by(flights, origin, tailnum)),
                                            flights %>% dplyr::group_by(origin, dest)),
                             dplyr_reconstruct(data.table::as.data.table(dplyr::group_by(flights, origin, tailnum)),
                                               flights %>% dplyr::group_by(origin, dest)))
})

