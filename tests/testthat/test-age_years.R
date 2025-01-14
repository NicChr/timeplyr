# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("Expect error", {
  expect_error(age_years(Inf))
  expect_error(age_years(-Inf))
  expect_error(age_years(NaN))
  expect_error(age_years(NULL))
  expect_error(age_years(Sys.Date(), end = "01-01-2000"))
})

test_that("Expect zero length vector", {
  empty_values <- list(
    # as.POSIXlt(numeric(0)),
    lubridate::Date(0),
    lubridate::POSIXct(0)
  )
  expect_identical(
    lapply(empty_values, function(x) age_years(x)),
    list(integer(0), integer(0))
  )
})

test_that("Expect NA", {
  expect_identical(
    age_years(lubridate::NA_Date_),
    NA_integer_
  )
  expect_identical(
    age_years(lubridate::NA_POSIXct_),
    NA_integer_
  )
  # expect_identical(age_years(as.POSIXlt(NA)),
  #                            NA_integer_)
})

test_that("Leap year", {
  leap1 <- lubridate::dmy("29-02-2020")
  leap2 <- lubridate::dmy("28-02-2021")
  leap3 <- lubridate::dmy("01-03-2021")
  expect_identical(age_years(leap1, leap2), 0L)
  expect_identical(age_years(leap1, leap3), 1L)
})
