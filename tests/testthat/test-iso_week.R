# Set number of data.table threads to 2
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("Compare to lubridate", {
  x <- time_seq(
    lubridate::today(),
    lubridate::today() + lubridate::years(20),
    "day"
  )
  x2 <- time_seq(
    lubridate::now(),
    lubridate::now() + lubridate::years(20),
    "12 hours"
  )
  x <- sample(x)
  x2 <- sample(x2)
  iso_week2 <- function(x) {
    paste0(
      lubridate::isoyear(x), "-W",
      stringr::str_pad(lubridate::isoweek(x),
        width = 2, pad = "0",
        side = "left"
      )
    )
  }
  iso_week3 <- function(x) {
    paste0(
      lubridate::isoyear(x), "-W",
      stringr::str_pad(lubridate::isoweek(x),
        width = 2, pad = "0",
        side = "left"
      ), "-",
      lubridate::wday(x, week_start = 1)
    )
  }
  iso_week4 <- function(x) {
    paste0(
      "W", stringr::str_pad(lubridate::isoweek(x),
        width = 2, pad = "0",
        side = "left"
      ), "-",
      lubridate::wday(x, week_start = 1)
    )
  }
  iso_week5 <- function(x) {
    paste0("W", stringr::str_pad(lubridate::isoweek(x),
      width = 2, pad = "0",
      side = "left"
    ))
  }
  expect_identical(
    isoday(x),
    as.integer(lubridate::wday(x,
      week_start = 1
    ))
  )
  expect_identical(
    isoday(x2),
    as.integer(lubridate::wday(x2,
      week_start = 1
    ))
  )
  expect_identical(
    iso_week(x, day = FALSE),
    iso_week2(x)
  )
  expect_identical(
    iso_week(x, day = TRUE),
    iso_week3(x)
  )
  expect_identical(
    iso_week(x, year = FALSE, day = TRUE),
    iso_week4(x)
  )
  expect_identical(
    iso_week(x, year = FALSE, day = FALSE),
    iso_week5(x)
  )
})
