testthat::test_that("unit_guess", {
  # Single units
  testthat::expect_equal(unit_guess("days"),
                         list("unit" = "days",
                              "num" = 1,
                              "scale" = 1))
  testthat::expect_equal(unit_guess("hours"),
                         list("unit" = "hours",
                              "num" = 1,
                              "scale" = 1))

  # Multi-units
  testthat::expect_equal(unit_guess("7 days"),
                         list("unit" = "days",
                              "num" = 7,
                              "scale" = 1))
  testthat::expect_equal(unit_guess("0.5 hours"),
                         list("unit" = "hours",
                              "num" = 0.5,
                              "scale" = 1))
  # Negative units
  testthat::expect_equal(unit_guess("-7 days"),
                         list("unit" = "days",
                              "num" = -7,
                              "scale" = 1))
  testthat::expect_equal(unit_guess("-.12 fortnights"),
                         list("unit" = "weeks",
                              "num" = -0.12,
                              "scale" = 2))

  # Basic math works too
  # testthat::expect_equal(unit_guess("10*10 days"),
  #                        list("unit" = "days",
  #                             "num" = 100,
  #                             "scale" = 1))
  # testthat::expect_equal(unit_guess("10/10 days"),
  #                        list("unit" = "days",
  #                             "num" = 1,
  #                             "scale" = 1))

  # Exotic units
  testthat::expect_equal(unit_guess("fortnights"),
                         list("unit" = "weeks",
                              "num" = 1,
                              "scale" = 2))
  testthat::expect_equal(unit_guess("decades"),
                         list("unit" = "years",
                              "num" = 1,
                              "scale" = 10))

  # list input is accepted
  testthat::expect_equal(unit_guess(list("months" = 12)),
                         list("unit" = "months",
                              "num" = 12,
                              "scale" = 1))
  # With a list, a vector of numbers is accepted
  testthat::expect_equal(unit_guess(list("months" = 1:10)),
                         list("unit" = "months",
                              "num" = 1:10,
                              "scale" = 1))
  testthat::expect_equal(unit_guess(list("days" = -10:10 %% 7)),
                         list("unit" = "days",
                              "num" = -10:10 %% 7,
                              "scale" = 1))

  # Numbers also accepted
  testthat::expect_equal(unit_guess(100),
                         list("unit" = "numeric",
                              "num" = 100,
                              "scale" = 1))
  testthat::expect_equal(unit_guess(-100:100),
                         list("unit" = "numeric",
                              "num" = -100:100,
                              "scale" = 1))
})
