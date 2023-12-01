# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("fskim", {
  expect_snapshot(fskim(dplyr::slice(iris, 0)))
  expect_snapshot(fskim(dplyr::select(iris)))

testthat::expect_equal(iris %>%
  dplyr::select(Sepal.Length) %>%
  fskim() %>%
    .[["numeric"]],
  iris %>%
    dplyr::select(Sepal.Length) %>%
    dplyr::as_tibble() %>%
    dplyr::summarise(col = "Sepal.Length",
                     class = class(Sepal.Length),
                     n_missing = sum(is.na(Sepal.Length)),
                     p_complete = 1 - (n_missing / dplyr::n()),
                     n_unique = dplyr::n_distinct(Sepal.Length, na.rm = TRUE),
                     mean = mean(Sepal.Length, na.rm = TRUE),
                     p0 = min(Sepal.Length, na.rm = TRUE),
                     p25 = stats::quantile(Sepal.Length, probs = 0.25,
                                           na.rm = TRUE, names = FALSE),
                     p50 = stats::quantile(Sepal.Length, probs = 0.5,
                                           na.rm = TRUE, names = FALSE),
                     p75 = stats::quantile(Sepal.Length, probs = 0.75,
                                           na.rm = TRUE, names = FALSE),
                     p100 = stats::quantile(Sepal.Length, probs = 1,
                                           na.rm = TRUE, names = FALSE),
                     iqr = p75 - p25,
                     sd = stats::sd(Sepal.Length, na.rm = TRUE),
                     head = paste(utils::head(Sepal.Length, n = 3),
                                  collapse = ", "),
                     tail = paste(utils::tail(Sepal.Length, n = 3),
                                  collapse = ", ")))

skim_numeric <- function(x){
  fskim(x)$numeric
}

testthat::expect_equal(
  {
    iris %>%
      dplyr::select(Species, Sepal.Length) %>%
      dplyr::group_by(Species) %>%
      dplyr::do(skim_numeric(.)) %>%
      dplyr::ungroup()
  },
  {
    iris %>%
      dplyr::group_by(Species) %>%
      dplyr::summarise(col = "Sepal.Length",
                       class = class(Sepal.Length),
                       n_missing = sum(is.na(Sepal.Length)),
                       p_complete = 1 - (n_missing / dplyr::n()),
                       n_unique = dplyr::n_distinct(Sepal.Length, na.rm = TRUE),
                       mean = mean(Sepal.Length, na.rm = TRUE),
                       p0 = min(Sepal.Length, na.rm = TRUE),
                       p25 = stats::quantile(Sepal.Length, probs = 0.25,
                                             na.rm = TRUE, names = FALSE),
                       p50 = stats::quantile(Sepal.Length, probs = 0.5,
                                             na.rm = TRUE, names = FALSE),
                       p75 = stats::quantile(Sepal.Length, probs = 0.75,
                                             na.rm = TRUE, names = FALSE),
                       p100 = stats::quantile(Sepal.Length, probs = 1,
                                              na.rm = TRUE, names = FALSE),
                       iqr = p75 - p25,
                       sd = stats::sd(Sepal.Length, na.rm = TRUE),
                       head = paste(utils::head(Sepal.Length, n = 3),
                                    collapse = ", "),
                       tail = paste(utils::tail(Sepal.Length, n = 3),
                                    collapse = ", "))
  }
)
})
