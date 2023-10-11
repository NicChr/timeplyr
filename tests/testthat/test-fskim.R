# Set number of data.table threads to 1
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("fskim", {
  testthat::expect_equal(fskim(dplyr::slice(iris, 0)),
                         list(nrow = 0L, ncol = 5L, logical = structure(list(col = character(0),
                                                                             class = character(0), n_missing = integer(0), p_complete = numeric(0),
                                                                             n_true = integer(0), n_false = integer(0), p_true = numeric(0),
                                                                             head = character(0), tail = character(0)), class = c("tbl_df",
                                                                                                                                  "tbl", "data.frame"), row.names = integer(0)), numeric = structure(list(
                                                                                                                                    col = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"
                                                                                                                                    ), class = c("numeric", "numeric", "numeric", "numeric"),
                                                                                                                                    n_missing = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_
                                                                                                                                    ), p_complete = c(NA_real_, NA_real_, NA_real_, NA_real_),
                                                                                                                                    n_unique = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_
                                                                                                                                    ), mean = c(NA_real_, NA_real_, NA_real_, NA_real_), p0 = c(NA_real_,
                                                                                                                                                                                                NA_real_, NA_real_, NA_real_), p25 = c(NA_real_, NA_real_,
                                                                                                                                                                                                                                       NA_real_, NA_real_), p50 = c(NA_real_, NA_real_, NA_real_,
                                                                                                                                                                                                                                                                    NA_real_), p75 = c(NA_real_, NA_real_, NA_real_, NA_real_
                                                                                                                                                                                                                                                                    ), p100 = c(NA_real_, NA_real_, NA_real_, NA_real_), iqr = c(NA_real_,
                                                                                                                                                                                                                                                                                                                                 NA_real_, NA_real_, NA_real_), sd = c(NA_real_, NA_real_,
                                                                                                                                                                                                                                                                                                                                                                       NA_real_, NA_real_), head = c(NA_character_, NA_character_,
                                                                                                                                                                                                                                                                                                                                                                                                     NA_character_, NA_character_), tail = c(NA_character_, NA_character_,
                                                                                                                                                                                                                                                                                                                                                                                                                                             NA_character_, NA_character_)), class = c("tbl_df", "tbl",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "data.frame"), row.names = c(NA, -4L)), date = structure(list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         col = character(0), class = character(0), n_missing = integer(0),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         p_complete = numeric(0), n_unique = integer(0), min = structure(numeric(0), class = "Date"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         max = structure(numeric(0), class = "Date"), head = character(0),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         tail = character(0)), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ), row.names = integer(0)), datetime = structure(list(col = character(0),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               class = character(0), n_missing = integer(0), p_complete = numeric(0),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               n_unique = integer(0), min = structure(numeric(0), class = c("POSIXct",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "POSIXt"), tzone = "UTC"), max = structure(numeric(0), class = c("POSIXct",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "POSIXt"), tzone = "UTC"), head = character(0), tail = character(0)), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "tbl", "data.frame"), row.names = integer(0)), categorical = structure(list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               col = "Species", class = "factor", n_missing = NA_integer_,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               p_complete = NA_real_, n_unique = NA_integer_, min = NA_character_,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               max = NA_character_, head = NA_character_, tail = NA_character_), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           "tbl", "data.frame"), row.names = c(NA, -1L))))

  testthat::expect_equal(fskim(dplyr::select(iris)),
                         list(nrow = 150L, ncol = 0L, logical = structure(list(col = character(0),
                                                                               class = character(0), n_missing = integer(0), p_complete = numeric(0),
                                                                               n_true = integer(0), n_false = integer(0), p_true = numeric(0),
                                                                               head = character(0), tail = character(0)), class = c("tbl_df",
                                                                                                                                    "tbl", "data.frame"), row.names = integer(0)), numeric = structure(list(
                                                                                                                                      col = character(0), class = character(0), n_missing = integer(0),
                                                                                                                                      p_complete = numeric(0), n_unique = integer(0), mean = numeric(0),
                                                                                                                                      p0 = numeric(0), p25 = numeric(0), p50 = numeric(0), p75 = numeric(0),
                                                                                                                                      p100 = numeric(0), iqr = numeric(0), sd = numeric(0), head = character(0),
                                                                                                                                      tail = character(0)), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                      ), row.names = integer(0)), date = structure(list(col = character(0),
                                                                                                                                                                                        class = character(0), n_missing = integer(0), p_complete = numeric(0),
                                                                                                                                                                                        n_unique = integer(0), min = structure(numeric(0), class = "Date"),
                                                                                                                                                                                        max = structure(numeric(0), class = "Date"), head = character(0),
                                                                                                                                                                                        tail = character(0)), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                                                                        ), row.names = integer(0)), datetime = structure(list(col = character(0),
                                                                                                                                                                                                                                              class = character(0), n_missing = integer(0), p_complete = numeric(0),
                                                                                                                                                                                                                                              n_unique = integer(0), min = structure(numeric(0), class = c("POSIXct",
                                                                                                                                                                                                                                                                                                           "POSIXt"), tzone = "UTC"), max = structure(numeric(0), class = c("POSIXct",
                                                                                                                                                                                                                                                                                                                                                                            "POSIXt"), tzone = "UTC"), head = character(0), tail = character(0)), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                                                                                                                                                                            "tbl", "data.frame"), row.names = integer(0)), categorical = structure(list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                              col = character(0), class = character(0), n_missing = integer(0),
                                                                                                                                                                                                                                                                                                                                                                                                                                                              p_complete = numeric(0), n_unique = integer(0), min = character(0),
                                                                                                                                                                                                                                                                                                                                                                                                                                                              max = character(0), head = character(0), tail = character(0)), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "tbl", "data.frame"), row.names = integer(0))))
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
